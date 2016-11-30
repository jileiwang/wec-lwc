-module(trace_analyze).
-export([start/2]).

-record(session, {cacheline_id=-1, session_id=-1, status=open, access_num=0, last_access_gid=-1, irgs=[], irrs=[]}).
-record(statline, {cacheline_id=-1, session_id=-1, access_num=0, mean_irg, max_irg, min_irg, sum_irg, mean_irr, max_irr, min_irr, sum_irr, irgs=[], irrs=[]}).

getTraceFile() -> "D:/test.csv".        % around one day
%getTraceFile() -> "D:/CAMRESWEBA03-lvm1.csv".
getStatFile() -> "D:/stat.csv".
getCachelineFile() -> "D:/cacheline.csv".

start(CachelineSize, Ait) -> 
  Reqs = loadTraces(getTraceFile()),
  io:format("req num = ~p~n", [length(Reqs)]),
  Cachelines = lists:reverse(split2cacheline(Reqs, CachelineSize, [])),
  io:format("cachelines num = ~p~n", [length(Cachelines)]),
  ClStat = statCachlines(Cachelines),
  writeCacheline(getCachelineFile(), ClStat).

  %Sessions = genSessions(Cachelines, Ait),
  %io:format("sessions num = ~p~n", [length(Sessions)]),
  %StatList = session2statline(Sessions),
  %SortedSL = lists:sort(fun statline_cmp/2, StatList),
  %io:format("sorted stat list length = ~p~n", [length(SortedSL)]),
  %writeStat(getStatFile(), SortedSL).
  

% [{read/write, diskID, Blkno, Size, RT}...] -> [{read/write, diskID, cachelineID} ...]
split2cacheline([], _CachelineSize, Cachelines) -> Cachelines;
split2cacheline([H|T], CachelineSize, Cachelines) -> 
	{Type, DiskID, Blkno, Size, _RT} = H,
	FirstCachelineID = Blkno div CachelineSize,
	LastCachelineID = (Blkno+Size-1) div CachelineSize,
	io:format("H=~p, First=~p, Last=~p~n", [H, FirstCachelineID, LastCachelineID]),
	L = genCachelines(Type, DiskID, FirstCachelineID, LastCachelineID, Cachelines),
	split2cacheline(T, CachelineSize, L).

genCachelines(Type, DiskID, Last, Last, Cachelines) -> [{Type, DiskID, Last}|Cachelines];
genCachelines(Type, DiskID, First, Last, Cachelines) -> genCachelines(Type, DiskID, First+1, Last, [{Type, DiskID, First}|Cachelines]).

statCachlines(Cachelines) -> statCachlines(Cachelines, []).

% only consider read ops
statCachlines([], ClStat) -> ClStat;
statCachlines([H|T], ClStat) -> 
  {Type, DiskID, CachelineID} = H,
  R = lists:keyfind(CachelineID, 1, ClStat),
	case R of 
		false -> statCachlines(T, [{CachelineID, 1}|ClStat]);
		_ ->
      {_, Num} = R, 
      statCachlines(T, [{CachelineID, Num+1}|lists:delete(R, ClStat)])
  end.     


genSessions(Cachelines, Ait) -> genSessions(Cachelines, Ait, [], 0, []). 

% AssistL is used for calculating IRRs
genSessions([], _Ait, L, _, _) -> L;
genSessions([H|T], Ait, L, N, AssistL) -> 
	{Type, _DiskID, CachelineID} = H,
	case Type of 
		write -> 
			L1 = closeSession(CachelineID, L),
			genSessions(T, Ait, L1, N, AssistL);
		read -> 
			{R, SessionID, L1} = searchExistingSession(CachelineID, Ait, L, N, AssistL),
			case R of 
				false ->
					NewSession = #session{cacheline_id=CachelineID, session_id=SessionID+1, access_num=1, last_access_gid=N},
					L2 = lists:reverse([NewSession|lists:reverse(L1)]),
					AssistL1 = updateAssistL(CachelineID, AssistL),
					genSessions(T, Ait, L2, N+1, AssistL1);
				_ ->
					IRG = N-1-R#session.last_access_gid,
					IRR = getIRR(CachelineID, lists:reverse(AssistL)),	
					%io:format("IRG=~p, IRR=~p, AssistL=~p~n", [IRG, IRR, AssistL]),				
					R1 = R#session{access_num=R#session.access_num+1, last_access_gid=N, 
							irgs=lists:reverse([IRG|lists:reverse(R#session.irgs)]), 
							irrs=lists:reverse([IRR|lists:reverse(R#session.irrs)])},
					L2 = lists:reverse([R1|lists:delete(R, lists:reverse(L1))]),
					AssistL1 = updateAssistL(CachelineID, AssistL),
					genSessions(T, Ait, L2, N+1, AssistL1)
			end
	end.

	
updateAssistL(CachelineID, AssistL) ->
	case lists:member(CachelineID, AssistL) of
		false -> 
			lists:reverse([CachelineID|lists:reverse(AssistL)]);
		true -> 
			lists:reverse([CachelineID|lists:reverse(lists:delete(CachelineID, AssistL))])
	end.


getIRR(CachelineID, AssistL) -> getIRR(CachelineID, AssistL, 0).

getIRR(_CachelineID, [], _Pos) -> -1;	 % should not be
getIRR(CachelineID, [H|T], Pos) ->
	case H=:=CachelineID of
		true -> Pos;
		false -> getIRR(CachelineID, T, Pos+1)
	end.


closeSession(CachelineID, SessionList) -> 
	ReverseL = lists:reverse(SessionList),
	R = lists:keyfind(CachelineID, 2, ReverseL),
	case R of 
		false -> SessionList;	% no session need to be closed
		_ -> 
			case R#session.status of 
				close -> SessionList;	% the session has been closed
				open -> 
					R1 = R#session{status = close},
					lists:reverse([R1|lists:delete(R, ReverseL)])
			end
	end.


% return the last open session with the same CachelineID
% or false if no such a session
% may modify SessionList if Gid is beyond Ait
searchExistingSession(CachelineID, Ait, SessionList, Gid, AssistL) -> 
	ReverseL = lists:reverse(SessionList),
	R = lists:keyfind(CachelineID, 2, ReverseL),
	case R of 
		false -> {false, -1, SessionList};
		_ -> 
			case R#session.status of
				close -> {false, R#session.session_id, SessionList};
				open->
					IRR = getIRR(CachelineID, lists:reverse(AssistL)),
					if 
						IRR =< Ait -> {R, R#session.session_id, SessionList};
						true -> 
							R1 = R#session{status=close},
							SessionList1 = lists:reverse([R1|lists:delete(R, ReverseL)]),
							{false, R#session.session_id, SessionList1}
					end	
			end
	end.


% stats
%-record(session, {cacheline_id=-1, session_id=-1, status=open, access_num=0, last_access_gid=-1, irgs=[], irrs=[]}).
%-record(statline, {cacheline_id=-1, session_id=-1, access_num=0, last_access_gid=-1, mean_irg, max_irg, min_irg, sum_irg, mean_irr, max_irr, min_irr, sum_irr, irgs=[], irrs=[]}).
session2statline(Sessions) -> session2statline(Sessions, []).

session2statline([], Stats) -> Stats;
session2statline([H|T], Stats) ->
	{MaxIrg, MinIrg, SumIrg} = calcIr(H#session.irgs),
	{MaxIrr, MinIrr, SumIrr} = calcIr(H#session.irrs),
	MeanIrg = SumIrg/H#session.access_num,
	MeanIrr = SumIrr/H#session.access_num,
	Statline = #statline{
			cacheline_id=H#session.cacheline_id,
			session_id=H#session.session_id,
			access_num=H#session.access_num,
			irgs=H#session.irgs,
			irrs=H#session.irrs,
			mean_irg=MeanIrg,
			max_irg=MaxIrg,
			min_irg=MinIrg,
			sum_irg=SumIrg,
			mean_irr=MeanIrr,
			max_irr=MaxIrr,
			min_irr=MinIrr,
			sum_irr=SumIrr},
	session2statline(T, [Statline|Stats]).

% return {MaxIrg, MinIrg, SumIrg} or {MaxIrr, MinIrr, SumIrr}
calcIr(L) -> calcIr(L, -1, -1, 0).

calcIr([], Max, Min, Sum) -> {Max, Min, Sum};
calcIr([H|T], Max, Min, Sum) ->
	if 
		Max<0 orelse H>Max ->
			if 
				Min<0 orelse H<Min -> calcIr(T, H, H, Sum + H);
				true -> calcIr(T, H, Min, Sum + H)
			end;
		true ->
			if 
				Min<0 orelse H<Min -> calcIr(T, Max, H, Sum + H);
				true -> calcIr(T, Max, Min, Sum + H)
			end		
	end.	


statline_cmp(Statline1, Statline2) -> Statline1#statline.access_num >= Statline2#statline.access_num.


readlines(S, Reqs) ->
  Line = io:get_line(S, ''),
  case Line of 
    eof -> 
         file:close(S),
         lists:reverse(Reqs);  
    _ ->
         Res = string:tokens(Line, ", \t\r\n"),
         case Res=:=[] of 
           true -> readlines(S, Reqs);
           _ ->  
             % 128166386724652876,web,1,Read,1422921216,7680,1935
             % <timestamp>, <server>, <diskno>, <type>, <blkno(Byte)>, <size(Byte)>, <response time>
             [_, _, DiskID_str, Type, Blkno_str, Size_str, RT_str|_] = Res,  
             {DiskID, _} = string:to_integer(DiskID_str),
             {Blkno, _} = string:to_integer(Blkno_str),
             {Size, _} = string:to_integer(Size_str),
             {RT, _} = string:to_integer(RT_str),
             case Type of 
                "Read" -> readlines(S, [{read, DiskID, Blkno, Size, RT}|Reqs]);
                "Write" -> readlines(S, [{write, DiskID, Blkno, Size, RT}|Reqs])
             end 
         end
  end.  

loadTraces(TraceFile) -> 
  {ok, S} = file:open(TraceFile, read),
  readlines(S, []).

writeStat(StatFile, SortedSL) ->
	{ok, S} = file:open(StatFile, write),
	lists:foreach(fun(Statline) ->
					io:format(S, "~p,~p,~p,~p,~p,~p,~p,~p,~p~n", [
						Statline#statline.access_num,
						Statline#statline.mean_irg,
						Statline#statline.max_irg,
						Statline#statline.min_irg,
						Statline#statline.sum_irg,
						Statline#statline.mean_irr,
						Statline#statline.max_irr,
						Statline#statline.min_irr,
						Statline#statline.sum_irr])
			end, SortedSL),
	file:close(S).

writeCacheline(CachelineFile, ClStat) ->
  SortedL = lists:sort(fun cmp/2, ClStat),  
	{ok, S} = file:open(CachelineFile, write),
	lists:foreach(fun(Cl) ->
    {ID, Num} = Cl,
    io:format(S, "~p,~p~n", [ID, Num]) end, SortedL),
  file:close(S).

cmp(A, B) ->
  {_, NumA} = A,
  {_, NumB} = B,
  NumA >= NumB.