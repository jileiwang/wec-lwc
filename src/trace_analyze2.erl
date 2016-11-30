-module(trace_analyze2).
-export([start/1]).

-record(session, {cacheline_id=-1, session_id=-1, status=open, access_num=0, last_access_gid=-1, irgs=[], irrs=[]}).
-record(statline, {cacheline_id=-1, session_id=-1, access_num=0, mean_irg, max_irg, min_irg, sum_irg, mean_irr, max_irr, min_irr, sum_irr, irgs=[], irrs=[]}).

getTraceFile() -> "D:/test.csv".        % around one day
%getTraceFile() -> "D:/FormalTrace/as2-4K.req".
getStatFile() -> "D:/Trace_Analyze/as2-4K.csv".
%getStatFile() -> "D:/test.csv". 

start(Ait) -> 
  Cachelines = loadTraces(getTraceFile()),
  io:format("Cachelines num = ~p~n", [length(Cachelines)]),
 
  Sessions = genSessions(Cachelines, Ait),
  io:format("sessions num = ~p~n", [length(Sessions)]),
  
  StatList = session2statline(Sessions),
  SortedSL = lists:sort(fun statline_cmp/2, StatList),
  io:format("sorted stat list length = ~p~n", [length(SortedSL)]),
  writeStat(getStatFile(), SortedSL).
  

genSessions(Cachelines, Ait) -> genSessions(Cachelines, Ait, [], 0, []). 

% AssistL is used for calculating IRRs
genSessions([], _Ait, L, _, _) -> L;
genSessions([H|T], Ait, L, N, AssistL) -> 
	if 
		N rem 10000 =:= 9999 -> io:format("genSessions(), N=~p~n", [N]);
		true -> ok
	end, 
	{Type, _DiskID, CachelineID} = H,
	case Type of 
		1 ->	% write 
			L1 = closeSession(CachelineID, L),
			genSessions(T, Ait, L1, N, AssistL);
		0 ->  % read
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
         Res = string:tokens(Line, " \t\n"),
         case Res=:=[] of 
           true -> readlines(S, Reqs);
           _ ->  
             % line format: <Type(0-read, 1-write) DiskID(no use now) CachelineID>
             [Type_str, DiskID_str, CachelineID_str|_] = Res,
             {Type, _} = string:to_integer(Type_str),
             {DiskID, _} = string:to_integer(DiskID_str),
             {CachelineID, _} = string:to_integer(CachelineID_str),
             readlines(S, [{Type, DiskID, CachelineID}|Reqs])
         end
  end.  

loadTraces(TraceFile) -> 
  {ok, S} = file:open(TraceFile, read),
  readlines(S, []).
  

writeStat(StatFile, SortedSL) ->
	{ok, S} = file:open(StatFile, write),
	lists:foreach(fun(Statline) ->
			if 
				length(Statline#statline.irrs) > 0 ->
					io:format(S, "~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p", [
						Statline#statline.cacheline_id,
						Statline#statline.session_id,
						Statline#statline.access_num,
						Statline#statline.mean_irg,
						Statline#statline.max_irg,
						Statline#statline.min_irg,
						Statline#statline.sum_irg,
						Statline#statline.mean_irr,
						Statline#statline.max_irr,
						Statline#statline.min_irr,
						Statline#statline.sum_irr]),
          print_list(S, Statline#statline.irrs),
          io:format(S, "~n", []);
				true -> ok
			end 
		end, SortedSL),
	file:close(S).

print_list(_S, []) -> ok;
print_list(S, [H|T]) ->
  io:format(S, ", ~p", [H]),
  print_list(S, T).
