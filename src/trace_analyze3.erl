-module(trace_analyze3).
-export([start/0, start/1, interval/2, freq/1]).

-record(session, {cacheline_id=-1, session_id=-1, status=open, access_num=0, last_access_gid=-1, irgs=[], irrs=[]}).
-record(statline, {cacheline_id=-1, session_id=-1, access_num=0, mean_irg, max_irg, min_irg, sum_irg, mean_irr, max_irr, min_irr, sum_irr, irgs=[], irrs=[], kuadu=0}).
-record(section, {perc=-1.0, session_num=-1, total_access_num=0, mean_access_num=0, all_irrs=[], mean_kuadu=0}).

getTraceFile() -> "D:/Traces/mix-trace.req".
getStatFile() -> "D:/freq-mix-trace.csv".
getStatFile2(P) -> "D:/interval-mix-trace-top" ++ float_to_list(P) ++ ".csv".

start(P) ->
  PercList = [[P]],
  work(PercList). 

start() ->
  PercList = [[0.01], [0.02], [0.05], [0.1], [0.2]],
  work(PercList).
  
work([]) -> ok;
work([H|T]) ->
  io:format("perc list = ~p~n", [H]),
  interval(100000000000000, H),
  work(T).


interval(Ait, PercList) ->
  Cachelines = loadTraces(getTraceFile()),
  io:format("Cachelines num = ~p~n", [length(Cachelines)]),
 
  Sessions = genSessions(Cachelines, Ait),
  io:format("sessions num = ~p~n", [length(Sessions)]),

  StatList = session2statline(Sessions),
  SortedSL = lists:sort(fun statline_cmp/2, StatList),
  io:format("sorted stat list length = ~p~n", [length(SortedSL)]),
  
  %PercList = [0.001],
  [Perc] = PercList,
  io:format("PerList = ~p, Perc = ~p~n", [PercList, Perc]),
  %Lens = lists:map(fun(X) -> round(length(SortedSL)*X) end, PercList),
  %Lens = lists:map(fun(X) -> round(307825*X) end, PercList),
  Lens = [round(Perc*307825)],
  io:format("will split to ~p sublists, Lens sum = ~p~n", [length(Lens), sum(Lens)]),
  io:format("Lens = ~p~n", [Lens]),
  StatlinesList = splitSL(SortedSL, Lens, []),
  SectionList = change2section(StatlinesList, [], 0), % 分成两个section，第一个是我们要的
  
  [TopSection|_] = SectionList,
  Irrs = TopSection#section.all_irrs,
  SortedIrrs = lists:sort(Irrs),
  %Max = lists:last(SortedIrrs),
  Max = 100000,
  io:format("Max = ~p~n", [Max]),
  %io:format("SortedIrrs = ~p~n", [SortedIrrs]),
  %Ites = genList(0.0001, lists:seq(1,9)) ++ genList(0.001, lists:seq(1,9)) ++ genList(0.01, lists:seq(1,9)) ++ genList(0.1, lists:seq(1,9)) ++ [1.0],
  Ites = genList(0.001, lists:seq(1,1000)),
  StatIrrs = statIrrs(SortedIrrs, Max, [], Ites, 0),
  [P] = PercList,
  io:format("will write intervals~n"),   
  writeIntervals(getStatFile2(P), StatIrrs).
  
statIrrs(_SortedIrrs, _Max, Result, [], _TmpCount) -> lists:reverse(Result);
statIrrs([], Max, Result, [Hite|_Tite], TmpCount) -> lists:reverse([{Hite, Max*Hite, TmpCount}|Result]);
statIrrs([H|T], Max, Result, [Hite|Tite], TmpCount) ->
  case H=<Max*Hite of
    true ->
      statIrrs(T, Max, Result, [Hite|Tite], TmpCount+1);
    _ -> 
      statIrrs([H|T], Max, [{Hite, Max*Hite, TmpCount}|Result], Tite, 0)
  end.


freq(Ait) -> 
  Cachelines = loadTraces(getTraceFile()),
  io:format("Cachelines num = ~p~n", [length(Cachelines)]),
 
  Sessions = genSessions(Cachelines, Ait),
  io:format("sessions num = ~p~n", [length(Sessions)]),
  
  StatList = session2statline(Sessions),
  SortedSL = lists:sort(fun statline_cmp/2, StatList),
  io:format("sorted stat list length = ~p~n", [length(SortedSL)]),
  
  L = [1, 1, 1, 1, 1, 1, 1, 1, 1],
  PercList = genList(0.001, L) ++ genList(0.01, L) ++ genList(0.1, L),
  Lens = lists:map(fun(X) -> round(length(SortedSL)*X) end, PercList),
  io:format("will split to ~p sublists, Lens sum = ~p~n", [length(Lens), sum(Lens)]),
  io:format("Lens = ~p~n", [Lens]),
  StatlinesList = splitSL(SortedSL, Lens, []),
  SectionList = change2section(StatlinesList, [], 0),  
  writeStat(getStatFile(), SectionList).


sum([]) -> 0;
sum([H|T]) -> H+sum(T).

splitSL(SortedSL, [], Lists) -> lists:reverse([SortedSL|Lists]);
splitSL(SortedSL, [H|T], Lists) ->
  case H >= length(SortedSL) of
    true ->
      lists:reverse([SortedSL|Lists]);
    false ->
      L1 = lists:sublist(SortedSL, H),
      L2 = lists:sublist(SortedSL, H+1, length(SortedSL)),
      splitSL(L2, T, [L1|Lists])    
  end.  

change2section([], SectionList, _ID) -> lists:reverse(SectionList);
change2section([H|T], SectionList, ID) ->
  {SessionNum, TotalAccessNum, AllIrrs, SumKuadu} = calcStatelines(H, 0, 0, [], 0),
  MeanKuadu = SumKuadu/SessionNum,
  Section = #section{perc=ID, session_num=SessionNum, total_access_num=TotalAccessNum, mean_access_num=TotalAccessNum/SessionNum, all_irrs=AllIrrs, mean_kuadu=MeanKuadu},
  change2section(T, [Section|SectionList], ID+1).


% return {SessionNum, TotalAccessNum, AllIrrs}
calcStatelines([], SessionNum, TotalAccessNum, AllIrrs, SumKuadu) -> {SessionNum, TotalAccessNum, AllIrrs, SumKuadu}; 
calcStatelines([H|T], SessionNum, TotalAccessNum, AllIrrs, SumKuadu) -> 
  calcStatelines(T, 1+SessionNum, H#statline.access_num + TotalAccessNum, H#statline.irrs ++ AllIrrs, SumKuadu + H#statline.kuadu).


genList(C, L) -> lists:map(fun(X) -> C*X end, L).  

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
%-record(statline, {cacheline_id=-1, session_id=-1, access_num=0, last_access_gid=-1, mean_irg, max_irg, min_irg, sum_irg, mean_irr, max_irr, min_irr, sum_irr, irgs=[], irrs=[], kuadu=0}).
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
			sum_irr=SumIrr,
      kuadu=H#session.access_num+sum(H#session.irrs)},
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


statline_cmp(Statline1, Statline2) -> 
  Statline1#statline.access_num >= Statline2#statline.access_num.


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
  
%-record(section, {perc=-1.0, session_num=-1, total_access_num=0, mean_access_num=0, all_irrs=[], mean_kuadu=0})
writeStat(StatFile, SectionList) ->
	{ok, S} = file:open(StatFile, write),
	lists:foreach(fun(Section) ->
      MeanIrr = calcMeanIrr(Section#section.all_irrs, 0, 0),
			io:format(S, "~p, ~p, ~p, ~p, ~p, ~p~n", [
				Section#section.perc,
				Section#section.mean_access_num,
				Section#section.session_num,
				Section#section.total_access_num,
				MeanIrr,
        Section#section.mean_kuadu])
		end, SectionList),
	file:close(S).

writeIntervals(StatFile, StatIrrs) ->
	{ok, S} = file:open(StatFile, write),
	lists:foreach(fun(Item) ->
      {Perc, Irr, Num} = Item,
			io:format(S, "~p, ~p, ~p~n", [Perc, Irr, Num])
		end, StatIrrs),
	file:close(S).  

print_list(_S, []) -> ok;
print_list(S, [H|T]) ->
  io:format(S, ", ~p", [H]),
  print_list(S, T).

calcMeanIrr([], Num, Sum) ->
  case Num of
    0 -> 0;
    _ -> Sum/Num
  end;
calcMeanIrr([H|T], Num, Sum) -> calcMeanIrr(T, Num+1, Sum+H).
