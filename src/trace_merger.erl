-module(trace_merger).
-export([start/0, mergeReqs/3]).

% for scale traces, merge multiple traces into one. note: sort reqs according to arrival time

getTraceFile(N) -> "D:/scale-small-" ++ integer_to_list(N) ++ ".trace".
getWriteFile(N) -> "D:/scale-small-1-" ++ integer_to_list(N) ++ ".trace".

start() -> work(1, 10, []).

work(N, Total, ExistReqs) when N > Total -> ExistReqs;
work(N, Total, ExistReqs) ->
  NewReqs = loadTraces(getTraceFile(N), (N-1)*10000),
  Reqs = mergeReqs(ExistReqs, NewReqs, []),
  writeReqs(getWriteFile(N), Reqs),
  work(N+1, Total, Reqs).

mergeReqs([], NewReqs, Result) -> lists:reverse(Result) ++ NewReqs;
mergeReqs(ExistReqs, [], Result) -> lists:reverse(Result) ++ ExistReqs;
mergeReqs([H1|T1], [H2|T2], Result) ->
  {Time1,_,_,_,_} = H1,
  {Time2,_,_,_,_} = H2,
  case Time1 > Time2 of
    true -> mergeReqs([H1|T1], T2, [H2|Result]);
    _ -> mergeReqs(T1, [H2|T2], [H1|Result])
  end.
  

loadTraces(TraceFile, Prefix) -> 
  {ok, S} = file:open(TraceFile, read),
  readlines(S, [], Prefix).
  
% for scale traces, Prefix is set different values for different trace;
% for general cctv case, Prefix is set 0.
readlines(S, Reqs, Prefix) ->
  Line = io:get_line(S, ''),
  case Line of 
    eof -> 
         file:close(S),
         lists:reverse(Reqs);  
    _ ->
         Res = string:tokens(Line, ", \t\r\n"),
         case Res=:=[] of 
           true -> readlines(S, Reqs, Prefix);
           _ ->  
             % 4.026150432936176	69	100	0	3
             % <timestamp(second)>, <fileid>, <duration>, <flag1>, <flag2>
             [Time_str, Fileid_str, Duration_str|_] = Res,  
             {Time, _} = string:to_integer(Time_str),
             {Fileid, _} = string:to_integer(Fileid_str),
             {Duration, _} = string:to_integer(Duration_str),
             readlines(S, [{Time, Prefix+Fileid, Duration, 0, 3}|Reqs], Prefix)
         end
  end.  

writeReqs(File, Reqs) ->
	{ok, S} = file:open(File, write), 
  %∏Ò Ω£∫
  % <timestamp(second)>, <fileid>, <duration>, <flag1>, <flag2> 
	lists:foreach(fun(Req) ->
          %io:format("Req = ~p~n", [Req]),
	        {Time, Urlid, Duration, F1, F2} = Req,
	        %io:format("~p ~p ~p ~p ~p~n", [Time, Urlid, Duration, F1, F2]),
					io:format(S, "~p ~p ~p ~p ~p~n", [Time, Urlid, Duration, F1, F2])
			         end, Reqs),
	file:close(S).
