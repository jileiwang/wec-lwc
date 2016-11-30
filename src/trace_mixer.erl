-module(trace_mixer).
-export([start/0, start/1]).

% merge multiple traces into one mixed trace

getFilePath() -> "../trace/".

getWriteFile() -> getFilePath() ++ "mix-trace.log".


  
start() ->
  Traces = [metanodej, fbn, fbnw, fbrfa, cctv1],
  %Traces = [build1, as2],
  start(Traces).  
  
start(Traces) ->
  % ReqLists format: [{finance, [...]}, {websearch1, [...]}, ...]
  ReqLists = loopLoadTraces(Traces, []),
  %io:format("ReqList = ~p~n", [ReqLists]),
  MergedLists = merge(ReqLists, []),
  writeMergedList(MergedLists).

merge([], MergedLists) -> MergedLists;
merge(ReqLists, MergedLists) ->
  {One, Others} = getNthReqs(ReqLists, random:uniform(length(ReqLists))),
  {Trace, Reqs} = One, 
  {Some, LeftReqs} = getSomeReqs(Trace, Reqs),
  io:format("Trace ~p left Reqs num = ~p~n", [Trace, length(LeftReqs)]),
  case length(LeftReqs) of
    0 ->      
      merge(Others, Some ++ MergedLists);
    _ ->
      merge([{Trace, LeftReqs}|Others], Some ++ MergedLists)
  end.

  
getNthReqs(ReqLists, N) -> getNthReqs(ReqLists, N-1, []).
  
getNthReqs([], _N, _ReqLists) -> error;
getNthReqs([H|T], 0, ReqLists) -> {H, lists:reverse(ReqLists) ++ T};
getNthReqs([H|T], N, ReqLists) -> getNthReqs(T, N-1, [H|ReqLists]).
  
     
getSomeReqs(Trace, Reqs) -> 
  Max = getMax(Trace),
  N = random:uniform(Max),
  getSomeReqs(Reqs, N, []).
  
getSomeReqs([], _N, LeaveReqs) -> {lists:reverse(LeaveReqs), []};
getSomeReqs(Reqs, 0, LeaveReqs) -> {lists:reverse(LeaveReqs), Reqs};  
getSomeReqs([H|T], N, LeaveReqs) -> getSomeReqs(T, N-1, [H|LeaveReqs]).    

getMax(Trace) ->
  case Trace of
    finance -> 300;
    websearch1 -> 1000;
    build1 -> 200;
    as2 -> 30;
    cctv1 ->100;
    metanodej -> 100;
    fbn -> 20;
    fbnw -> 20;
    fbrfa -> 40
  end.  

writeMergedList(MergedList) ->
  {ok, S} = file:open(getWriteFile(), write), 
  writeMergedList(MergedList, S).

writeMergedList([], _S) -> ok;
writeMergedList([H|T], S) ->
  {Type, DiskID, CachelineID} = H,
  io:format(S, "~p ~p ~p~n", [Type, DiskID, CachelineID]),
  writeMergedList(T, S).


%% -------------------------
%%     Load Trace Module
%% -------------------------

loopLoadTraces([], ReqLists) -> lists:reverse(ReqLists);
loopLoadTraces([H|T], ReqLists) -> 
  io:format("~nWill load trace ~p...~n", [H]),
  Reqs = loadTraces(H, trace_getTracePars:getTraceFile(H)),
  io:format(" req num = ~p~n", [length(Reqs)]),
  loopLoadTraces(T, [{H, Reqs}|ReqLists]).

loadTraces(Trace, TraceFile) -> 
  {ok, S} = file:open(TraceFile, read),
  readlines(S, [], Trace).

readlines(S, Reqs, TraceType) ->
  Line = io:get_line(S, ''),
  case Line of 
    eof -> 
         file:close(S),
         lists:reverse(Reqs);  
    _ ->
         Res = string:tokens(Line, " ,\t\n{}"),
         case Res=:=[] of 
           true -> readlines(S, Reqs, TraceType);
           _ ->  
             % line format: <Type(0-read, 1-write) DiskID(no use now) CachelineID>
             [Type_str, DiskID_str, CachelineID_str|_] = Res,
             {Type, _} = string:to_integer(Type_str),
             {DiskID, _} = string:to_integer(DiskID_str),
             {CachelineID, _} = string:to_integer(CachelineID_str),
             CachelineID1 = modifyID(TraceType, CachelineID),     
             readlines(S, [{Type, DiskID, CachelineID1}|Reqs], TraceType)
         end
  end.  

modifyID(TraceType, CachelineID) ->
  case TraceType of
    % finance -> CachelineID;
    % websearch1 -> 10000000 + CachelineID;
    % build1 -> 20000000 + CachelineID;
    % cctv1 -> 30000000 + CachelineID;
    % as2 -> CachelineID;   

    % _ -> error
    cctv1 -> 200000000 + CachelineID;
    fbn -> CachelineID;
    fbnw -> 300000000 + CachelineID;
    fbrfa -> 400000000 + CachelineID;
    metanodej -> 500000000 + CachelineID
  end.
