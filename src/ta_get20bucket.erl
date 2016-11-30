-module (ta_get20bucket).
-export ([start/0, start/1]).

getPeriodLength() -> 10000.



getFilePath() -> "../log/".

getList() -> ["5%", "10%", "15%", "20%", "25%", "30%", "35%", "40%", "45%", "50%", 
                                              "55%", "60%", "65%", "70%", "75%", "80%", "85%", "90%", "95%", "100%"].
getNameForBuckets(Num) -> lists:nth(Num, getList()).

 getLogFile(Trace) ->
  {getFilePath() ++ "accSplit_" ++ atom_to_list(Trace) ++ ".csv",
  getFilePath() ++ "buckets_" ++ atom_to_list(Trace) ++ ".csv"} .


start() ->
    Traces = [ metanodea, metanodes],
    loopTraces(Traces).

loopTraces([]) -> ok;
loopTraces([H|T]) ->
    start(H),
    loopTraces(T).

start(Trace) ->
	erase(),
	put("period", 1),    % short : In the end, ignore the last period if it's not a complete period
	put("trace", Trace),	
    put("line", 0),
    put("read_req", 0),
    put("ucln", 0), 
    put("averagesum",{0,0}), %short, store the sum of average(req/ucln) of each period
    put("average", 0), % only change once
    initFor20Bucket(), % short
	{ok, S} = file:open(trace_getTracePars:getTraceFile(Trace), read),
	Dict = readlines(S, dict:new(), 0),
	% P = get("period"),
    
	file:close(S),
    {LogOfAcc, LogOfBucket} = getLogFile(Trace),

    writeSplitAcc(LogOfAcc, Dict).
    % funOf20buckets(LogOfBucket, Dict).
    

funOf20buckets(LogOfBucket, Dict) -> 
    put("period", round(get("read_req")/getPeriodLength())),        
    {ok, S2} = file:open(LogOfBucket, write),
    {AverageSum, _} = get("averagesum"),
    Average = 0.8 * AverageSum / get("period"),
    put("average",Average),
    lists:foreach(fun(X) -> calculate(X) end, dict:to_list(Dict)),
    io:format(S2, "Average,~p~n", [Average]),
    output20Buckets(S2),
    file:close(S2).

writeSplitAcc(LogOfAcc, Dict) ->
    {ok, S1} = file:open(LogOfAcc, write),

    % io:format("Dict = ~p~n",[Dict]),
    % io:format("List = ~p~n",[getList()]),
    % get the List of [{Cache, Acc}] ordered by acc
    List = lists:reverse(lists:keysort(2, lists:map(fun
        (X) ->
            {Cache, {_, _, Acc, _}} = X,
            {Cache, Acc}
    end, dict:to_list(Dict)))),
    % io:format("get List~n"),
    % io:format("~p~n", [List]),
    % write the acc of each period of each block
    lists:foreach(fun(X) -> {Cache,_} = X,  
    case  dict:find(Cache,Dict) of
       {ok, {L,_,_,_}} ->
           io:format(S1, "~p,",[Cache]),
           % io:format("~p", [Cache]),
           lists:foreach(fun(B)->
                 io:format(S1, "~p,",[B])
           end,lists:reverse(L)),

    case length(L) < get("period") of
        true ->
            lists:foreach(fun(B)->
            io:format(S1, "~p,",[B])
    end,lists:duplicate(get("period")-length(L),0));
        _->

            ok
    end,
        io:format(S1, "~n",[]);
    _-> 
    % io:format("Warning: not found the block~n"),
    ok
end
     end, List),
    file:close(S1).

initFor20Bucket() ->
    initFor20Bucket(1).


initFor20Bucket(21) -> ok;
initFor20Bucket(Num) -> 
    put(getNameForBuckets(Num), {0, 0}),
    initFor20Bucket(Num + 1).

calculate(X) ->
 % io:format("calculate~n"),
 {_, {L, _, Acc,StartPeriod}} = X,
 % io:format("~p~n",[L]),
 case StartPeriod > get("period") of
     true ->
         ok;
    _->

 
 Num = min(get("period"),getNumOfGoodPeriod(L)),
 % io:format("Period = ~p,", [get("period")]),
 % io:format("Num = ~p, StartPeriod = ~p~n",[Num, StartPeriod]),
 P = min(1, Num / (get("period") - StartPeriod + 1)),
 % io:format("~p~n",[P]),
 logBucket(P, Acc)
end.

 getNumOfGoodPeriod(List) ->
    getNumOfGoodPeriod(List, 0).
getNumOfGoodPeriod([], Num) -> Num;
getNumOfGoodPeriod([H|T], Num) ->
    case H >= get("average") of
        true ->
            getNumOfGoodPeriod(T, Num + 1);
        _->
            getNumOfGoodPeriod(T, Num)
    end.


logBucket(P, Acc) ->
    logBucket(P, Acc, 1).
logBucket(P, Acc, Num) ->
    case P =<  0.05 * Num + 0.001 of
        true->
            % io:format("get: ~p, ~p~n",[P, Num]),
            Name = getNameForBuckets(Num),
            {Acc1, Sum1} = get(Name),
            put(Name, {Acc1 + Acc, Sum1 + 1});
        _->
            % io:format("wrong: ~p~n",[Num + 1]),
            logBucket(P, Acc, Num + 1)
    end.

output20Buckets(S) ->
    loop20Buckets(S, 1).

loop20Buckets(_, 21) -> ok;
loop20Buckets(S, Num) ->
    {Acc, Sum} = get(getNameForBuckets(Num)),    
    io:format(S, "~p,~p,~p~n", [getNameForBuckets(Num),Sum, Sum/trace_getTracePars:getUCLN(get("trace"))]),        
    loop20Buckets(S, Num + 1).

periodDeal() ->
    % io:format("~p~n",[get("ucln")]),
    case get("ucln") > 0 of
        true ->
            Average = getPeriodLength() / get("ucln"),
            {AverageSum, PastAverage} = get("averagesum"),
    put("averagesum",  {AverageSum + PastAverage, Average}),
    put("ucln", 0);
    _-> ok
    end
    .
	
readlines(S, Dict, Num) ->
	
	case Num =:= getPeriodLength() * get("period") of
		true ->
            Ptem = get("period"),
			put("period", Ptem + 1),
            io:format("Readreq = ~p~n",[get("read_req")]),
            io:format("Line = ~p~n",[get("line")]),
            io:format("Period = ~p~n",[get("period")]),
            periodDeal();

		_->
			ok
	end,
	Line = io:get_line(S, ''),
    put("line", get("line") + 1),
	Period = get("period"),
    case Line of 
        eof ->                 
            file:close(S),
            Dict;         
        _ ->             
            Res = string:tokens(Line, " ,\t\n{}"),
            [Type_str, _, Cache_str|_] = Res,
            {Cache, _} = string:to_integer(Cache_str),
            {Type,_} = string:to_integer(Type_str),
            if
            	Type =:= 1 ->            		
            		readlines(S, Dict, Num);
            	true ->
                    put("read_req", get("read_req") + 1),            		
                            % io:format("~p~n", [Cache]),
            				case dict:find(Cache,Dict) of
            					{ok,{L,P,Sum,StartPeriod}}->
            						case P < Period of
            							true->
                                            put("ucln", get("ucln") + 1),
            								D1 = dict:store(Cache,{[1] ++ lists:duplicate(max(Period-length(L)-1,0), 0) ++ L,Period, Sum + 1, StartPeriod},Dict);
            							_->
                                            [H|T] = L,
            							    D1 = dict:store(Cache,{[H+1|T],Period, Sum + 1, StartPeriod},Dict)

            						end;
            					error->
                                    put("ucln", get("ucln") + 1),
            					    D1 = dict:store(Cache,{[1|lists:duplicate(Period-1,0)],Period,1, Period},Dict)
            				end,
            			
            		
            	% io:format("~p~n", [D1]),
            	readlines(S, D1, Num+1)
            end
            
    end.

