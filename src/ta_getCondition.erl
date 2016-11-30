-module (ta_getCondition).
-export ([start/0, start/1]).

getSplitFile(Trace) -> "../log/" ++ "accSplit_" ++ atom_to_list(Trace) ++ ".csv".
getPercentList() -> [0.01, 0.04, 0.05, 0.1, 0.3, 0.5].

getGoodPeriod(Trace) -> 
	0.2 * trace_getTracePars:getAverage(Trace).

getLogFile(Trace) ->
	"../log/" ++ "con_" ++ atom_to_list(Trace) ++ ".csv".

start() ->	
    Traces = [metanodes, websearch1, metanodea, finance],
    loopTraces(Traces).

loopTraces([]) -> ok;
loopTraces([H|T]) ->
    start(H),
    loopTraces(T).

start(Trace) ->
	erase(),
	put("maxPeriod", 20),
	put("trace", Trace),
	Dict = readlines(Trace),
	List = lists:reverse(lists:keysort(2, lists:map(fun
        (X) ->
            {Cache, { Acc, GCnum}} = X,
            {Cache, Acc, GCnum}
    end, dict:to_list(Dict)))),

    % result is a list of {numof3, numof2, numof1, numof0}
    LofPercent = calPercent(List),
    outputResult(LofPercent).

calPercent(List) ->
	calPercent(List, getPercentList(), []).
calPercent(_, [], L) -> lists:reverse(L);
calPercent(AccList, [Percent|T], ResultL) ->
	Length =  round(Percent * length(AccList) + 0.49999),
	NeedList = lists:sublist(AccList, Length),
	NewList = lists:sublist(AccList, Length + 1, length(AccList) - Length),
	% io:format("NeedList = ~p~n", [NeedList]),
	Elem = calElem(NeedList),
	calPercent(NewList, T, [Elem|ResultL]).

calElem(L) -> calElem(L, {0, 0, 0, 0}).
calElem([], E) -> E;
calElem([H|T], E) ->
	{_, _, Num} = H,
	{N3, N2, N1, N0} = E,
	case Num of
		0 ->
			calElem(T, {N3, N2, N1, N0 + 1});
		1 ->
			calElem(T, {N3, N2, N1 + 1, N0});
		2 ->
			calElem(T, {N3, N2 + 1, N1, N0});
		3 ->
			calElem(T, {N3 + 1, N2, N1, N0})
	end.

outputResult(LofPercent) ->
	{ok, S} = file:open(getLogFile(get("trace")), write),
	lists:foreach(fun(X) -> {N3, N2, N1, N0} = X,
		Sum = N3 + N2 + N1 + N0,
		io:format(S, "~p%,~p%,~p%,~p%~n", [100 * (N3/Sum), 100 * (N2/Sum), 100 * (N1/Sum), 100 * (N0/Sum)]) end, LofPercent).



readlines(Trace) ->
	{ok, S} = file:open(getSplitFile(Trace), read),
	readlines(S, dict:new()).

readlines(S, Dict) ->
	Line = io:get_line(S, ''),
	case Line of 
        eof ->                 
            file:close(S),
            Dict;   
        _->
        Res = string:tokens(Line, " ,\t\n{}"),
        [Cache_str|AccList_str] = Res,      
        {Cache, _} = string:to_integer(Cache_str),
        Dict1 = dealWith(Cache, Dict, AccList_str),
        readlines(S, Dict1)
    end.

dealWith(Cache, Dict, AccList_str) ->
	AccList = getFormalList(AccList_str),
	Condition = calConditon(AccList),
	if
		Condition =:= ok ->
			Dict;
		true->

	
	Sum = lists:sum(AccList),
	Dict1 = dict:store(Cache,{Sum, Condition}, Dict),
	Dict1
end.


getFormalList(L) -> getFormalList(L, []).
getFormalList([], L) -> lists:reverse(L);
getFormalList([H|T], L) -> 
	case length(L) =:= 20 of
		true ->
			lists:reverse(L);
		_->
			{Acc, _} = string:to_integer(H),
			getFormalList(T,[Acc|L])
	end.

calConditon(List) ->
	% NewList delete the former 0s.
	{StartPeriod, NewList} = calStartPeriod(List),
	if
		StartPeriod =:= 0 ->
			ok;
		true ->
	
			case length(NewList) < 10 of
				true ->
					calConditon(lists:duplicate(10 - length(NewList), 0) ++ NewList, 0); 
				_->
					calConditon(NewList, 0)
			end
	end.
 
 calStartPeriod(List) -> calStartPeriod(List, 1).
 calStartPeriod([], _) -> {0,[]};
 calStartPeriod([H|T], Num) ->
 	if
 		H =:= 0 ->
 			calStartPeriod(T, Num + 1);
 		true ->
 			{Num, [H|T]} 
 	end.

calConditon(List, Num) ->
	case length(List) < 10 of
		true ->
			Num;
		_->
		    [_H|T] = List,
		    SubList = lists:sublist(List, 10),
		    Condition = calGoodCondition(SubList),
		    if
		    	Condition > Num ->
		    		calConditon(T, Condition);
		    	true ->
		    	    calConditon(T, Num)
		    end
	end.

calGoodCondition(List) ->
	checkAcc(List) + checkCgp(List) + checkGpn(List).

checkAcc(L) ->
	Sum = lists:sum(L),
	case Sum < 3 * trace_getTracePars:getAverage(get("trace")) of
		true ->
			0;
		_-> 1
	end.

checkCgp(L) ->
	case lists:last(L) < getGoodPeriod(get("trace")) of
		true ->
			0;
		_->
		checkCgp(lists:sublist(L, length(L) - 1), 1)
	end.

checkCgp(_, 3) -> 1;
checkCgp(L, Num) ->
	case lists:last(L) < getGoodPeriod(get("trace")) of
		true ->
			0;
		_->
			checkCgp(lists:sublist(L, length(L) - 1), Num + 1)
	end.

checkGpn(List) ->
	Gp = calgp(List),
	if
		Gp < 4 ->
			0;
		true->
		    1
	end.

calgp(L) -> calgp(L, 0).
calgp([], N) -> N;
calgp([H|T], Num) ->
	case H < getGoodPeriod(get("trace")) of
		true ->
			calgp(T, Num);
		_->
		    calgp(T, Num + 1)
	end.