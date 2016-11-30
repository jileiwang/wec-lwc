-module (mc_test).
-export ([start/0]).

-record (dictElem, {acc = {1, 0, 0, 0, 0, 0, 0, 0, 0, 0}, p = 1, location = hdd, last_acc_period = 0, waitq = 0, last_gcon = 0,
	last_acc_time = 0}).

getWaitQ(Num) ->
case Num of
	0 ->
		[];
	1 -> 
	    [11, 1,2,3,4,5,6,7,8,9,10];
	2->
	    [{1,10}, {2,9}, {3,5}];
	3-> 
	    [{1,1}, {2,2}, {3,3}];
	4 ->
		[[1,2], [3,4], [5,6]];
	5 ->
		[[{1,10}, {2,9}, {3,5}], [{4,10}, {5,9}, {6,5}], [{7,10}, {8,9}, {9,5}]]
end.

generateDcitElem(AccQueue) ->
	
     #dictElem{acc = AccQueue}.

getDict(Num) ->
case Num of
	1 ->
		dict:new();
	2->
	    Dict = dict:new(),
	    X = generateDcitElem(list_to_tuple([11|lists:duplicate(9,0)])),
	    dict:store(1, X, Dict)

end.

test_register() ->
	% init process varibles
     put("enableMC", false),
     % put("enableMC", false),
     put("period_num", 1),
     put("waitq_alg",fifo),
    put("goodcoe", 0.1),
    put("acccoe", 3),
    put("cgp", 3),
    put("gpn", 0.4),
    put("average", 15),
    % request([[{1,5}]])
    % request([[{1,1},{2,1},{3,1},{1,1},{4,1}]])
   request([[{1,5},{2,15}],[{1,5},{2,2}], [{1,5},{3,45}], [{1,30}], [], [], [], [], [], [], [{1,10},{3,1}]])
   .

request(List) ->
	N = 11,
	put("sum", N),
	loop_period(N, List, dict:new(), [], 1).

loop_period(N, List, Dict, WaitQ, SysTime) ->
	io:format("~p：~p~n~p~n",[get("sum")-N, dict:to_list(Dict), WaitQ]),
	
	case N>0 of
		true ->
		    Order = get("sum")-N+1,
			put("period_num", Order),
			io:format("~p     ~p     ",[wec_lgb:getAccThrehold(), wec_lgb:getGoodPeriod()]),
			Reqlist_Per_Period = lists:nth(Order, List),
			{Dict1, WaitQ1, SysTime1} = loop_request(Reqlist_Per_Period, Dict, WaitQ, SysTime),
			loop_period(N-1, List, Dict1, WaitQ1, SysTime1);
		false ->
			{Dict, WaitQ, SysTime}

	end.

loop_request([], Dict, WaitQ, SysTime) ->
    {Dict, WaitQ, SysTime};
loop_request([{CacheID, Num}|T], Dict, WaitQ, SysTime) ->
	{Dict1, WaitQ1, SysTime1} = register(CacheID, Num, Dict, WaitQ, SysTime),
	loop_request(T, Dict1, WaitQ1, SysTime1).

 register(CacheID, Num, Dict, WaitQ, SysTime) ->
 	% io:format("~p : ~p~n~p~n",[20-Num, dict:to_list(Dict), WaitQ]),
    case Num>0 of
    	true ->
    		{Dict1, WaitQ1} = wec_lgb:register(CacheID, SysTime, Dict, WaitQ),
    		register(CacheID, Num-1, Dict1, WaitQ1, SysTime+1);
    	false ->
    		{Dict, WaitQ, SysTime}
    end.
    

    








test_updateWaitQ() ->
	WaitQ = mc_algorithm:updateWaitQ(1, getWaitQ(0), fifo, getDict(2)),
	io:format("~p~n", [WaitQ]).

test([[H|T]|T1]) ->
    length(T) + 1 + length(lists:append(T1));
test([H|T]) ->
    length(T)+1.


test_getNhead() ->
    
    Enter = mc_algorithm:getNHeadWaitQ(3, getWaitQ(5), true),
    io:format("~p~n",[Enter]),
    Enter1 = mc_algorithm:getNHeadWaitQ(3, getWaitQ(1), false),
    io:format("~p~n", [Enter1]).
start() ->	
	SSDQ_enter = wec_lgb:concat(2, [{1,3}], [{2,1}]),
	io:format("~p~n",[SSDQ_enter]).

read(S,0) -> ok;
read(S, Num) ->
	io:format("~p~n",[io:get_line(S, '')]),
	read(S, Num-1).
	% 