-module (gd_getLength).
-export ([start/0]).

getPeriodLength() -> 10000.


getFilePath1() -> "../trace/".
getFilePath2() -> "../log/".


getLogFile() -> getFilePath2() ++ "pictureForLength.csv".



start() ->
	
	List = [56, 5326, 134, 8088, 7711, 71, 8371, 7419],
    % List = lists:map(fun({Cache, _})  ->
    %         Cache
    % end,ta_getRanklist:getRanklist(websearch1)),
    put("period", 20),
    put("list", List),
    put("trace", websearch1),
	{ok, S} = file:open(trace_getTracePars:getTraceFile(get("trace")), read),
    Di = storageList(List, dict:new()),
	Dict = readlines(S, Di, 0),
	io:format("read finish~n"),
	
	{ok, S1} = file:open(getLogFile(), write),
    % io:format("Dict = ~p~n",[Dict]),
    % io:format("List = ~p~n",[getList()]),
    lists:foreach(fun(X) ->
            {ok, L} = dict:find(X, Dict),
            lists:foreach(fun(Y) ->
                    io:format(S1, "~p,", [Y])
            end, lists:reverse(L)),
            io:format(S1, "~n", [])
    end, List),
    file:close(S1).

storageList([], Dict) -> Dict;
storageList([H|T], Dict) ->
    D1 = dict:store(H, [], Dict),
    put(H, []),
    storageList(T, D1).

	
readlines(S, Dict, Num) ->
	
	case Num >= get("period") * getPeriodLength() of
		true ->
            file:close(S),
            Dict;

		_->
			
	       Line = io:get_line(S, ''),    
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
                        % io:format("~p~n", [Cache]),

            			     case dict:find(Cache,Dict) of
            				    {ok,List}->
            				        Percent = getPercent(Cache),
                                    addList(Cache),
                                    put(Cache, []),
                                    D1 = dict:store(Cache, [Percent|List], Dict);	
            				    error->
                                    addList(Cache),
            					   D1 = Dict
            			     end,

            			
            		
            	% io:format("~p~n", [D1]),
            	           readlines(S, D1, Num+1)
                    end
            end
    end.

getPercent(Cache) ->
    N = length(get(Cache)),
    N/trace_getTracePars:getUCLN(get("trace")).

addList(Cache) ->
    List = get("list"),
    lists:foreach(fun(X)->
        Xlist = get(X),
        case lists:member(Cache, Xlist) of
            true ->
                ok;
            false ->
                put(X, [Cache|Xlist])
        end  end, List).





