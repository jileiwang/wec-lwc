-module(data_analyze).
-export([start/0]).

getGoodPer() -> 10.
getWinSize() -> 4.
getTraceFile() -> "D:/finalresultas2.csv".
getOrderFile() -> "D:/allrank_as2.csv".
getWriteFile() -> "D:/result_stable_as2.csv".
getWriteFile2() -> "D:/result_ratio_as2.csv".
getLineLimit() -> 60000000.
getRatioList()-> [0.01,0.03,0.05,0.1,0.2,0.3,0.5,0.99,1].

start() ->
	
	{ok, S1} = file:open(getTraceFile(), read),
	{Dict,PerNum} = readlines(S1,dict:new(),0,0),
	{ok, S2} = file:open(getOrderFile(), read),
	{ok, S3} = file:open(getWriteFile(), write),
	{ok, S4} = file:open(getWriteFile2(), write),
	writeResult(Dict,S2,S3,S4,PerNum,0,0,getRatioList()).

% Dict: Key = CacheId, Value = {PresentContinuePeriod, ContinuePeriod, GoodPeriod}
readlines(S,Dict,Length,PerNum) ->
	if 
    Length rem 10000 == 0 ->
	     io:format("Reqs num = ~p, line limit = ~p~n", [Length, getLineLimit()]);
	  true -> ok
	end,
	case Length >= getLineLimit() of 
	  true -> 
	    file:close(S),
	    {Dict,PerNum};     	 
	  false -> 
      	Line = io:get_line(S, ''),
      	case Line of 
        	eof -> 
             	file:close(S),
             	{Dict,PerNum};
        	_ ->
             	Res = string:tokens(Line, ", \t\r\n"),
             	case Res of 
               		[_,_,_] -> readlines(S,Dict,Length,PerNum+1);
               		[Cacheid_str,Num_str] ->  
                 		{Cacheid, _} = string:to_integer(Cacheid_str),
                 		{Num,_} = string:to_integer(Num_str),
                 		case dict:find(Cacheid,Dict) of
                 			error ->
                 				case Num > getGoodPer() of
                 					true ->
                 						D = dict:store(Cacheid,{1,0,1},Dict);
                 					_ ->
                 						D = dict:store(Cacheid,{0,0,0},Dict)
                 				end;
                 				
                 			{ok,{Present,Con,GoodPeriod}} ->
                 				case Num > getGoodPer() of
                 					true ->
                 						case Present+1 >= getWinSize() of
                 							true ->
                 								D = dict:store(Cacheid,{Present+1,Con+1,GoodPeriod+1},Dict);
                 							_ ->
                 								D = dict:store(Cacheid,{Present+1,Con,GoodPeriod+1},Dict)
                 						end;
                 					_->
                 						D = dict:store(Cacheid,{0,Con,GoodPeriod},Dict)
                 				end
                 			   
                 		end,
                 readlines(S,D,Length+1,PerNum)                 
        		end
      	end
  	end.  

writeResult(Dict,S,Sws,Swr,PerNum,Length,Sum,[H|T]) ->
	if 
    Length rem 10000 == 0 ->
	     io:format("Reqs num = ~p, line limit = ~p~n", [Length, getLineLimit()]);
	  true -> ok
	end,
	case Length >= getLineLimit() of 
	  true -> 
	    file:close(S),
	    file:close(Swr),
	    file:close(Sws);     	 
	  false -> 
      	Line = io:get_line(S, ''),
      	case Line of 
        	eof -> 
             	file:close(S),
             	file:close(Swr),
	    		file:close(Sws);
	    	"\n" ->
	    		file:close(S),
             	file:close(Swr),
	    		file:close(Sws);
        	_ ->
             	Res = string:tokens(Line, ", \t\r\n"),
             	 
               	[Cacheid_str,_] = Res,  
                {Cacheid, _} = string:to_integer(Cacheid_str),                 		
                {ok,{_,Con,GoodPeriod}} = dict:find(Cacheid,Dict),
                % io:format("~p,~p~n",[Cacheid,Con]),
                io:format(Swr,"~p,~p~n",[Cacheid,GoodPeriod/PerNum]),
                Sum1 = Sum + Con,
                case Length>=H*dict:size(Dict) of
                	true->
                        io:format(Sws,"~p,~p~n",[H,Sum1/Length]),
                        writeResult(Dict,S,Sws,Swr,PerNum,Length+1,Sum1,T);
                    _ ->
                    	writeResult(Dict,S,Sws,Swr,PerNum,Length+1,Sum1,[H|T])
                end              
        		
      	end
  	end.  