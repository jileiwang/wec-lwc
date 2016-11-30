-module(deadwindow).
-export([start/0]).

getBadPeriod() -> 3.
getTraceFile() -> "D:/finalresultwebsearch1.csv".
getOrderFile() -> "D:/allrank_web1.csv".
getWriteFile() -> "D:/result_dw_web1.csv".

getLineLimit() -> 60000000.


start() ->
	
	{ok, S1} = file:open(getTraceFile(), read),
	{Dict,PerNum} = readlines(S1,dict:new(),0,0),
	{ok, S2} = file:open(getOrderFile(), read),
	{ok, S3} = file:open(getWriteFile(), write),	
	writeResult(Dict,S2,S3,PerNum).

% Dict: Key = CacheId, Value = {PresentBadPeriod,LastPeriod,[BadPNum]}
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
                 				case Num =< getBadPeriod() of
                 					true ->
                 						D = dict:store(Cacheid,{1,PerNum,[PerNum-1]},Dict);
                 					_ ->
                 						D = dict:store(Cacheid,{0,PerNum,[PerNum-1]},Dict)
                 				end;
                 				
                 			{ok,{Present,Lp,L}} ->
                 				case Num =< getBadPeriod() of
                 					true ->
                                        D = dict:store(Cacheid,{Present+PerNum-Lp,PerNum,L},Dict);          						
                 					_->                                       
                                        D = dict:store(Cacheid,{0,PerNum,[Present+PerNum-Lp-1|L]},Dict) 
                 				end
                 			   
                 		end,
                 readlines(S,D,Length+1,PerNum)                 
        		end
      	end
  	end.  

writeResult(Dict,Sr,Sw,PerNum) ->
	
	
      	Line = io:get_line(Sr, ''),
      	case Line of 
        	eof -> 
             	file:close(Sr),
             	file:close(Sw);
	    		
	    	"\n" ->
	    		file:close(Sr),
                file:close(Sw);
        	_ ->
             	Res = string:tokens(Line, ", \t\r\n"),
             	 
               	[Cacheid_str,_] = Res,  
                {Cacheid, _} = string:to_integer(Cacheid_str),
                % io:format("~p~n",[Cacheid]),                 		
                {ok,{Present,Lp,List}} = dict:find(Cacheid,Dict),
                % io:format("~p,",[Cacheid]),
                io:format(Sw,"~p,",[Cacheid]),
                List1 = [Present+PerNum-Lp|List],
                L = lists:filter(fun(X) ->
            if
                X>0 ->
                    true;
                true ->
                    false
            end   end, List1),
                S = writeLine(Sw,L,PerNum),
                writeResult(Dict,Sr,S,PerNum)
        		
      	
  	end.  

writeLine(S,[],_) ->
    io:format(S,"0,0,0~n",[]),
    S;
writeLine(S,L,PerNum) ->
    
        io:format(S,"~p,~p,",[lists:max(L),lists:sum(L)]),
        lists:foreach(fun(X) -> io:format(S,"~p,",[X]) end, lists:reverse(L)),
        io:format(S,"~n",[]),
        S.

