
-module(trace_rank).
-export([start/0]).

 getTraceFile() -> "D:/finalresultas2.csv".
 getWriteFile() -> "D:/allrank_as2.csv".
 getWritePAccFile() -> "D:/pRank_Accas2.csv".
 getWritePeriodHRFile() -> "D:/pHitRatioas2.csv".
 getWritePRankFile() -> "D:/pRankas2.csv".
 getWriteARankAccFile() -> "D:/aRank_Accas2.csv".
 getWriteARankFile() -> "D:/aRankas2.csv".
getLineLimit() -> 60000000.
getRankNum() -> 20.

start() -> 
	loadtrace(getTraceFile(),getWriteFile()).


loadtrace(TraceFile, WriteFile) -> 
 	{ok, S} = file:open(TraceFile, read),
	{ok, S1} = file:open(WriteFile, write),
	Dict = readlines(S,dict:new(),0),
	List = lists:reverse(lists:keysort(2,dict:to_list(Dict))),	
	lists:foreach(fun(L) ->
          {CachelineID,Number} = L,
          io:format(S1, "~p,~p~n", [CachelineID,Number])          % write [CachelineID,Number] to WriteFile
               end, List),
	ListOfARank = getGood(List),
  % io:format("~p~n",[ListOfARank]),
	{ok, S2} = file:open(TraceFile, read),
	{ok, S3} = file:open(getWritePAccFile(),write),
	{ok, S4} = file:open(getWritePeriodHRFile(),write),
  {ok, S5} = file:open(getWritePRankFile(),write),
  {ok, S6} = file:open(getWriteARankAccFile(),write),
  {ok, S7} = file:open(getWriteARankFile(),write),
	{DictPeriod,ListPeriod} = getRankPerPeriod(S2,dict:new(),ListOfARank,0,0,0,[]),
	List2 = dict:to_list(DictPeriod),	
	lists:foreach(fun(L) ->
          {CachelineID,{State,ListOfPAcc,ListOfPRank}} = L,
          case State=:=0 of
          	false->
              Rank = getRank(CachelineID,List,1),
          		io:format(S3,"~p(~p)~n",[CachelineID,Rank]),
          		print(S3,lists:reverse(ListOfPAcc)),
          		io:format(S3,"~n",[]),
              io:format(S5,"~p(~p)~n",[CachelineID,Rank]),
              print(S5,lists:reverse(ListOfPRank)),
              io:format(S5,"~n",[]);
          	_ ->
          		ok
          end
            
               end, List2),
  lists:foreach(fun(L) -> {ok,{_,ListOfAcc,ListOfRank}} = dict:find(L,DictPeriod), 
    io:format(S6,"~p~n",[L]), io:format(S7,"~p~n",[L]), 
    print(S6,lists:reverse(ListOfAcc)), print(S7,lists:reverse(ListOfRank)), 
    io:format(S6,"~n",[]) ,io:format(S7,"~n",[]) end, 
    ListOfARank),
  file:close(S1),
	file:close(S3),
	print(S4,ListPeriod),
	file:close(S4),
  file:close(S5),
  file:close(S7),
  file:close(S6).


readlines(S,Dict,Length) ->
	if 
    Length rem 10000 == 0 ->
	     io:format("Reqs num = ~p, line limit = ~p~n", [Length, getLineLimit()]);
	  true -> ok
	end,
	case Length >= getLineLimit() of 
	  true -> 
	    file:close(S),
	    Dict;     	 
	  false -> 
      	Line = io:get_line(S, ''),
      	case Line of 
        	eof -> 
             	file:close(S),
             	Dict;
        	_ ->
             	Res = string:tokens(Line, ", \t\r\n"),
             	case Res of 
               		[_,_,_] -> readlines(S,Dict,Length);
               		[Cacheid_str,Num_str] ->  
                 		{Cacheid, _} = string:to_integer(Cacheid_str),
                 		{Num,_} = string:to_integer(Num_str),
                 		case dict:find(Cacheid,Dict) of
                 			error ->
                 				D = dict:store(Cacheid,Num,Dict);
                 			{ok,Value} ->
                 			    D = dict:store(Cacheid,Value+Num,Dict)
                 		end,
                 readlines(S,D,Length+1)                 
        		end
      	end
  	end.  


%Dict用于存放每一个cacheid的访问次数，其value值为{State,ListOfAcc,ListOfPRank},State取值为0或正数，非0值表示该块在筛选范围内，List为存放每个周期dict值的列表
  getRankPerPeriod(S,Dict,ListOfARank,Length,PeriodNum,PeriodRank,List) ->
  	if 
    Length rem 10000 == 0 ->
	     io:format("Reqs num = ~p, line limit = ~p~n", [Length, getLineLimit()]);
	  true -> ok
	end,
	case Length >= getLineLimit() of 
	  true -> 
	    file:close(S),
	    {Dict,lists:reverse(List)};     	 
	  false -> 
      	Line = io:get_line(S, ''),
      	case Line of 
        	eof -> 
             	file:close(S),
             	{Dict,lists:reverse(List)};
        	_ ->
             	Res = string:tokens(Line, ", \t\r\n"),
             	case Res of 
               		[_,_,_] -> getRankPerPeriod(S,Dict,ListOfARank,Length,PeriodNum+1,1,[0|List]);
               		[Cacheid_str,Num_str] ->                 			
                 		{Cacheid, _} = string:to_integer(Cacheid_str),
                 		{Num,_} = string:to_integer(Num_str),
                 		case PeriodRank > getRankNum() of
                 			true ->
                 				State = 0,
                 				L = List;
                 			_ ->
                 				State = 1,
                 				% io:format("~p~n",[lists:member(Cacheid,ListOfARank)]),
                 				case lists:member(Cacheid,ListOfARank) of
                 				    true ->
                 				        [H|T] = List,
                 				        L = [H+1|T];
                 				    false ->
                 				    	L = List
                 				end        				

                 		end,
                 		case dict:find(Cacheid,Dict) of
                 			error ->
                 				D = dict:store(Cacheid,{State,[Num|lists:duplicate(PeriodNum-1,0)],[PeriodRank|lists:duplicate(PeriodNum-1,-1)]},Dict);
                 			{ok,{StateOrg,ListOrg,List2Org}} ->    
                          % if  Cacheid=:=56 ->    io:format("~p,~p~n",[PeriodNum,length(ListOrg)]);
                          %   true -> ok
                          %                     end,             				
                 			    D = dict:store(Cacheid,{State+StateOrg,[Num|lists:duplicate(PeriodNum-1-length(ListOrg),0)]++ListOrg,[PeriodRank|lists:duplicate(PeriodNum-1-length(ListOrg),-1)]++List2Org},Dict)
                 		end,
                 		getRankPerPeriod(S,D,ListOfARank,Length+1,PeriodNum,PeriodRank+1,L)                 
        		end
      	end
  	end.  

  	print(S3,List) ->
  		lists:foreach(fun(L) -> io:format(S3,"~p,",[L])	end,List).

  	getGood(List) ->
      List2 = lists:map(fun(L) -> {Cacheid,_} = L, Cacheid end,List),
  		lists:reverse(lists:nthtail(length(List)-getRankNum(),lists:reverse(List2))).

    getRank(_,[],_) -> -1;
    getRank(Cacheid,[H|T],Rank) ->
      {Cache,_} = H,
      case Cacheid=:=Cache of
        true->
          Rank;
        _->
          getRank(Cacheid,T,Rank+1)
      end.