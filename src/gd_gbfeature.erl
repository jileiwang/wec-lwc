-module (gd_gbfeature).
-export ([start/1, getGoodBlockPeriod/1]).

getPeriodLength() -> 10000.


getFilePath1() -> "../trace/".
getFilePath2() -> "../log/".


getTraceFile1() -> getFilePath1() ++ "spc-financial-150w-4K.req".
getTraceFile2() -> getFilePath1() ++ "spc-websearch1-500w-4K.req".
getTraceFile3() -> getFilePath1() ++ "spc-websearch2-470w-4K.req".
getTraceFile4() -> getFilePath1() ++ "production-build00-1-4K.req".
getTraceFile5() -> getFilePath1() ++ "production-build00-2-4K.req".
getTraceFile6() -> getFilePath1() ++ "as1-4K.req".
getTraceFile7() -> getFilePath1() ++ "as2-4K.req".
getTraceFile8() -> getFilePath1() ++ "cctv1-5h-40K.req". 
getTraceFile9() -> getFilePath1() ++ "cctv2-4h-40K.req".
getTraceFile10() -> getFilePath1() ++ "mix-trace.req". 

getTraceFile(Trace) ->
  case Trace of
    finance -> getTraceFile1();
    websearch1 -> getTraceFile2();
    websearch2 -> getTraceFile3();
    build1 -> getTraceFile4();
    build2 -> getTraceFile5();
    as1 -> getTraceFile6();
    as2 -> getTraceFile7();
    cctv1 -> getTraceFile8();
    cctv2 -> getTraceFile9();
    mix -> getTraceFile10();
    test -> getFilePath1() ++ "test.req";
    fbrd -> getFilePath1() ++ "filebench-randomread.req";
    fbrfa -> getFilePath1() ++ "filebench-randomfileaccess.req";
    generate -> getFilePath1() ++ "cctv-generated.req";
    fbnw -> getFilePath1() ++ "filebench-networkfs.req";
    mds1 -> getFilePath1() ++ "cambridge-mds1-155w-4K.req";
    msnfs -> getFilePath1() ++ "production-MSN-FS-4k.req";
      lm -> getFilePath1() ++ "production-LiveMap-Backend-4K.req";
    datanode3 -> getFilePath1() ++ "datanode3-hive-select.req";    
    cam1 -> getFilePath1() ++ "CAM-01-SRV-lvm0.req";
    builds -> getFilePath1() ++ "24.hour.BuildServer.11-28-2007.07-39-PM.trace.req";
    datanode3a -> getFilePath1() ++ "datanode3-hive-aggregation.req";
    cam2 -> getFilePath1() ++ "CAM-02-SRV-lvm0.req";
    fbfs -> getFilePath1() ++ "filebench-fileserver.req";
    fbfsr -> getFilePath1() ++ "filebench-fivestreamread.req";
    fbn -> getFilePath1() ++ "filebench-netsfs.req";
    metanodea -> getFilePath1() ++ "metanode-hive-aggregation.req";
    metanodej -> getFilePath1() ++ "metanode-hive-join.req";
    metanodes -> getFilePath1() ++ "metanode-hive-select.req"

  end.


 getLogFile(Trace) ->
  case Trace of
    
    websearch1 -> getFilePath2() ++ "web1_ana.csv";
    _-> getFilePath2() ++ atom_to_list(Trace) ++ "_ana.csv"
  end.

getList() ->
	ta_getRanklist:getRanklist(get("trace")).




start(Trace) ->
	
	put("period", 0),
	put("trace", Trace),	
    put("line", 0),
    put("read_req", 0),
	{ok, S} = file:open(getTraceFile(Trace), read),
	Dict = readlines(S, dict:new(), 0),
	P = get("period"),
	file:close(S),
	{ok, S1} = file:open(getLogFile(Trace), write),
    % io:format("Dict = ~p~n",[Dict]),
    % io:format("List = ~p~n",[getList()]),
	lists:foreach(fun(X) -> {Cache,_} = X,	
    case  dict:find(Cache,Dict) of
	   {ok, {L,_}} ->
	       io:format(S1, "~p,",[Cache]),
	       % io:format("~p", [Cache]),
	       lists:foreach(fun(B)->
			     io:format(S1, "~p,",[B])
	       end,lists:reverse(L)),
	case length(L) < P of
		true ->
			lists:foreach(fun(B)->
			io:format(S1, "~p,",[B])
	end,lists:duplicate(P-length(L),0));
		_->
			ok
	end,
        io:format(S1, "~n",[]);
    _-> ok
end
	 end, getList()),
    file:close(S1).

	
readlines(S, Dict, Num) ->
	
	case Num rem getPeriodLength() of
		0 ->
            Ptem = get("period"),
			put("period", Ptem + 1),
            io:format("Readreq = ~p~n",[get("read_req")]),
            io:format("Line = ~p~n",[get("line")]),
            io:format("Period = ~p~n",[get("period")]);

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
            					{ok,{L,P}}->
            						case P < Period of
            							true->
            								D1 = dict:store(Cache,{[1] ++ lists:duplicate(max(Period-length(L)-1,0), 0) ++ L,Period},Dict);
            							_->
                                            [H|T] = L,
            							    D1 = dict:store(Cache,{[H+1|T],Period},Dict)

            						end;
            					error->
            					    D1 = dict:store(Cache,{[1|lists:duplicate(Period-1,0)],Period},Dict)
            				end,
            			
            		
            	% io:format("~p~n", [D1]),
            	readlines(S, D1, Num+1)
            end
            
    end.

getGoodBlockPeriod(Trace) ->
    {ok, Fread} = file:open(getLogFile(Trace), read),
    GoodThrottling = 10,
    L = loopBlocks(Fread, GoodThrottling, []),
    file:close(Fread),
    ListOrderByPercent = lists:reverse(lists:keysort(2, L)),
    {ok, LogFile} = file:open(getFilePath2() ++ "percent_" ++ atom_to_list(Trace) ++ ".csv", write),
    lists:foreach(fun(X) -> {Cache, Percent} = X,
            io:format(LogFile, "~p,~p~n", [Cache, Percent]) end, ListOrderByPercent),
    file:close(LogFile).



loopBlocks(Fread, GoodThrottling, L) ->
    Line = io:get_line(Fread, ''),
    case Line of 
        eof ->                 
            L;         
        _ ->             
            Res = string:tokens(Line, " ,\t\n"),
            [Cache_str|AccList] = Res,
            {Cache, _} = string:to_integer(Cache_str),
            Percent = loopPeriods(0, 0, 0, AccList, GoodThrottling),
            % io:format("~p",[0/0]),
            loopBlocks(Fread, GoodThrottling, [{Cache, Percent}|L])
    end.


loopPeriods(GoodPeriod, AllPeriod, _, [], _) ->
    GoodPeriod/AllPeriod;
loopPeriods(GoodPeriod, AllPeriod, LiveSign, [H_str|L], GoodThrottling) ->
    
    {H, _} = string:to_integer(H_str),
    % io:format("gp = ~p, AllPeriod = ~p, LiveSign = ~p, H = ~p~n",[GoodPeriod, AllPeriod, LiveSign, H]),
    if
        LiveSign =:= 0 ->
            if
                H=:=0 ->
                    loopPeriods(GoodPeriod, AllPeriod, LiveSign, L, GoodThrottling);
                true ->
                    if 
                        H >= GoodThrottling ->
                            loopPeriods(GoodPeriod + 1, AllPeriod + 1, 1, L, GoodThrottling);
                        true ->
                            loopPeriods(GoodPeriod, AllPeriod + 1, 1, L, GoodThrottling)
                        
                    end

            end;
        true ->
            if 
                        H >= GoodThrottling ->
                            loopPeriods(GoodPeriod + 1, AllPeriod + 1, 1, L, GoodThrottling);
                        true ->
                            loopPeriods(GoodPeriod, AllPeriod + 1, 1, L, GoodThrottling)
                        
            end
    end.
