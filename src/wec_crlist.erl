-module (wec_crlist).
-export ([start/1,getLongHotRate/0, getSplitLhbRate/0]).


getFilePath2() -> "../log/".




 getLogFile(Trace) ->
  
        getFilePath2() ++ "list-" ++ atom_to_list(Trace) ++ ".csv"
   .


getLogFile2(Trace) ->
    getFilePath2() ++ "acc-" ++ atom_to_list(Trace) ++ ".csv".


getSplitLhbRate() ->
    Traces = [mix],
    % [0.01, 0.05, 0.1]
    Rates = [0.01, 0.04, 0.05],
    PeriodLength = 10000,
    SplitPeriodNum = [10, 50],
    {ok, S} = file:open("../log/SplitlhdAccRate.csv", write),
    loopSplitTraces(S, Traces, Rates, PeriodLength, SplitPeriodNum),
    file:close(S).

loopSplitTraces(_, [], _R, _P, _S) -> ok;
loopSplitTraces(Logfile, [H|T], Rates, PeriodLength, SplitPeriodNum) ->
    loopSplitPeriodNum(Logfile, H, Rates, PeriodLength, SplitPeriodNum),
    loopSplitTraces(Logfile, T, Rates, PeriodLength, SplitPeriodNum).

loopSplitPeriodNum(_, _, _R, _P, []) ->
    ok;
loopSplitPeriodNum(Logfile, Trace, Rates, PeriodLength, [H|T]) ->
   getSplitLhbRate(Logfile, Trace, Rates, PeriodLength*H),
   loopSplitPeriodNum(Logfile, Trace, Rates, PeriodLength, T).


getSplitLhbRate(Logfile, Trace, Rates, SplitNum) ->
    io:format(Logfile, "Trace = ~p, ~p~n", [Trace, trace_getTracePars:getTraceFile(Trace)]),
    io:format(Logfile, "SplitNum = ~p~n",[SplitNum]),
    io:format("Trace = ~p, ~p~n", [Trace, trace_getTracePars: getTraceFile(Trace)]),
    io:format("SplitNum = ~p~n",[SplitNum]),
    {ok, TraceFile} = file:open(trace_getTracePars: getTraceFile(Trace), read),
    RatesList = readlines(TraceFile, Rates, SplitNum, lists:duplicate(length(Rates),[]), 0, []),
    AveRate = loopRatesPrint1(Logfile, Rates, RatesList, []),
    loopRatesPrint2(Logfile, Rates, AveRate).

loopRatesPrint1(_Logfile, [], [], L) ->lists:reverse(L) ;
loopRatesPrint1(Logfile, [H|T], [Hr|Tr], L) ->
    io:format(Logfile, "~p,~p~n", [H,Hr]),
    loopRatesPrint1(Logfile, T, Tr, [lists:sum(Hr)/length(Hr)|L]).

loopRatesPrint2(_, [], []) -> ok;
loopRatesPrint2(Logfile, [H|T], [H2|T2])->
    io:format(Logfile, "~p,~p~n", [H,H2]),
    loopRatesPrint2(Logfile, T, T2).


readlines(Tfile, Rates, SplitNum, RatesList, ReadNum, Plist) ->
    
    case (ReadNum + 1) rem SplitNum of
        0 ->
            io:format("~p~n", [ReadNum]),
            L = lists:reverse(lists:keysort(2, Plist)),
            % io:format("~p~n~p~n", [L, length(Plist)]),
            Numlist = lists:map(fun(A) -> round(A*length(Plist)) end, Rates),
            % io:format("~p~n", [Numlist]),
            R1 = calRate(Numlist, L, RatesList, [],  0, SplitNum),
            % io:format("~p~n",[R1]),
            % io:format("~p~n",[0/0]),
            readlines(Tfile, Rates, SplitNum, R1, 0, []);
        _->
            
            
            Line = io:get_line(Tfile, ''),
            case Line of 
                eof ->                 
                    file:close(Tfile),
                    RatesList;         
            _ ->             
                Res = string:tokens(Line, " ,\t\n{}"),
                [Type_str, _, Cache_str|_] = Res,
                {Cache, _} = string:to_integer(Cache_str),
                {Type,_} = string:to_integer(Type_str),
                if
                    Type =:= 1 ->                    
                    readlines(Tfile, Rates, SplitNum, RatesList, ReadNum, Plist);
                true ->
                    case lists:keysearch(Cache, 1, Plist) of
                        {value, {_, Acc}} ->
                            L1 = lists:keyreplace(Cache, 1, Plist, {Cache, Acc+1}),
                            readlines(Tfile, Rates, SplitNum, RatesList, ReadNum+1, L1);
                        _->
                            readlines(Tfile, Rates, SplitNum, RatesList, ReadNum+1, [{Cache, 1}|Plist])
                    end
            end
    end
end.

    calRate([], _L, _Olist,Nlist, _, _) ->
        lists:reverse(Nlist);
    calRate([H|T], L, [Hl|Tl], Nlist, Orate, SplitNum) ->
        % io:format("~p,~p,~p,~p,~p~n", [H, L, Hl, Nlist, Orate]),
        % io:format("~p,~p,~p~n",[Hl, Tl, [Hl|Tl]]),
        Toplist = lists:sublist(L, 1, H),
        % io:format("~p~n",[Toplist]),
        Acclist = lists:map(fun(A) -> {_, Accn} = A, Accn end, Toplist),
        R = lists:sum(Acclist)/SplitNum + Orate,
        Nl = lists:sublist(L, H+1, length(L) - H),
        % io:format("~p~n",[Nl]),
        % io:format("~n~n~n",[]),
        calRate(T, Nl, Tl, [[R|Hl]|Nlist], R, SplitNum).



getLongHotRate() ->
    Traces = [websearch1, cctv1, fbn, metanodej, fbnw],    
    % [0.01, 0.05, 0.1]
    Rates = [0.01, 0.04, 0.05],
    {ok, S} = file:open("../log/lhdAccRate.csv", write),
    loopTraces(S, Traces, Rates).

loopTraces(S, [], _Rates) ->
    file:close(S);
loopTraces(S, [H|T], Rates) ->
    io:format(S, "Trace = ~p~n", [H]),
    {ok, File} = file:open(getLogFile2(H), read),

    
    loopRates(H, File, S, Rates, 0, 0),
    file:close(File),
    io:format(S, "****************************~n~n", []),
    loopTraces(S, T, Rates).

loopRates(_, _, _, [], _, _) ->
    ok;
loopRates(Trace, Fread, Fwrite, [H|Tr], Brate, Arate) ->
    ReadLine = round(H * trace_getTracePars: getUCLN(Trace)),
    io:format("test: ReadLine = ~p~n",[ReadLine]),
    Acc = readAccFile(Fread, ReadLine, 0),
    Nbrate = H + Brate,
    Narate = Acc / trace_getTracePars: getTraceLen(Trace) + Arate,
    io:format(Fwrite, "~p,~p~n",[Nbrate, Narate]),
    loopRates(Trace, Fread, Fwrite, Tr, Nbrate, Narate).

readAccFile(_, 0, Acc) ->
    Acc;
readAccFile(Fread, Num, Acc) ->
    Line = io:get_line(Fread, ''),
    
            Res = string:tokens(Line, " ,\t\n"),
            [_, Acc_str|_] = Res,            
            {OneAcc,_} = string:to_integer(Acc_str),
            readAccFile(Fread, Num-1, Acc+OneAcc).

start(Trace) ->
	
	put("write",0),
	{ok, S} = file:open(trace_getTracePars:getTraceFile(Trace), read),
	L = readlines(S, dict:new()),
	L1 = lists:reverse(lists:keysort(2,L)),
    {ok, S2} = file:open(getLogFile2(Trace), write),
    lists:foreach(fun
        (X)->
            {C, A} = X, io:format(S2, "~p,~p~n", [C, A])
    end, L1),
    file:close(S2),
	{ok, S1} = file:open(getLogFile(Trace), write),
	[{Cache,Acc}|T] = L1,
	put("length",length(L1)),
	io:format(S1, "~p,",[{Cache,1}]),
	writeList(S1, T, 1, Acc, 1),
	io:format("~p~n",[get("write")]),
	file:close(S1).

writeList(_, [], _, _, _) -> ok;
writeList(S, L, Order, OAcc, 10) ->
	io:format(S, "~n", []),
	writeList(S, L, Order, OAcc, 0);
writeList(S, [{Cache,Acc}|T], Order, OAcc, Num) ->
	if
		Acc < OAcc ->
			io:format(S, "~p,", [{Cache,Order+1}]),
			writeList(S, T, get("length")-length(T), Acc, Num+1);
		true ->
		 	io:format(S, "~p,", [{Cache,Order}]),
		 	writeList(S, T, Order, OAcc, Num+1)
	end.




readlines(S, Dict) ->
	Line = io:get_line(S, ''),
    case Line of 
        eof ->                 
            file:close(S),
            dict:to_list(Dict);         
        _ ->             
            Res = string:tokens(Line, " ,\t\n{}"),
            [Type_str, _, Cache_str|_] = Res,
            {Cache, _} = string:to_integer(Cache_str),
            {Type,_} = string:to_integer(Type_str),
            if
            	Type =:= 1 ->
            		put("write", get("write")+1),
            		readlines(S, Dict);
            	true ->
            		case dict:find(Cache,Dict) of
            			{ok,Acc} ->
            				Dict1 = dict:store(Cache,Acc+1,Dict),
            				readlines(S, Dict1);
            			error ->
            				Dict1 = dict:store(Cache,1,Dict),
            				readlines(S, Dict1)
            		end
            end
    end.
