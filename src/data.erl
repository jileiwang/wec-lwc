-module(data).
-export([start/0]).

getTraceFile() -> "../log/macro-new/metanodej-0120.log".
getLogFile() -> "../log/macro-new/metanodej-0120.csv".

start() ->
	
	{ok, S1} = file:open(getTraceFile(), read),
	{ok, S2} = file:open(getLogFile(), write),
	Type = fr,
    case Type of
        fr ->
            outputFr(S2),
            downloadFr(S1, S2);
        lgb ->
            put("sign", 0),
            outputlgb(S2),
            downloadlgb(S1,S2)

    end.

outputFr(S) ->
    io:format(S, "Trace,Alg,Ssd,Sieve Mode,Index,Hit,Write,Nohit,Meanhit", []).

downloadFr(Sr, Sw) ->

      	Line = io:get_line(Sr, ''),
        % io:format("Line = ~p~n",[Line]),
      	case Line of 
        	eof -> 
             	file:close(Sr),
                file:close(Sw);             	
        	_ ->
             	case string:str(Line, "Index") of
                    0 ->
                        case string:str(Line, "Ssd Hit Rate") of
                            0 ->
                                case string:str(Line, "Trace=") of
                                    0 ->
                                        case string:str(Line, "SsdAddIn") of
                                            0 ->
                                                case string:str(Line, "ssd_evict_noaccessblock_num") of
                                                    0 ->
                                                        case string:str(Line, "mean hits") of
                                                            0 ->
                                                                case string:str(Line, "Sieve Mode") of
                                                                    0 ->
                                                                        ok;
                                                                    _->
                                                                        Res = string:tokens(Line, " =(),"),
                                
                                                                        [_, _, Sm|_] = Res,
                                                            
                                                                        io:format(Sw, "~s,", [Sm])
                                                                end;
                                                            _->
                                                                Res = string:tokens(Line, " ="),
                                
                                                            [_, _, Meanhitstr|_] = Res,
                                                            {Meanhit, _} = string:to_float(Meanhitstr),
                                                            io:format(Sw, "~p", [Meanhit])
                                                        end;
                                                    _ ->
                                                        Res = string:tokens(Line, " =,"),
                                
                                                        [_,  Nohitstr|_] = Res,
                                                        {Nohit, _} = string:to_integer(Nohitstr),
                                                        io:format(Sw, "~p,", [Nohit])
                                                    end;
                                            _->
                                                Res = string:tokens(Line, " =,"),
                                
                                                [_,  Wstr|_] = Res,
                                                {W, _} = string:to_integer(Wstr),
                                                io:format(Sw, "~p,", [W])
                                        end;
                                       
                                    _ ->
                                        Res = string:tokens(Line, " =,"),
                                        
                                        [_, Trace, _, Alg, _, _, _, _, _, Ssdstr|_] = Res,
                                        
                                        io:format(Sw, "~n~s,~s,~s,", [Trace, Alg, Ssdstr])
                                end;
                            _ ->
                                Res = string:tokens(Line, " =,"),
                                
                                [_, _, _, Hrstr|_] = Res,
                                {Hr, _} = string:to_float(Hrstr),
                                io:format(Sw, "~p%,", [Hr])


                        end;
                    _ ->
                        Res = string:tokens(Line, " ="),
                        [_, Indexstr|_] = Res,    
                        {Index , _} = string:to_float(Indexstr),                   
                        io:format(Sw, "~p,", [Index])

                end,
                downloadFr(Sr, Sw)
  	     end.
        

outputlgb(S) -> 
    io:format(S, "Trace,Alg,Ssd,Throttling,Index,Hit,Write,Nohit,Meanhit~n", []).
downloadlgb(Sr, Sw) -> 
    
    Line = io:get_line(Sr, ''),
        % io:format("Line = ~p~n",[Line]),
    case Line of 
        eof -> 
            file:close(Sr),
            file:close(Sw);                 
        _ ->
            case string:str(Line, "Index") of
                    0 ->
                        case string:str(Line, "Ssd Hit Rate") of
                            0 ->
                                case string:str(Line, "Trace=") of
                                    0 ->
                                        case string:str(Line, "SsdAddIn") of
                                            0 ->
                                                case string:str(Line, "SSD:") of
                                                    0 ->
                                                        case string:str(Line, "Mean Hits") of
                                                            0 ->
                                                                case string:str(Line, "throttling") of
                                                                    0 ->
                                                                        ok;
                                                                    _->
                                                                        Res = string:tokens(Line, " =\n"),
                                
                                                                        [_, T|_] = Res,
                                                                        
                                                                        % io:format("Throttling = ~s~n", [T]),
                                                                        io:format(Sw, "~s,~p,", [T, get("index")])
                                                                end;
                                                            _->
                                                                Res = string:tokens(Line, " =\n,"),
                                
                                                            [_, _, Meanhit, _, _, Nohit, _Nohitp|_] = Res,
                                                            put("meanhit", Meanhit),
                                                            put("Nohit", Nohit)
                                                            % io:format(Sw, "~s,~s,~s,", [Meanhit,Nohit,Nohitp])
                                                        end;
                                                    _ ->
                                                        Res = string:tokens(Line, " =,\n"),
                                
                                                        [_,  _, Ssd, _, Alg|_] = Res,
                                                        Sign = get("sign"),

                                                        if
                                                            Sign =:= 1 ->
                                                                ok;
                                                            true ->
                                                                io:format(Sw, "~s,",[get("trace")])
                                                        end,
                                                        io:format(Sw, "~s,~s,", [ Alg, Ssd])
                                                    end;
                                            _->
                                                Res = string:tokens(Line, " =,"),
                                
                                                [_,  Wstr|_] = Res,
                                                {W, _} = string:to_integer(Wstr),
                                                io:format(Sw, "~p,~s,~s~n", [W, get("Nohit"), get("meanhit")]),
                                                put("sign", 0)
                                        end;
                                       
                                    _ ->
                                        Res = string:tokens(Line, " =,"),
                                        
                                        [_, Trace|_] = Res,
                                        put("trace", Trace),
                                        put("sign", 1),
                                        
                                        io:format(Sw, "~s,", [Trace])
                                end;
                            _ ->
                                Res = string:tokens(Line, " =,"),
                                
                                [_, _, _, Hrstr|_] = Res,
                                {Hr, _} = string:to_float(Hrstr),
                                io:format(Sw, "~p%,", [Hr])


                        end;
                    _ ->
                        Res = string:tokens(Line, " ="),
                        [_, Indexstr|_] = Res,    
                        {Index , _} = string:to_float(Indexstr),      
                        put("index", Index)             
                        % io:format(Sw, "~p,", [Index])

                end,
                downloadlgb(Sr, Sw)
    end.