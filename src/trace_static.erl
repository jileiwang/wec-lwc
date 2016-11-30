-module(trace_static).
-export([start/0]).
% 将已经按照cacheline大小切好的结果按照周期计算访问次数和排名
% 注意，由于resultfile的打开模式为append，每次运行前必须清空写入结果的文件。
getTraceFile() -> "C:/Program Files/erl6.2/trace/mix-trace.req".
getResultFile()-> "C:/Program Files/erl6.2/log/mix-trace-final-result.csv".

getLineLimit() -> 5100000.
getSplitNum() -> 10000.

start() ->
    TraceType = finance,
	  loadTraces(getTraceFile(),TraceType).
	  % io:format("~p~n",[Cachelines]),
      


loadTraces(TraceFile,TraceType) -> 
  {ok, S} = file:open(TraceFile, read),
  case TraceType of
    production ->
      readlines(S, []);
    finance ->
      readlines2(S, [])
  end.
    

 readlines(S, Reqs) ->
  Num = getSplitNum(),
  if 
    length(Reqs) =:= Num ->
      io:format("Reqs num = ~p, line limit = ~p~n", [length(Reqs), getLineLimit()]),      
      Resultlines = calCachelines(0,Reqs,[dict:new()]),
      {ok, S1} = file:open(getResultFile(), [append]),
      writeResultlines(S1,Resultlines),
      file:close(S1),
      readlines(S,[]);
    true -> ok
  end,
	case length(Reqs) >= getLineLimit() of 
	  true -> 
	    file:close(S),
      %io:format("Reqs:~p~n",[Reqs]),
      lists:reverse(Reqs);  
	  false -> 
      Line = io:get_line(S, ''),
      case Line of 
        eof -> 
             file:close(S),
             % io:format("Reqs:~p~n",[Reqs]),
             lists:reverse(Reqs); 
        " " ->
             file:close(S),
             % io:format("Reqs:~p~n",[Reqs]),
             lists:reverse(Reqs);  
        _ ->
        	% io:format("Lines:~p~n",[Line]),
        	[Res_str|_] = string:tokens(Line, ",{}\t\r\n"),
        	{Res,_} = string:to_integer(Res_str),
        	% io:format("~p~n",[Res]),
        	readlines(S,[Res|Reqs])
               
      end
  end.  

readlines2(S, Reqs) ->
  % io:format("~p~n",[Reqs]),
  Num = getSplitNum(),
  if 
    length(Reqs) =:= Num ->
      io:format("Reqs num = ~p, line limit = ~p~n", [length(Reqs), getLineLimit()]),      
      Resultlines = calCachelines(0,Reqs,[dict:new()]),
      {ok, S1} = file:open(getResultFile(), [append]),
      writeResultlines(S1,Resultlines),
      file:close(S1),
      readlines2(S,[]);
    true -> ok
  end,
  case length(Reqs) >= getLineLimit() of 
    true -> 
      file:close(S),
      % io:format("Reqs:~p~n",[Reqs]),
      lists:reverse(Reqs);  
    false -> 
      Line = io:get_line(S, ''),
      case Line of 
        eof -> 
             file:close(S),
             % io:format("Reqs:~p~n",[Reqs]),
             lists:reverse(Reqs);  
        {error,_} ->
              file:close(S),             
             lists:reverse(Reqs); 
        _ ->
          % io:format("Lines:~p~n",[Line]),
          [Type_str,_,Res_str] = string:tokens(Line, "\s"),
          {Type,_} = string:to_integer(Type_str),
           % io:format("Type:~p~n",[Type]),
          case Type=:=0 of
            true->
              {Res,_} = string:to_integer(Res_str),
               % io:format("Res:~p~n",[Res]),
              readlines2(S,[Res|Reqs]);
            _ ->
              readlines2(S,Reqs)
          end
          
               
      end
  end.  

  writeResultlines(_,[]) -> ok;
  writeResultlines(S,[H|T]) ->
   
    List = lists:reverse(lists:keysort(2,dict:to_list(H))),
    % [{CachelineID1,Number1}|T1] = List,
    %io:format(S, "~p ~p~n", [CachelineID1,Number1]),
    Sum = getSum(List),
    io:format(S, "Sign,~p,~p~n",[Sum/length(List),getOutAveNum(List,Sum/length(List))]),

    lists:foreach(fun(L) ->
          {CachelineID,Number} = L,
          %io:format("~p ~p ~p~n", [Type, DiskID, CachelineID]),
          io:format(S, "~p,~p~n", [CachelineID,Number])
          
               end, List),
    writeResultlines(S,T).

%return a list of Dict. Get the access number of each block. No order.
calCachelines(_,[],Resultlines) -> 	Resultlines;
calCachelines(Num,[H|Cachelines],Resultlines) ->
  CacheId = H,
  [Dict|T] = Resultlines,
  case dict:find(CacheId,Dict) of
    {ok,Value}  ->
      %io:format("~p~n", [Value]),
      
      D = dict:store(CacheId,Value+1,Dict);

    error -> 
      D = dict:store(CacheId,1,Dict)
      %io:format("~p ~n", [CacheId])
  end,
  case Num=:=getSplitNum() of
    true ->
      calCachelines(0,Cachelines,[dict:new(),D|T]);
    _ ->
      calCachelines(Num+1,Cachelines,[D|T])
  end.


  getSum([]) -> 0;
	getSum([{_,Num}|List]) -> Num + getSum(List).

  getOutAveNum([],_) -> 0;
  getOutAveNum([{_,Num}|List],Ave) -> 
    case Num>=Ave of
      true ->
        1+getOutAveNum(List,Ave);
      _ -> getOutAveNum(List,Ave)
    end.