-module(trace_extraction).
-export([start/0]).

% convert trace's format, -> <disk_id, cacheline_id>
%cacheline_id指的是以cache块大小切割后的read请求


%getTraceFile() -> "H:/Trace/StreamingTrace/CCTV VOD/scale trace/scale1-1.trace".
getWriteFile() -> "D:/test.req".
getTraceFile() -> "D:/tmp.csv".


getLineLimit() -> 20000000.


start() -> 
CachelineSize = 8192,
TraceType = production,
  Reqs = loadTraces(getTraceFile(), TraceType),
  %io:format("test:~p~n",[Reqs]),
  io:format("req num = ~p~n", [length(Reqs)]),
  %io:format("req = ~p~n", [Reqs]),
  case TraceType of 
    cctv -> 
      Cachelines = genStreamAccesses(Reqs),
      writeCachelines(getWriteFile(), Cachelines);
    _ ->
		
      {Cachelines, _,_} = split2cacheline(Reqs, CachelineSize, [], 0, 0),
	  %io:format("test2~n"),
      %statCachelines是啥功能？？？
      %{ClStat, ReadNum1, WriteNum1} = statCachelines(lists:reverse(Cachelines), 0, 0),
      %case WriteNum1=:=0 of
      %  true -> io:format("unique cachelines num = ~p, theoritical CHR = ~p, all read~n", [length(ClStat), (ReadNum1-length(ClStat))/ReadNum1]);
      %  false -> io:format("unique cachelines num = ~p, theoritical CHR = ~p, read/write = ~p~n", [length(ClStat), (ReadNum1-length(ClStat))/ReadNum1, ReadNum1/WriteNum1])
      %end,
      %io:format("unique cachelines = ~p~n", [ClStat]),
      writeCachelines(getWriteFile(), lists:reverse(Cachelines))
      % Resultlines = calCachelines(0,Cachelines,[dict:new()]),
      % {ok, S} = file:open(getResultFile(), write),
      % writeResultlines(S,Resultlines),
      % file:close(S)      

  end.

% req format: {<Time(sec)>, <Fileid>, <Duration>}
% cacheline format: {0, 0, <CachelineID>}
% CachelineID = FileID*10000 + Offset (0~Duration-1)
genStreamAccesses(Reqs) -> 
  Raw = genStreamAccesses(Reqs, [], []),
  io:format("Raw size = ~p~n", [length(Raw)]),
  SortedL = lists:sort(fun time_cmp/2, Raw),
  %io:format("SortedL = ~p~n", [SortedL]),
  trans2Cacheline(SortedL).

genStreamAccesses([], Result, Stat) ->
  CachelineNum = getCachelineNum(Stat),
  io:format("unique cacheline num = ~p~n", [CachelineNum]),
  Result;
genStreamAccesses([H|T], Result, Stat) -> 
  {Time, FileID, Duration} = H,
  Stat1 = updateStat(Stat, FileID, Duration),  
  case Duration > 0 of
    false ->
      genStreamAccesses(T, Result, Stat1);
    true ->
      Result1 = addItems(Time, FileID, 0, Duration, Result),
      genStreamAccesses(T, Result1, Stat1)
  end.

getCachelineNum(Stat) -> getCachelineNum(Stat, 0).

getCachelineNum([], Num) -> Num;
getCachelineNum([H|T], Num) ->
  {_, ClNum} = H, 
  getCachelineNum(T, Num+ClNum).  

updateStat(Stat, FileID, NewDur) ->
  R = lists:keyfind(FileID, 1, Stat),
  case R of
    false -> [{FileID, NewDur}|Stat];
    _ ->
      {_, OldDur} = R,
      case NewDur > OldDur of
        true -> [{FileID, NewDur}|lists:delete(R, Stat)];
        _ -> Stat
      end
  end.
  

addItems(_Time, _FileID, _Duration, 0, Result) -> Result;
addItems(Time, FileID, Duration, Count, Result) ->
  CachelineID = FileID*10000 + Duration,
  %io:format("Time= ~p, CachelineID = ~p, Count=~p~n", [Time, CachelineID, count]),
  addItems(Time+1.0, FileID, Duration+1, Count-1, [{Time, CachelineID}|Result]).

trans2Cacheline(L) -> lists:reverse(trans2Cacheline(L, [])).

trans2Cacheline([], R) -> R;
trans2Cacheline([H|T], R) ->
  {_, CachelineID} = H,
  trans2Cacheline(T, [{0, 0, CachelineID}|R]). 

time_cmp(Req1, Req2) ->
  {T1, _} = Req1,
  {T2, _} = Req2,
  T1 =< T2.

% split2cacheline将读入数据按照cache块大小切割为cachelineID
% [{Blkno, Size}...] -> [{cachelineID} ...]  
% MaxLBA是所有数据中地址上限 max l... block address
split2cacheline([], _CachelineSize, Cachelines, MaxLBA, ReadNum) -> {Cachelines, MaxLBA, ReadNum};
split2cacheline([H|T], CachelineSize, Cachelines, MaxLBA, ReadNum) -> 
	{Blkno, Size} = H,
	FirstCachelineID = Blkno div CachelineSize,
	LastCachelineID = (Blkno+Size-1) div CachelineSize,
	%io:format("H=~p, First=~p, Last=~p~n", [H, FirstCachelineID, LastCachelineID]),
	L = genCachelines( FirstCachelineID, LastCachelineID, Cachelines),
	MaxLBA1 = getMaxLBA(MaxLBA, Blkno, Size),
	split2cacheline(T, CachelineSize, L, MaxLBA1, ReadNum+1).

genCachelines( Last, Last, Cachelines) -> [{Last}|Cachelines];
genCachelines(First, Last, Cachelines) -> genCachelines(First+1, Last, [{First}|Cachelines]).

getMaxLBA(MaxLBA, Blkno, Size) ->
  case MaxLBA < Blkno+Size of
    true -> Blkno+Size;
    _ -> MaxLBA
  end. 




%statCachelines(Cachelines, ReadNum) -> statCachelines(Cachelines, [], ReadNum).

%statCachelines([], ClStat, ReadNum) -> {ClStat, ReadNum};
%statCachelines([H|T], ClStat, ReadNum) -> 
%  {CachelineID} = H,%
%  {ReadNum1, WriteNum1} = updateRWNum(ReadNum, WriteNum, Type),
 % case Type of 
  %  1 -> statCachelines(T, ClStat, ReadNum1, WriteNum1);  % write op
   % 0 ->                            % read op   
    %  R = lists:member(CachelineID, ClStat),
    %	case R of 
    %		false -> statCachelines(T, [CachelineID|ClStat], ReadNum1, WriteNum1);
    %		_ -> statCachelines(T, ClStat, ReadNum1, WriteNum1) 
     % end
 % end.
 
 
  
  

readlines(S, Reqs) ->
  if 
    length(Reqs) rem 10000 == 0 ->
	     io:format("Reqs num = ~p, line limit = ~p~n", [length(Reqs), getLineLimit()]);
	  true -> ok
	end,
	case length(Reqs) >= getLineLimit() of 
	  true -> 
	    file:close(S),
      lists:reverse(Reqs);  
	  false -> 
      Line = io:get_line(S, ''),
      case Line of 
        eof -> 
             file:close(S),
             lists:reverse(Reqs);  
        _ ->
             Res = string:tokens(Line, ", \t\r\n"),
             case Res=:=[] of 
               true -> readlines(S, Reqs);
               _ ->  
                 % 128166386724652876,web,1,Read,1422921216,7680,1935
                 % <timestamp>, <server>, <diskno>, <type>, <blkno(Byte)>, <size(Byte)>, <response time>
                 [_, _, DiskID_str, Type, Blkno_str, Size_str, RT_str|_] = Res,  
                 {DiskID, _} = string:to_integer(DiskID_str),
                 {Blkno, _} = string:to_integer(Blkno_str),
                 {Size, _} = string:to_integer(Size_str),
                 {RT, _} = string:to_integer(RT_str),
                 case Type of 
                    "Read" -> readlines(S, [{0, DiskID, Blkno, Size, RT}|Reqs]);
                    "Write" -> readlines(S, [{1, DiskID, Blkno, Size, RT}|Reqs])
                 end 
             end
      end
  end.  
  
readlines2(S, Reqs) ->
  if 
    length(Reqs) rem 10000 == 0 ->
	    io:format("Reqs num = ~p, line limit = ~p~n", [length(Reqs), getLineLimit()]);
	  true -> ok
	end,
	case length(Reqs) >= getLineLimit() of 
	  true -> 
	    file:close(S),
      lists:reverse(Reqs);  
	  false -> 
      Line = io:get_line(S, ''),
      case Line of 
        eof -> 
             file:close(S),
             lists:reverse(Reqs);  
        _ ->
             Res = string:tokens(Line, ", \t\r\n"),
             case Res=:=[] of 
               true -> readlines2(S, Reqs);
               _ ->  
                 % 2,1093855,512,r,0.039321
                 % <app id>, <lba(byte)>, <size(byte)>, <r/w>, <timestamp(second)>
                 [_, Blkno_str, Size_str, Type, _|_] = Res,  
                 {Blkno, _} = string:to_integer(Blkno_str),
                 {Size, _} = string:to_integer(Size_str),
                 case Type of 
                    "r" -> readlines2(S, [{0, 0, Blkno, Size, 0}|Reqs]);
                    "R" -> readlines2(S, [{0, 0, Blkno, Size, 0}|Reqs]);
                    "w" -> readlines2(S, [{1, 0, Blkno, Size, 0}|Reqs]);
                    "W" -> readlines2(S, [{1, 0, Blkno, Size, 0}|Reqs])
                 end 
             end
      end
  end.  
% 将所有读请求的，忽略写请求，且只能读LineLimit个。格式为{Blkno, Size}组成的列表
readlines3(S, Reqs) ->
  if 
    length(Reqs) rem 10000 == 9999 ->
	    io:format("Reqs num = ~p, line limit = ~p~n", [length(Reqs), getLineLimit()]);
	  true -> ok
	end,
	case length(Reqs) >= getLineLimit() of 
	  true -> 
	    file:close(S),
      %io:format("~p~n",[Reqs]),
      lists:reverse(Reqs);  
	  false -> 
      Line = io:get_line(S, ''),
      case Line of 
        eof -> 
             file:close(S),
             lists:reverse(Reqs);  
        _ ->
             Res = string:tokens(Line, ","),
             case Res=:=[] of 
               true -> readlines3(S, Reqs);
               _ ->  
			   %Res有五个参数，第五个如何处理？
                 [Type, _, Blkno_str, Size_str|_] = Res,  
                 case string:strip(Type) of 
                    "0" ->                     
                      {Blkno, _} = string:to_integer(Blkno_str),              
                      {Size, _} = string:to_integer(Size_str),
                      readlines3(S, [{Blkno, Size}|Reqs]);                      
                    "1" ->                                             
                      readlines3(S, Reqs);                    
                    _ -> readlines3(S, Reqs)
                 end 
             end
      end
  end.  

% for scale traces, Prefix is set different values for different trace;
% for general cctv case, Prefix is set 0.
readlines4(S, Reqs) ->
  if 
    length(Reqs) rem 10000 == 0 ->
	    io:format("Reqs num = ~p, line limit = ~p~n", [length(Reqs), getLineLimit()]);
	  true -> ok
	end,
	case length(Reqs) >= getLineLimit() of 
	  true -> 
	    file:close(S),
      lists:reverse(Reqs);  
	  false -> 
      Line = io:get_line(S, ''),
      case Line of 
        eof -> 
             file:close(S),
             lists:reverse(Reqs);  
        _ ->
             Res = string:tokens(Line, ", \t\r\n"),
             case Res=:=[] of 
               true -> readlines4(S, Reqs);
               _ ->  
                 % 4.026150432936176	69	100	0	3
                 % <timestamp(second)>, <fileid>, <duration>, <flag1>, <flag2>
                 [Time_str, Fileid_str, Duration_str|_] = Res,  
                 {Time, _} = string:to_integer(Time_str),
                 {Fileid, _} = string:to_integer(Fileid_str),
                 {Duration, _} = string:to_integer(Duration_str),
                 readlines4(S, [{Time, Fileid, Duration}|Reqs])
             end
      end
  end.  



loadTraces(TraceFile, TraceType) -> 
  {ok, S} = file:open(TraceFile, read),
  io:format("TraceType = ~p~n", [TraceType]),
  case TraceType of 
    cambridge -> 
      readlines(S, []);
    spc ->
      readlines2(S, []);
    production ->
      readlines3(S, []);
    cctv ->
      readlines4(S, []);
    _ -> 
      error
  end. 
  

% Cacheline Format: <CachelineID >
writeCachelines(File, Cachelines) ->
	{ok, S} = file:open(File, write),
	lists:foreach(fun(Cacheline) ->
	        {CachelineID} = Cacheline,
	        %io:format("~p ~p ~p~n", [Type, DiskID, CachelineID]),
					io:format(S, "~p~n", [CachelineID])
			         end, Cachelines),
	file:close(S).

