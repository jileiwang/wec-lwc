-module(trace_convertor).
-export([start/2]).

% convert trace's format, -> <r/w, disk_id, cacheline_id>  r=0, w=1


%getTraceFile() -> "H:/Trace/StreamingTrace/CCTV VOD/scale trace/scale1-1.trace".
getWriteFile() -> "D:/test.req".
getTraceFile() -> "D:/test.csv".

getLineLimit() -> 200.

start(CachelineSize, TraceType) -> 
  Reqs = loadTraces(getTraceFile(), TraceType),
  io:format("req num = ~p~n", [length(Reqs)]),
  %io:format("req = ~p~n", [Reqs]),
  case TraceType of 
    cctv -> 
      Cachelines = genStreamAccesses(Reqs),
      writeCachelines(getWriteFile(), Cachelines);
    _ ->
		io:format("test1~n"),
      {Cachelines, MaxLBA, ReadNum, WriteNum} = split2cacheline(Reqs, CachelineSize, [], 0, 0, 0),
	  io:format("test2~n"),
      case WriteNum=:=0 of
        true -> io:format("cachelines num = ~p, MaxLBA = ~p, all read~n", [length(Cachelines), MaxLBA]); 
        false ->
          io:format("cachelines num = ~p, MaxLBA = ~p, read/write = ~p~n", [length(Cachelines), MaxLBA, ReadNum/WriteNum])
      end,
      {ClStat, ReadNum1, WriteNum1} = statCachelines(lists:reverse(Cachelines), 0, 0),
      case WriteNum1=:=0 of
        true -> io:format("unique cachelines num = ~p, theoritical CHR = ~p, all read~n", [length(ClStat), (ReadNum1-length(ClStat))/ReadNum1]);
        false -> io:format("unique cachelines num = ~p, theoritical CHR = ~p, read/write = ~p~n", [length(ClStat), (ReadNum1-length(ClStat))/ReadNum1, ReadNum1/WriteNum1])
      end,
      %io:format("unique cachelines = ~p~n", [ClStat]),
      writeCachelines(getWriteFile(), lists:reverse(Cachelines))
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


% [{read/write, diskID, Blkno, Size, RT}...] -> [{read/write, diskID, cachelineID} ...]             
split2cacheline([], _CachelineSize, Cachelines, MaxLBA, ReadNum, WriteNum) -> {Cachelines, MaxLBA, ReadNum, WriteNum};
split2cacheline([H|T], CachelineSize, Cachelines, MaxLBA, ReadNum, WriteNum) -> 
	{Type, DiskID, Blkno, Size, _RT} = H,
	FirstCachelineID = Blkno div CachelineSize,
	LastCachelineID = (Blkno+Size-1) div CachelineSize,
	%io:format("H=~p, First=~p, Last=~p~n", [H, FirstCachelineID, LastCachelineID]),
	L = genCachelines(Type, DiskID, FirstCachelineID, LastCachelineID, Cachelines),
	MaxLBA1 = getMaxLBA(MaxLBA, Blkno, Size),
	{ReadNum1, WriteNum1} = updateRWNum(ReadNum, WriteNum, Type),
  split2cacheline(T, CachelineSize, L, MaxLBA1, ReadNum1, WriteNum1).

genCachelines(Type, DiskID, Last, Last, Cachelines) -> [{Type, DiskID, Last}|Cachelines];
genCachelines(Type, DiskID, First, Last, Cachelines) -> genCachelines(Type, DiskID, First+1, Last, [{Type, DiskID, First}|Cachelines]).

getMaxLBA(MaxLBA, Blkno, Size) ->
  case MaxLBA < Blkno+Size of
    true -> Blkno+Size;
    _ -> MaxLBA
  end. 

updateRWNum(ReadNum, WriteNum, Type) ->
  case Type of 
    0 -> {ReadNum+1, WriteNum};
    1 -> {ReadNum, WriteNum+1}
  end.


statCachelines(Cachelines, ReadNum, WriteNum) -> statCachelines(Cachelines, [], ReadNum, WriteNum).

statCachelines([], ClStat, ReadNum, WriteNum) -> {ClStat, ReadNum, WriteNum};
statCachelines([H|T], ClStat, ReadNum, WriteNum) -> 
  {Type, _DiskID, CachelineID} = H,
  {ReadNum1, WriteNum1} = updateRWNum(ReadNum, WriteNum, Type),
  case Type of 
    1 -> statCachelines(T, ClStat, ReadNum1, WriteNum1);  % write op
    0 ->                            % read op   
      R = lists:member(CachelineID, ClStat),
    	case R of 
    		false -> statCachelines(T, [CachelineID|ClStat], ReadNum1, WriteNum1);
    		_ -> statCachelines(T, ClStat, ReadNum1, WriteNum1) 
      end
  end.
  
  

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

readlines3(S, Reqs) ->
  if 
    length(Reqs) rem 10000 == 9999 ->
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
             Res = string:tokens(Line, ","),
             case Res=:=[] of 
               true -> readlines3(S, Reqs);
               _ ->  
                 [Type, _, Blkno_str, Size_str|_] = Res,  
                 case string:strip(Type) of 
                    "0" ->                     
                      {Blkno, _} = string:to_integer(Blkno_str),              
                      {Size, _} = string:to_integer(Size_str),
                      readlines3(S, [{0, 0, Blkno, Size, 0}|Reqs]);                      
                    "1" ->                       
                      {Blkno, _} = string:to_integer(Blkno_str),              
                      {Size, _} = string:to_integer(Size_str),
                      readlines3(S, [{1, 0, Blkno, Size, 0}|Reqs]);                    
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
  

% Cacheline Format: < Type, DiskID, CachelineID >
writeCachelines(File, Cachelines) ->
	{ok, S} = file:open(File, write),
	lists:foreach(fun(Cacheline) ->
	        {Type, DiskID, CachelineID} = Cacheline,
	        %io:format("~p ~p ~p~n", [Type, DiskID, CachelineID]),
					io:format(S, "~p ~p ~p~n", [Type, DiskID, CachelineID])
			         end, Cachelines),
	file:close(S).

	