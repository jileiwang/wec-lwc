-module(trace_filter2).
-export([start/0]).

% convert trace's format, -> <r/w, disk_id, cacheline_id>  r=0, w=1

%getTraceFile() -> "D:/test.csv".        % around one day
getTraceFile() -> "D:/24.hour.BuildServer.11-28-2007.07-39-PM.trace.csv".
%getWriteFile() -> "D:/test.req".
%getTraceFile() -> "D:/External Library/Trace/Auspex Server/trace_1".
getWriteFile() -> "D:/test.csv".

getLineLimit() -> 150000.

start() -> 
  loadTraces(getTraceFile(), getWriteFile(), production).


readlines(S, S1, Count) ->
  if 
    Count rem 10000 == 9999 -> io:format("Reqs num = ~p, line limit = ~p~n", [Count, getLineLimit()]);
	  true -> now
	end,
	case Count >= getLineLimit() of 
	  true -> 
         file:close(S),
         file:close(S1),
         ok;  
	  false -> 
      Line = io:get_line(S, ''),
      case Line of 
        eof -> 
             file:close(S),
             file:close(S1),
             ok;  
        _ ->
             Res = string:tokens(Line, " "),
             case Res=:=[] of 
               true -> readlines(S, S1, Count);
               _ ->  
                 % DiskRead,     153302, p1 ( 4),         36, 0xfffffa8024947010, 0x01b0130000, 0x00001000,        5434,        0, 0x060043,        5434,       3, Original, 0xfffffa8007d4d460, "Disk1:\s0"
                 [TimeStamp, Type1, Type2, _, Fidstr, _, Offsetstr, _, Sizestr|_] = Res,
                 %io:format("~p~n", [TimeStamp]),
                 G1 = string:strip(Type1) =:= "Block" andalso string:strip(Type2) =:= "READ",
                 G2 = string:strip(Type1) =:= "Dir" andalso string:strip(Type2) =:= "READ",
                 G3 = string:strip(Type1) =:= "Block" andalso string:strip(Type2) =:= "WRITE",
                 if
                 		%e.g. 748521923.402318 Block READ FID: 5e1e000000004d79 OFF: 1744896 SIZE: 4096 HOST: 1066
                 		 G1 ->
                      BlknoInit = list_to_integer(Fidstr, 16),
                 			Offset = list_to_integer(Offsetstr, 10),
                 			Blkno = BlknoInit + Offset,
                 			Size = list_to_integer(Sizestr, 10),
                      io:format(S1, "~p,~p,~p,~p,~p~n", [0, 0, Blkno, Size, 0]),
                      readlines(S, S1, Count+1);                      
                     G2 ->
                      BlknoInit = list_to_integer(Fidstr, 16),
                 			Offset = list_to_integer(Offsetstr, 10),
                 			Blkno = BlknoInit + Offset,
                 			Size = list_to_integer(Sizestr, 10),
                      io:format(S1, "~p,~p,~p,~p,~p~n", [0, 0, Blkno, Size, 0]),
                      readlines(S, S1, Count+1);                      
                     G3 ->                    
                      BlknoInit = list_to_integer(Fidstr, 16),
                 			Offset = list_to_integer(Offsetstr, 10),
                 			Blkno = BlknoInit + Offset,
                 			Size = list_to_integer(Sizestr, 10),
                      io:format(S1, "~p,~p,~p,~p,~p~n", [1, 0, Blkno, Size, 0]),
                      readlines(S, S1, Count+1);                      
                    true -> 
                    	readlines(S, S1, Count)
                 end 
             end
      end
  end.  

loadTraces(TraceFile, WriteFile, TraceType) -> 
  io:format("TraceType = ~p~n", [TraceType]),
  {ok, S} = file:open(TraceFile, read),
	{ok, S1} = file:open(WriteFile, write),
  case TraceType of 
    production ->
      readlines(S, S1, 0);
    as ->
    	readlines(S, S1, 0);
    _ -> 
      error
  end. 
  
	