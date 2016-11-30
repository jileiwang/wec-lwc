-module(trace_filter).
-export([start/0]).

% convert trace's format, -> <r/w, disk_id, cacheline_id>  r=0, w=1

 getTraceFile() -> "D:/test.csv".        % around one day
  % getTraceFile() -> "D:/24.hour.BuildServer.11-28-2007.07-39-PM.trace.csv".
 % getTraceFile() -> "L:/Trace/MS-Production/BuildServer00/BuildServer/Traces/24.hour.BuildServer.11-28-2007.07-55-PM.trace.csv".
%getWriteFile() -> "D:/test.req".
%getTraceFile() -> "D:/LiveMapsBE.02-21-2008.11-30-AM.trace.csv.csv".
getWriteFile() -> "D:/tmp.csv".

getLineLimit() -> 60000000.

start() -> 
  loadTraces(getTraceFile(), getWriteFile(), production).


readlines(S, S1, Count) ->
	io:format("Count1 = ~p~n",[Count]),
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
             Res = string:tokens(Line, ",\t"),
             case Res=:=[] of 
               true -> readlines(S, S1, Count);
               _->
               case Res=:=["\n"] of
                 true -> readlines(S, S1, Count);
               
               _ ->  

                 % DiskRead,     153302, p1 ( 4),         36, 0xfffffa8024947010, 0x01b0130000, 0x00001000,        5434,        0, 0x060043,        5434,       3, Original, 0xfffffa8007d4d460, "Disk1:\s0"
                 [Type, TimeStamp|_] = Res,
                 % io:format("~p~n", [TimeStamp]),  
                 case string:strip(Type) of 
                    "FileIoRead" ->
                    io:format("FileIoRead Count1 = ~p~n",[Count]),
                      [_, _, _, _, _, _, _, Blkno_str, Size_str|_] = Res,                     
                      Blkno_str1 = string:sub_string(string:strip(Blkno_str), 3),
                      %io:format("FileIoRead, Blkno_str=~p, Size_str=~p~n", [Blkno_str1, Size_str]),
                      Blkno = list_to_integer(Blkno_str1, 16),
                      Size_str1 = string:sub_string(string:strip(Size_str), 3),                 
                      Size = list_to_integer(Size_str1, 16),
                      io:format(S1, "~p,~p,~p,~p,~p~n", [0, 0, Blkno, Size, 0]),
                      readlines(S, S1, Count+1);
                    "FileIoWrite" ->                       
                    io:format("FileIoWrite Count1 = ~p~n",[Count]),
                      [_, _, _, _, _, _, _, Blkno_str, Size_str|_] = Res,
                      Blkno_str1 = string:sub_string(string:strip(Blkno_str), 3),
                      %io:format("FileIoWrite, Blkno_str=~p, Size_str=~p~n", [Blkno_str1, Size_str]),
                      Blkno = list_to_integer(Blkno_str1, 16),
                      Size_str1 = string:sub_string(string:strip(Size_str), 3),                 
                      Size = list_to_integer(Size_str1, 16),
                      io:format(S1, "~p,~p,~p,~p,~p~n", [1, 0, Blkno, Size, 0]),
                      readlines(S, S1, Count+1);              
                    "DiskRead" ->
			                io:format("DiskRead Count1 = ~p~n",[Count]),
                      [_, _, _, _, _, Blkno_str, Size_str|_] = Res,                     
                      Blkno_str1 = string:sub_string(string:strip(Blkno_str), 3),
                      %io:format("DiskRead, Blkno_str=~p, Size_str=~p~n", [Blkno_str1, Size_str]),
                      Blkno = list_to_integer(Blkno_str1, 16),
                      Size_str1 = string:sub_string(string:strip(Size_str), 3),                 
                      Size = list_to_integer(Size_str1, 16),
                      io:format(S1, "~p,~p,~p,~p,~p~n", [0, 0, Blkno, Size, 0]),
                      readlines(S, S1, Count+1);                      
                    "DiskWrite" ->    
                    io:format("DiskWrite Count1 = ~p~n",[Count]),                   
                      [_, _, _, _, _, Blkno_str, Size_str|_] = Res,
                      Blkno_str1 = string:sub_string(string:strip(Blkno_str), 3),
                      %io:format("DiskWrite, Blkno_str=~p, Size_str=~p~n", [Blkno_str1, Size_str]),
                      Blkno = list_to_integer(Blkno_str1, 16),
                      Size_str1 = string:sub_string(string:strip(Size_str), 3),                 
                      Size = list_to_integer(Size_str1, 16),
                      io:format(S1, "~p,~p,~p,~p,~p~n", [1, 0, Blkno, Size, 0]),
                      readlines(S, S1, Count+1);
                    _ -> 
                    io:format("Type: ~p~n",[Type]),
                    readlines(S, S1, Count)
                 end 
             end
            end
      end
  end.  

skipheader(S) ->
  Line = io:get_line(S, ''),
  io:format("~p~n",[Line]),
  case Line of 
        eof -> 
             file:close(S),
             ok;
        _ ->
  Res = string:tokens(Line, ","),
  
  
      case Res=:=["EndHeader\n"]  of
          true->
      % io:format("End"),
      S;
    _ ->

      skipheader(S)
    
  end
  end.


loadTraces(TraceFile, WriteFile, TraceType) -> 
%io:format("enter loadTraces~n"),
  % io:format("TraceType = ~p~n", [TraceType]),
  {ok, S} = file:open(TraceFile, read),
	{ok, S1} = file:open(WriteFile, [append]),
%	io:format("fileopen~n"),
  case TraceType of 
    production ->
      
      readlines(skipheader(S), S1, 0);
    _ -> 
      error
  end. 
  
	