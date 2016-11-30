-module(trace_loader).
-export([start/0]).

% load original cctv trace, output in <time> <fileid> <duration> <flag1> <flag2> format 

getTraceFile(N) -> "F:/Trace/StreamingTrace/CCTV VOD/winmedialog/" ++ integer_to_list(N) ++ "/NetShow.050111000.log".
%getTraceFile() -> "D:/test.log".
getWriteFile(N) -> "D:/scale-mini-" ++ integer_to_list(N) ++ ".trace".

getLineLimit() -> 2500.

start() ->
  L = [{1, 68}, {2, 69}, {3, 71}, {4, 73}, {5, 74}, {6, 75}, {7, 76}, {8, 78}, {9, 79}, {10, 67}],
  lists:foreach(fun start/1, L).

start({I, N}) -> 
  {UrlDict, Reqs} = loadTraces(getTraceFile(N)),
  io:format("Trace [~p] req num = ~p, ", [N, length(Reqs)]),
  writeReqs(getWriteFile(I), Reqs),
  {TotalDuration, TotalFileLen} = getTotalFileLen(dict:to_list(UrlDict), 0, 0),
  io:format("total duration = ~p, total file len = ~p, total size = ~p GB~n", [TotalDuration, TotalFileLen, TotalFileLen*40/(1024*1024)]).
  %io:format("~nUrlDict = ~p~n~n, Reqs = ~p~n", [UrlDict, Reqs]).


getTotalFileLen([], Dur, Len) -> {Dur, Len};
getTotalFileLen([H|T], Dur, Len) -> 
  {_, [_, Duration, FileLen, _]} = H,
  if 
    FileLen < 0 -> io:format("Filelen = ~p~n", [FileLen]);
    true -> ok
  end,    
  %io:format("Filelen = ~p~n", [FileLen]),
  %io:format("H = ~p, FileLen = ~p~n", [H, FileLen]),
  getTotalFileLen(T, Duration+Dur, FileLen+Len).


readlines(S, Reqs, UrlDict, Skip) ->
  %if 
  %  length(Reqs) rem 10000 == 0 ->
	    %io:format("Reqs num = ~p, line limit = ~p~n", [length(Reqs), getLineLimit()]);
	%  true -> ok
	%end,
	case length(Reqs) >= getLineLimit() of 
	  true -> 
	    file:close(S),
      {UrlDict, lists:reverse(Reqs)};  
	  false -> 
      Line = io:get_line(S, ''),
      case Line of 
        eof -> 
          file:close(S),
          {UrlDict, lists:reverse(Reqs)};  
        _ ->
          case Skip > 0 of
            true ->
              readlines(S, Reqs, UrlDict, Skip-1);
            _ ->    
              Res = string:tokens(Line, " "),
              case Res=:=[] of 
                true -> readlines(S, Reqs, UrlDict, Skip);
                _ ->  
                  % <time(3)>, <url(5)>, <starttime(6)>, <duration(7)>, <rate(8)>, <filelength(20)>, <filesize(21)>
                  case length(Res) < 21 of
                    true ->
                      readlines(S, Reqs, UrlDict, Skip);
                    _ ->
                      [_, _, Time_str, _, Url, Startime_str, Duration_str, Rate_str, _, _, _, _, _, _, _, _, _, _, _, FileLength_str, FileSize_str|_] = Res,  
                      Times = string:tokens(Time_str, ":"),
                      case length(Times) < 3 of
                        true ->
                          readlines(S, Reqs, UrlDict, Skip);
                        false ->
                          [Hour_str, Min_str, Sec_str] = Times,
                          {Hour, _} = string:to_integer(Hour_str),
                          {Min, _} = string:to_integer(Min_str),
                          {Sec, _} = string:to_integer(Sec_str),                 
                          Time = Hour*3600 + Min*60 + Sec,
                          {Duration, _} = string:to_integer(Duration_str),
                          case Duration of 
                            0 -> 
                              readlines(S, Reqs, UrlDict, Skip);                                    
                            _ ->
                              {Filelength, _} = string:to_integer(FileLength_str),
                              {Filesize, _} = string:to_integer(FileSize_str),
                              Filelength1 = verify(Filelength, Duration),
                              case Filelength1 of
                                error ->
                                  readlines(S, Reqs, UrlDict, Skip);
                                _ ->                       
                                  {UrlDict1, Urlid} = processURL(UrlDict, Url, Duration, Filelength1, Filesize),                 
                                  readlines(S, [{Time, Urlid, Duration, 0, 3}|Reqs], UrlDict1, Skip)
                              end
                          end
                      end
                  end
              end
          end
      end
  end.  
  
verify(Filelength, Duration) ->
  case is_integer(Duration) of
    false -> error;
    _ ->
      case is_integer(Filelength) of
        false -> Duration;
        _ -> Filelength
      end
  end.
                           
% return Url's id
processURL(UrlDict, Url, Duration, Filelength, Filesize) ->
  R = dict:is_key(Url, UrlDict),
  case R of
    false ->
      UrlID = dict:size(UrlDict),
      UrlDict1 = dict:store(Url, [UrlID, Duration, Filelength, Filesize], UrlDict),
      {UrlDict1, UrlID};      
    _ -> 
      [UrlID, _, _, _] = dict:fetch(Url, UrlDict),
      {UrlDict, UrlID}
  end.


loadTraces(TraceFile) -> 
  {ok, S} = file:open(TraceFile, read),
  readlines(S, [], dict:new(), 4).
  

writeReqs(File, Reqs) ->
	{ok, S} = file:open(File, write), 
  %∏Ò Ω£∫
  % <timestamp(second)>, <fileid>, <duration>, <flag1>, <flag2> 
	lists:foreach(fun(Req) ->
	        {Time, Urlid, Duration, F1, F2} = Req,
	        %io:format("~p ~p ~p ~p ~p~n", [Time, Urlid, Duration, F1, F2]),
					io:format(S, "~p ~p ~p ~p ~p~n", [Time, Urlid, Duration, F1, F2])
			         end, Reqs),
	file:close(S).

	