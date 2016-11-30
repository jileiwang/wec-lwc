-module(wec_throttling).
-export([init/3, time_tick/2, consume/1]).

-record(throttling, {do, epoch, update_limit, left_updates}).
  
%% -----------------------------------
%%          Throttling Module
%% -----------------------------------  
init(Do, Epoch, UpdateLimit) ->
  #throttling{do=Do, epoch=Epoch, update_limit=UpdateLimit, left_updates=UpdateLimit}.

% may modify Throttling, return it.
time_tick(SysTime, Throttling) ->
  case Throttling#throttling.do of
    true ->     
      Epoch = Throttling#throttling.epoch,
      case SysTime rem Epoch of
        0 -> 
          UpdateLimit = Throttling#throttling.update_limit,          
          Throttling#throttling{left_updates=UpdateLimit};
        _ ->
          Throttling
      end;
    _ ->
      Throttling
  end.

% return if we can update (1-can, 0-can NOT).
consume(Throttling) ->
  case Throttling#throttling.do of
    true ->
      LeftUpdates = Throttling#throttling.left_updates,
      case LeftUpdates =< 0 of
        true -> 
          % io:format("0, ~p~n",[get("ssd_addinto_num")]),
          {0, Throttling};
        _ ->
          % io:format("1,~p, ~p~n",[get("read_req_num"),get("ssd_addinto_num")]),
          {1, Throttling#throttling{left_updates=LeftUpdates-1}}
      end;
    _ ->
      {1, Throttling}
  end.


