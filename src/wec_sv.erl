-module(wec_sv).
-export([initSV/4, tick/0, checkEnterCache/1, resetSieve/1, accumSieve/1]).

%% -----------------------------------
%%     Sieve Module
%% -----------------------------------
% SieveMode may be miss_sieve, time_sieve or false
initSV(Mode, Thr, Period, TailPercentage) ->
  put("sieve_mode", Mode),
  put("sieve_thr", Thr),
  put("time_period", Period),
  put("tail_percentage", TailPercentage),
  put("time_counter", 0).


tick() ->
  Counter = get("time_counter"),
  put("time_counter", Counter+1).
  

% called whn miss
checkEnterCache(CachelineID) ->
  Thr = get("sieve_thr"),
  Miss = get(CachelineID),
  %io:format("checkEnterCache(), CachelineID=~p, Thr=~p, Miss=~p~n", [CachelineID, Thr, Miss]),
  case Miss =/= undefined of
    false -> false;   % < thr, will not enter cache
    true -> Miss >= Thr
  end.


% called when cache hit
resetSieve(CachelineID) ->
  put(CachelineID, 0).
  
  
% called when miss
accumSieve(CachelineID) ->
  N = get(CachelineID),
  case N =/= undefined of
    false -> put(CachelineID, 1);  
    true -> put(CachelineID, N+1) 
  end.

