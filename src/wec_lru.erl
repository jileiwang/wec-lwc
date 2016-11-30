-module(wec_lru).
-export([inLRUQueue/2, addtoLRUQueue/3, deleteFromLRUQueue/2, markLruQ/2, getSsdItemNumFromLru/1]).

-record(cacheline, {fileid=-1, access_num=1, last_access_time=-1, expireTime=-1, location=hdd, type=undefined}). 

%% -----------------------------------
%%            LRU Module
%% -----------------------------------
inLRUQueue(Q, CachelineID) ->
  R = lists:keyfind(CachelineID, 2, Q),
  case R of
    false -> {miss, Q, null};
    _ ->
      L = lists:delete(R, Q),          
      {hit, L, R}
  end.

addtoLRUQueue(Q, Item, CacheCap) -> 
  case CacheCap of 
    0 -> {Q, []};
    _ -> 
      case length(Q) < CacheCap of 
        true ->
          {[Item|Q], []};
        false ->
          Last = lists:last(Q),
          {[Item|lists:sublist(Q, length(Q)-1)], [Last]}      
      end
  end.


deleteFromLRUQueue(Q, DelItem) -> 
  lists:delete(DelItem, Q).  
  
markLruQ(SsdQ, N) -> markLruQ(SsdQ, N, []).
  
markLruQ([], Left, SsdQ) -> {Left, lists:reverse(SsdQ)};
markLruQ(Q, 0, SsdQ) -> {0, lists:reverse(SsdQ) ++ Q};
markLruQ([H|T], Left, SsdQ) -> 
  case H#cacheline.location of
    hdd ->
      H1 = H#cacheline{location=ssd},
      markLruQ(T, Left-1, [H1|SsdQ]);
    _ -> 
      markLruQ(T, Left, [H|SsdQ])
  end.

getSsdItemNumFromLru(SsdQ) -> getSsdItemNumFromLru(SsdQ, 0).

getSsdItemNumFromLru([], N) -> N;
getSsdItemNumFromLru([H|T], N) ->
  case H#cacheline.location of
    ssd -> getSsdItemNumFromLru(T, N+1);
    _ -> getSsdItemNumFromLru(T, N)
  end.
