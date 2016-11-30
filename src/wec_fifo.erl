-module(wec_fifo).
-export([inFIFOQueue/2, addtoFIFOQueue/3, deleteFromFIFOQueue/2, markFifoQ/2, getSsdItemNumFromFifo/1]).

-record(cacheline, {fileid=-1, access_num=1, last_access_time=-1, expireTime=-1, location=hdd, type=undefined}). 

%% -----------------------------------
%%            FIFO Module
%% -----------------------------------
inFIFOQueue(Q, CachelineID) -> 
  R = lists:keyfind(CachelineID, 2, Q),
  case R of 
    false -> {miss, Q, null};
    _ -> {hit, Q, R}
  end. 
  
addtoFIFOQueue(Q, Item, CacheCap) -> 
  case CacheCap of 
    0 -> {Q, []};
    _ ->
      R = lists:keyfind(Item#cacheline.fileid, 2, Q),
      case R of 
        false ->
          case length(Q) < CacheCap of 
            true ->
              {[Item|Q], []};
            false ->
              Last = lists:last(Q),
              {[Item|lists:sublist(Q, length(Q)-1)], [Last]}      
          end;
        _ ->
          {Q, []}
      end
  end.
  
deleteFromFIFOQueue(Q, _DelItem) -> Q.

markFifoQ(SsdQ, N) -> markFifoQ(SsdQ, N, []).
  
markFifoQ([], Left, SsdQ) -> {Left, lists:reverse(SsdQ)};
markFifoQ(Q, 0, SsdQ) -> {0, lists:reverse(SsdQ) ++ Q};
markFifoQ([H|T], Left, SsdQ) -> 
  case H#cacheline.location of
    hdd ->
      H1 = H#cacheline{location=ssd},
      markFifoQ(T, Left-1, [H1|SsdQ]);
    _ -> 
      markFifoQ(T, Left, [H|SsdQ])
  end.
  
getSsdItemNumFromFifo(SsdQ) -> getSsdItemNumFromFifo(SsdQ, 0).

getSsdItemNumFromFifo([], N) -> N;
getSsdItemNumFromFifo([H|T], N) ->
  case H#cacheline.location of
    ssd -> getSsdItemNumFromFifo(T, N+1);
    _ -> getSsdItemNumFromFifo(T, N)
  end.
    
