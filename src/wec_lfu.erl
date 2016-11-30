-module(wec_lfu).
-export([inLFUQueue/2, addtoLFUQueue/3, deleteFromLFUQueue/2, markLfuQ/2, getSsdItemNumFromLfu/1]).

-record(cacheline, {fileid=-1, access_num=1, last_access_time=-1, expireTime=-1, location=hdd, type=undefined}). 

%% -----------------------------------
%%            LFU Module
%% -----------------------------------
inLFUQueue(Q, CachelineID) ->
  R = lists:keyfind(CachelineID, 2, Q),
  case R of
    false -> {miss, Q, null};
    _ ->
      L = lists:delete(R, Q),          
      {hit, L, R}
  end.
  
addtoLFUQueue(Q, Item, CacheCap) ->
  case CacheCap of
    0 -> {Q, []};
    _ ->
      Pos = ite(Q, Item#cacheline.access_num),
      case length(Q) < CacheCap of 
        true ->
          {lists:sublist(Q, Pos-1) ++ [Item] ++ lists:sublist(Q, Pos, length(Q)-Pos+1), []};
        false ->
          case Pos>length(Q) of
            true -> 
              {Q, Item};
            _ -> 
              Last = lists:last(Q),
              {lists:sublist(Q, Pos-1) ++ [Item] ++ lists:sublist(Q, Pos, length(Q)-Pos), [Last]}
          end       
      end
  end.

deleteFromLFUQueue(Q, DelItem) -> 
  lists:delete(DelItem, Q).

markLfuQ(SsdQ, N) -> markLfuQ(SsdQ, N, []).
  
markLfuQ([], Left, SsdQ) -> {Left, lists:reverse(SsdQ)};
markLfuQ(Q, 0, SsdQ) -> {0, lists:reverse(SsdQ) ++ Q};
markLfuQ([H|T], Left, SsdQ) -> 
  case H#cacheline.location of
    hdd ->
      H1 = H#cacheline{location=ssd},
      markLfuQ(T, Left-1, [H1|SsdQ]);
    _ -> 
      markLfuQ(T, Left, [H|SsdQ])
  end.
  
% return the pos that is the last one with the access_num larger than given. 
ite(Q, Access_num) -> ite(Q, Access_num, 1). 

ite([], _Access_num, Seq) -> Seq;
ite([H|T], Access_num, Seq) -> 
  case H#cacheline.access_num>Access_num of
    true -> ite(T, Access_num, Seq+1);
    _ -> Seq
  end.
  
getSsdItemNumFromLfu(SsdQ) -> getSsdItemNumFromLfu(SsdQ, 0).

getSsdItemNumFromLfu([], N) -> N;
getSsdItemNumFromLfu([H|T], N) ->
  case H#cacheline.location of
    ssd -> getSsdItemNumFromLfu(T, N+1);
    _ -> getSsdItemNumFromLfu(T, N)
  end.    
