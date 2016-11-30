-module(wec_larc).
-export([initLARCQueue/1, inLARCQueue/2, addtoLARCQueue/4, markLARCQ/2, getSsdItemNumFromLARC/1]).

-record(cacheline, {fileid=-1, access_num=1, last_access_time=-1, expireTime=-1, location=hdd, type=undefined}). 

%% -----------------------------------
%%            LARC Module
%% -----------------------------------
% {NQ, FQ}
%  NQ - Normal LRU Q;
%  FQ - Filter LRU Q, has a length limit.
initLARCQueue(CacheCap) ->
  put("fq_limit", wec_misc:ceil(0.1*CacheCap)),
  {[], []}.

inLARCQueue(Q, CachelineID) ->
  {NQ, FQ} = Q,
  SsdCacheCap = get("ssd_cache_cap"),
  OldLimit = get("fq_limit"),
  %io:format("CacheCap = ~p, old = ~p~n", [SsdCacheCap, OldLimit]),
  R = lists:keyfind(CachelineID, 2, NQ),
  case R of
    false -> 
      Limit = min(trunc(0.9*SsdCacheCap), trunc(OldLimit + SsdCacheCap/OldLimit)),
      put("fq_limit", Limit),
      %io:format("miss, new limit = ~p~n", [Limit]),
      R1 = lists:keyfind(CachelineID, 2, FQ),
      case R1 of
        false ->
          {miss, Q, null, null};
        _ ->
          L1 = lists:delete(R1, FQ),
          {miss, {NQ, L1}, R1, filter_hit}
      end;
    _ ->
      L = lists:delete(R, NQ),       
      Limit = max(wec_misc:ceil(0.1*SsdCacheCap), trunc(OldLimit - SsdCacheCap/(SsdCacheCap-OldLimit))),   
      put("fq_limit", Limit),
      %io:format("hit, new limit = ~p~n", [Limit]),
      {hit, {L, FQ}, R, phy_hit} 
  end.

checkFQ(FQ, LenLimit) ->
  case length(FQ) > LenLimit of
    true ->
      lists:sublist(FQ, LenLimit);
    _ ->
      FQ
  end.

% return {Q, Evicts List}
addtoLARCQueue(Q, Item, HitType, CacheCap) -> 
  {NQ, FQ} = Q,
  FQ_Limit = get("fq_limit"),
  NewFQ = checkFQ(FQ, FQ_Limit),
  case HitType of
    phy_hit ->
      {{[Item|NQ], NewFQ}, []};
    filter_hit ->
      case length(NQ) < CacheCap of 
        true ->
          {{[Item|NQ], NewFQ}, []};
        false ->
          Last = lists:last(NQ),
          {{[Item|lists:sublist(NQ, length(NQ)-1)], NewFQ}, [Last]}
      end;      
    null -> 
      %io:format("in addtoLARCQ(), FQ len = ~p, FQ_Limit = ~p~n", [length(FQ), FQ_Limit]),
      case length(NewFQ) < FQ_Limit of 
        true ->
          {{NQ, [Item|NewFQ]}, []};
        false ->
          %Last1 = lists:last(NewFQ),
          {{NQ, [Item|lists:sublist(NewFQ, length(NewFQ)-1)]}, []}  % filter queue evict, do not return Evicts
      end
  end.

markLARCQ(SsdQ, N) -> 
  {NQ, FQ} = SsdQ,
  {Left, NewNQ} = markLARCQ(NQ, N, []), 
  {Left, {NewNQ, FQ}}.
  
markLARCQ([], Left, SsdQ) -> {Left, lists:reverse(SsdQ)};
markLARCQ(Q, 0, SsdQ) -> {0, lists:reverse(SsdQ) ++ Q};
markLARCQ([H|T], Left, SsdQ) -> 
  case H#cacheline.location of
    hdd ->
      H1 = H#cacheline{location=ssd},
      markLARCQ(T, Left-1, [H1|SsdQ]);
    _ -> 
      markLARCQ(T, Left, [H|SsdQ])
  end.


getSsdItemNumFromLARC(SsdQ) -> 
  %io:format("in get SSd Item Num, SsdQ = ~p~n", [SsdQ]),
  {NQ, _FQ} = SsdQ,
  getSsdItemNumFromLARC(NQ, 0).

getSsdItemNumFromLARC([], N) -> N;
getSsdItemNumFromLARC([H|T], N) ->
  case H#cacheline.location of
    ssd -> getSsdItemNumFromLARC(T, N+1);
    _ -> getSsdItemNumFromLARC(T, N)
  end.

