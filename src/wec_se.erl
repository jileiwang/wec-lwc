-module(wec_se).
-export([initEB/2, push2EvictBuffer/3, smartEvictProcess/3, delFromEvictBuffer/2, inEvictBuffer/2]).

% location may be ram, ssd, or hdd.
% type may be lir, rhir, nonrhir, undefined
-record(cacheline, {fileid=-1, access_num=1, last_access_time=-1, expireTime=-1, location=hdd, type=undefined}). 

initEB(EBType, IdleTimeThr) ->
  put("eb_type", EBType),
  put("idle_time_thr", IdleTimeThr).


% only Item located in SSD will be added to EB
push2EvictBuffer(EB, Item, SsdQ) ->
  EBType = get("eb_type"),
  %io:format("in push2EvictBuffer(), Item=~p~n", [Item]),
  case EBType of
    no ->
      wec_misc:ssdEvict(Item),
      % wec_misc:calHit([Item#cacheline.fileid]),
      % 如果Item存在于shadow cache中，还需要将其中的item的Location改为hdd
      SsdQ1 = wec_cr:markShadowCache(SsdQ, Item#cacheline.fileid, ssd),
      {[], SsdQ1};
    _ -> 
      case Item#cacheline.location of
        ssd ->
          %io:format("in push2EvictBuffer(), Item = ~p~n", [Item]),
          wec_misc:ebWrite(), 
          {[Item|EB], SsdQ};
        _ -> {EB, SsdQ}
      end
  end.


% temp impl
% return new {EB, SsdQ}
smartEvictProcess(EB, SsdQ, SysTime) ->
  %io:format("in smartEvictProcess(), EB= ~p, SsdQ=~p~n", [EB, SsdQ]),
  % Step#1 Judge Evict
  Thr = get("idle_time_thr"),
  {EB1, SsdQ1} = processAllInEB(EB, SsdQ, SysTime, Thr),

  % Step#2 Fully fill SSD  
  SsdCap = get("ssd_cache_cap"),
  %io:format("getSsdItemNum(~p)", [SsdQ1]),
  SsdItemNum = getSsdItemNum(SsdQ1) + length(EB1),
  case SsdItemNum < SsdCap of
    true -> 
      {Left, SsdQ2} = markSsdQ(SsdQ1, SsdCap-SsdItemNum),
      %io:format("ssd cap = ~p, ssd item num = ~p, left = ~p, ssdadd num = ~p~n", [SsdCap, SsdItemNum, Left, SsdCap-SsdItemNum-Left]),
      %io:format("      SsdQ2 = ~p, cap = ~p, SsdItemNum = ~p, Left=~p~n", [SsdQ2, SsdCap, SsdItemNum, Left]),
      wec_misc:ssdAddinto(SsdCap-SsdItemNum-Left),
      {EB1, SsdQ2};
    false ->
      {EB1, SsdQ1}
  end.

% return {EB1, SsdQ1} 
processAllInEB(EB, SsdQ, SysTime, Thr) -> 
  loopEB(EB, SysTime, Thr, SsdQ, []).
  
loopEB([], _SysTime, _Thr, SsdQ, NewEB) -> {lists:reverse(NewEB), SsdQ};
loopEB([H|T], SysTime, Thr, SsdQ, NewEB) -> 
  case SysTime-H#cacheline.last_access_time >= Thr of
    true -> % evict H
      %io:format("SysTime:~p, lastAccessTime:~p, Thr:~p, EB evicted ~p~n", [SysTime, H#cacheline.last_access_time, Thr,H]),
      wec_misc:ssdEvict(H),

      % 如果决定evict某一项，还要调用wec_cr:markShadowCache()将shadow cache中的对应项的location改为hdd
      SsdQ1 = wec_cr:markShadowCache(SsdQ, H#cacheline.fileid, ssd),
      loopEB(T, SysTime, Thr, SsdQ1, NewEB);
    _ ->  % not evict H
      loopEB(T, SysTime, Thr, SsdQ, [H|NewEB])
  end.


getSsdItemNum(SsdQ) ->
  Alg = get("ssd_alg"),
  case Alg of 
    fifo -> wec_fifo:getSsdItemNumFromFifo(SsdQ);
    lru -> wec_lru:getSsdItemNumFromLru(SsdQ);
    lfu -> wec_lfu:getSsdItemNumFromLfu(SsdQ);
    mq -> wec_mq:getSsdItemNumFromMq(SsdQ);
    lirs -> wec_lirs:getSsdItemNumFromLirs(SsdQ);
    arc -> wec_arc:getSsdItemNumFromArc(SsdQ);
    larc -> wec_larc:getSsdItemNumFromLARC(SsdQ);
    lru_lwc -> wec_lru_lwc:getSsdItemNumFromLruLwc(SsdQ)
  end.  


% mark the first N hdd items as ssd item
markSsdQ(SsdQ, N) ->
  %io:format("in markSsdQ(), SSDQ = ~p, N = ~p~n", [SsdQ, N]),
  Alg = get("ssd_alg"),
  case Alg of 
    fifo -> wec_fifo:markFifoQ(SsdQ, N);
    lru -> wec_lru:markLruQ(SsdQ, N);
    lfu -> wec_lfu:markLfuQ(SsdQ, N);
    mq -> wec_mq:markMqQ(SsdQ, N);
    lirs -> wec_lirs:markLirsQ(SsdQ, N);
    arc -> wec_arc:markArcQ(SsdQ, N);
    larc -> wec_larc:markLARCQ(SsdQ, N);
    lru_lwc -> wec_lru_lwc:markLruLwcQ(SsdQ, N)
  end.
  
  
% called when WRITE arrives
delFromEvictBuffer(EB, CachelineID) ->
  Item = lists:keyfind(CachelineID, 2, EB),
  case Item of
    false -> EB;
    _ ->
      wec_misc:ssdEvict(Item), 
      lists:delete(Item, EB)       
  end.
  
  
% called when READ arrives
% if CachelineID is in EB, del it from EB and return it. (it will be put at the head of SSD Cache Queue)
inEvictBuffer(EB, CachelineID) ->
  Item = lists:keyfind(CachelineID, 2, EB),
  case Item of
    false -> {EB, null};
    _ -> {lists:delete(Item, EB), Item}       
  end.

