-module(wec_cr).
-export([initCacheReplacement/6, readReq2Cache/6, writeReq2Cache/3, inQueue/4, push2CachePool/5, hitQueue/5, markShadowCache/3]).

% location may be ram, ssd, or hdd.
% type may be lir, rhir, nonrhir, undefined
-record(cacheline, {fileid=-1, access_num=1, last_access_time=-1, expireTime=-1, location=hdd, type=undefined}). 

% Algs: FIFO, LRU, LFU, MQ, LIRS, ARC     

%% -----------------------------------
%%            Init Module
%% -----------------------------------
initCacheReplacement(Alg, QoutLimit, Lifetime, DefaultLocation, HirPercentage, CacheCap) ->
  put("location", DefaultLocation), %ram, hdd or ssd
  %put("hir_percentage", HirPercentage),
  case Alg of
    fifo -> []; 
    lru -> []; 
    lfu -> [];
    mq -> wec_mq:initMQ(QoutLimit, Lifetime, 23);
    lirs -> wec_lirs:initLIRS(HirPercentage);
    arc -> wec_arc:initARC();
    larc -> wec_larc:initLARCQueue(CacheCap);
    lgb -> wec_lgb:initLGB();
    sd -> [];
    idea -> wec_idea:initIDEA();
    lru_lwc -> wec_lru_lwc:initLruLwc()
  end.


%% -----------------------------------------
%%      Cache Pool Management Module
%% -----------------------------------------
% fifo, lru, lfu: Q is a list
% mq: Q = {MQ, Qout}
% lirs: Q = {LRUStack, HIRQ}
% arc: Q = {L1, L2, T1, T2, P}

getAlg(QType) ->
  case QType of
    ram -> get("ram_alg");
    ssd -> get("ssd_alg")
  end.

% return {hit/miss, Location, SysTime, Q, EvictedItemList}
readReq2Cache(Q, QType, CachelineID, SysTime, IfSieve, RamQ) ->
  Alg = getAlg(QType),
  %io:format("readReq2Cache() start, Q=~p~n", [Q]),
  {Ret, Q1, HitItem, HitType} = inQueue(Q, CachelineID, Alg, QType),
  %io:format("    Q1 = ~p~n", [Q1]),
  case Ret of
    miss ->   % physical cache miss
      %io:format("[[physical cache miss]]~n"),
      case HitItem of
        null ->  % shadow cache miss too
          %io:format("[[all miss]]~n"),    
          case IfSieve of
            miss_sieve ->   % Sieve based on Miss Num
             Through = wec_sv:checkEnterCache(CachelineID),
              case Through of 
                true ->  % pass the sieve
                  {Q2, Evicted} = push2CachePool(Q1, QType, CachelineID, SysTime, HitType),
                  {miss, null, Q2, Evicted};
                false -> % will not enter SSD cache
                  {miss, null, Q1, []}
              end;
            time_sieve ->   % Sieve based on time, read from the tail of RamQ
              Period = get("time_period"),
              Counter = get("time_counter"),
              case Counter < Period of
                true ->
                  {miss, null, Q1, []}; 
                _ -> 
                  put("time_counter", 0), 
                  TailPercentage = get("tail_percentage"),
                  L = getQTail(RamQ, TailPercentage, get("ram_alg")),
                  {Q2, Evicts} = loopPush(L, Q1, QType, SysTime, []),
                  % Qshort = lists:map(fun(X)->
                  %     {_, Id, Acc, _, _, _, _} = X,
                  %     {Id, Acc}
                  % end, Q2),
                  % {ok, S} = file:open("../log/index.csv",[append]),
                  % wec_sd:printIndex(S, Qshort),
                  % file:close(S),
                  % io:format("Wec_cr:readReq2Cache:Before change:~p~n",[Q1]),
                  % io:format("Wec_cr:readReq2Cache:After change:~p~n",[Q2]),
                  {miss, null, Q2, Evicts}
              end;
            false -> % donot sieve, enter Q directly
              {Q2, Evicted} = push2CachePool(Q1, QType, CachelineID, SysTime, HitType),
              %io:format("  Q2 = ~p~n", [Q2]),
              {miss, null, Q2, Evicted}
          end;        
        _ -> % shadow cache hit, donot need sieve, enter Q directly
          %io:format("[[shadow cache hit]]~n"),
          {Q2, Evicted} = hitQueue(Q1, QType, HitItem, SysTime, HitType),          
          {miss, null, Q2, Evicted}
      end;                  
    hit ->      % hit
      %io:format("[[physical cache hit]]~n"),      
      {Q2, Evicted} = hitQueue(Q1, QType, HitItem, SysTime, HitType),
      {hit, HitItem#cacheline.location, Q2, Evicted}
  end.


writeReq2Cache(Q, QType, CachelineID) ->
  Alg = getAlg(QType),
  {Hit, Q2, HitItem, _} = inQueue(Q, CachelineID, Alg, null),
  if
    Alg=:=fifo ->
      Q1 = lists:keydelete(CachelineID,2,Q2);
    true->
      Q1 = Q2
  end,
  case Hit of
    hit -> 
      case QType of 
        ssd -> wec_misc:ssdEvict(HitItem);
        _ -> ok
      end,
      case Alg of
        lirs ->
          {Stack, HirQ} = Q1, 
          Stack1 = wec_lirs:stackPruning(Stack),
          {Stack1, HirQ};
        _ ->
          Q1
      end;
    _ -> 
      Q1
  end.


loopPush([], Q, _QType, _SysTime, Evicts) -> {Q, Evicts};
loopPush([H|T], Q, QType, SysTime, Evicts) ->
  CachelineID = H#cacheline.fileid,
  {Q1, Evicted} = push2CachePool(Q, QType, CachelineID, SysTime, null),
  %io:format("loopPush(), Evicted = ~p~n", [Evicted]),
  case length(Evicted) of
    0 ->
      loopPush(T, Q1, QType, SysTime, Evicts);
    _ ->
      [E] = Evicted,
      loopPush(T, Q1, QType, SysTime, [E|Evicts])
  end.


getQTail(Q, TailPercentage, Alg) ->
  case Alg of
    fifo -> lists:reverse(lists:sublist(lists:reverse(Q), wec_misc:ceil(length(Q)*TailPercentage))); 
    lru -> lists:reverse(lists:sublist(lists:reverse(Q), wec_misc:ceil(length(Q)*TailPercentage))); 
    lfu -> lists:reverse(lists:sublist(lists:reverse(Q), wec_misc:ceil(length(Q)*TailPercentage)));
    mq -> wec_mq:getMQTail(Q, TailPercentage);
    lirs -> wec_lirs:getLIRSQTail(Q, TailPercentage);
    arc -> wec_arc:getARCQTail(Q, TailPercentage)         
  end.  


getCacheCap(QType) ->
  case QType of
    ram -> get("ram_cache_cap");
    ssd -> get("ssd_cache_cap");
    null -> -1
  end.

% if hit, remove and return the item, ret {hit, Q, HitItem, miss}
% if miss, Q not changed, ret {miss, Q, null, shadow hit/miss}
inQueue(Q, CachelineID, Alg, QType) -> 
  case Alg of
    fifo -> 
      {Ret, Q1, HitItem} = wec_fifo:inFIFOQueue(Q, CachelineID),
      {Ret, Q1, HitItem, null};
    lru -> 
      {Ret, Q1, HitItem} = wec_lru:inLRUQueue(Q, CachelineID),
      {Ret, Q1, HitItem, null};
    lfu -> 
      {Ret, Q1, HitItem} = wec_lfu:inLFUQueue(Q, CachelineID),
      {Ret, Q1, HitItem, null};
    mq -> 
      {Ret, Q1, HitItem} = wec_mq:inMQ(Q, CachelineID),
      {Ret, Q1, HitItem, null};      
    lirs -> 
      wec_lirs:inLIRS(Q, CachelineID);
    arc -> 
      CacheCap = getCacheCap(QType),
      wec_arc:inARC(Q, CachelineID, CacheCap);
    larc ->
      wec_larc:inLARCQueue(Q, CachelineID);
    lru_lwc ->
      CacheCap = getCacheCap(QType),
      {Ret, Q1, HitItem} = wec_lru_lwc:inLruLwcQueue(Q, CachelineID, CacheCap),
      {Ret, Q1, HitItem, null}
    
  end.


% called when Cache Miss
% return {Q, EvcitItem} or {Q, null}
% Q脦麓卤脴脢脟list拢卢脰禄脢脟脭脷fifo, lru, lfu脢脟脢脟脪禄赂枚list拢卢脝盲脣没脟茅驴枚脧脗脢脟tuple
push2CachePool(Q, QType, CachelineID, SysTime, HitType) ->
  %io:format("in push2CachePool(), CachelineID = ~p, Q = ~p~n", [CachelineID, Q]),
  %Location = QType,
  Location = get("location"),
  Item = #cacheline{fileid=CachelineID, access_num=1, last_access_time=SysTime, location=Location},
  CacheCap = getCacheCap(QType),  
  Alg = getAlg(QType),
  case Alg of 
    fifo -> wec_fifo:addtoFIFOQueue(Q, Item, CacheCap);
    lru -> wec_lru:addtoLRUQueue(Q, Item, CacheCap);
    lfu -> wec_lfu:addtoLFUQueue(Q, Item, CacheCap);
    lirs -> wec_lirs:addtoLIRSQueue(Q, Item, HitType, CacheCap);    
    mq -> wec_mq:addtoMQ(Q, Item, SysTime, CacheCap);
    arc -> wec_arc:addtoARC(Q, Item, HitType, CacheCap);   % addtoARC1
    larc -> wec_larc:addtoLARCQueue(Q, Item, HitType, CacheCap);
    lru_lwc -> wec_lru_lwc:addtoLruLwcQueue(Q, Item, CacheCap, push)
  end.  


% called when cache hit, return NewQ
% 鲁媒脕脣FIFO拢卢脝盲脣没脣茫路篓碌脛麓娄脌铆拢卢脫娄赂脙脫毛push2CachePool脪禄脩霉拢卢脰禄脢脟路脙脦脢脢媒+1
hitQueue(Q, QType, Item, SysTime, HitType) ->
  OldAccNum = Item#cacheline.access_num,
  Item1 = Item#cacheline{access_num = OldAccNum + 1, last_access_time=SysTime}, 
  CacheCap = getCacheCap(QType), 
  Alg = getAlg(QType),
  case Alg of
    fifo -> wec_fifo:addtoFIFOQueue(Q, Item1, CacheCap);
    lru -> wec_lru:addtoLRUQueue(Q, Item1, CacheCap);
    lfu -> wec_lfu:addtoLFUQueue(Q, Item1, CacheCap);
    mq -> wec_mq:addtoMQ(Q, Item1, SysTime, CacheCap);
    lirs -> wec_lirs:addtoLIRSQueue(Q, Item1, HitType, CacheCap); 
    arc -> wec_arc:addtoARC(Q, Item1, HitType, CacheCap);  % addtoARC2
    larc -> wec_larc:addtoLARCQueue(Q, Item1, HitType, CacheCap);
    lru_lwc -> wec_lru_lwc:addtoLruLwcQueue(Q, Item, CacheCap, hit)
  end.

markShadowCache(Q, CachelineID, QType) ->
  Alg = getAlg(QType),
  case Alg of
    fifo -> Q;
    lru -> Q;
    lfu -> Q;
    mq ->
      {Mq, Qout} = Q, 
      {Mq, wec_mq:markQout(Qout, CachelineID)};
    lirs ->
      wec_lirs:markNrhir(Q, CachelineID);
    arc ->
      wec_arc:markBottom(Q, CachelineID);
    larc -> Q;
    lru_lwc ->
      Q
  end.

