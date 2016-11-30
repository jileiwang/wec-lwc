-module(wec_mq).
-export([initMQ/3, inMQ/2, addtoMQ/4, deleteFromMQ/2, getMQTail/2, markMqQ/2, getSsdItemNumFromMq/1, markQout/2, printQ/2, printMQ/2]).

-record(cacheline, {fileid=-1, access_num=1, last_access_time=-1, expireTime=-1, location=hdd, type=undefined}).

%% -----------------------------------
%%             MQ Module
%% -----------------------------------

initMQ(QoutLimit, Lifetime, N) -> 
  put("qout_limit", QoutLimit),
  put("lifetime", Lifetime),
  MQ = initMQ(N, []),
  % io:format("{MQ,[]} = ~p~n",[{MQ,[]}]),
  {MQ, []}.

initMQ(0, List) -> List;
initMQ(N, List) -> initMQ(N-1, [[]|List]).


printMQ([], _N) -> ok;
printMQ([H|T], N) ->
  io:format("Q[~p]~n", [N]),
  printQ(H, 0),
  printMQ(T, N+1).

printQ([], _N) -> ok;
printQ([H|T], N) ->
  io:format("    [~p] (ID:~p, AN:~p, ET:~p) ~n", [N, H#cacheline.fileid, H#cacheline.access_num, H#cacheline.expireTime]),
  printQ(T, N+1).
  

inMQ(Q, CachelineID) ->
  {MQ, Qout} = Q,
  {Ret, HitItem, NewMQ} = blockInMQ(CachelineID, MQ, []),    
  case Ret of
    true ->        
      {hit, {NewMQ, Qout}, HitItem};
    false ->     
      {Ret1, Block, Qout1} = blockInQ(CachelineID, Qout, []),
      case Ret1 of 
        true ->  % block in Qout
          {miss, {MQ, Qout1}, Block};
        false ->  % block in NOT Qout
          {miss, Q, null}
      end      
  end.  


addtoMQ(Q, Item, SysTime, CacheCap) ->
  {MQ, Qout} = Q, 
  QoutLimit = get("qout_limit"),
  LifeTime = get("lifetime"),   
  {MQ1, Qout1, Evicted} = evictBlock(MQ, Qout, CacheCap, QoutLimit),
  case Evicted of
    null ->
      {modifyBlock(Item, MQ1, Qout1, SysTime, QoutLimit, LifeTime), []};
    _ ->
      {modifyBlock(Item, MQ1, Qout1, SysTime, QoutLimit, LifeTime), [Evicted]}
  end. 
  


% for write reqs
% check if cachelineID is in MQ or Qout, if in, delete it.
deleteFromMQ(Q, DelItem) ->
  {MQ, Qout} = Q,
  CachelineID = DelItem#cacheline.fileid,
  {_, _Evicted1, NewMQ} = blockInMQ(CachelineID, MQ, []),
  {_, _Evicted2, NewQout} = blockInQ(CachelineID, Qout, []),
  {NewMQ, NewQout}.

 
getMQTail(Q, TailPercentage) ->
  %io:format("Q=~p~n", [Q]),
  {MQ, _Qout} = Q,
  Num = wec_misc:ceil(lengthOfMQ(MQ, 0)*TailPercentage),
  getEnoughMQTail(lists:reverse(MQ), Num, []).

getEnoughMQTail(_L, 0, Result) -> Result;
getEnoughMQTail([], _Left, Result) -> Result;
getEnoughMQTail([H|T], Left, Result) -> 
  case length(H) > Left of
    true ->
      lists:sublist(lists:reverse(H), Left) ++ Result;
    false ->
      getEnoughMQTail(T, Left-length(H), H ++ Result)
  end.
  

% reinsert Block into MQ
% ret: {SysTime, MQ, Qout}  
modifyBlock(Block, MQ, Qout, SysTime, QoutLimit, LifeTime) ->
  Block1 = Block#cacheline{expireTime = SysTime + LifeTime},
  QueueNum = getQueueNum(Block1#cacheline.access_num),
  NewMQ = reInsertBlock(Block1, QueueNum, MQ, []),
  %io:format("Blk:~p, Q-num:~p~n", [Block1, QueueNum]),
  adjust(NewMQ, Qout, SysTime, QoutLimit, LifeTime). 
  
  
% func: insert Block to No. QueueNum (start from 0) queue in MQ
% para: (block, queueNum, MQ, NewMQ)
% ret:  NewMQ                                      
reInsertBlock(_Block, _QueueNum, [], _NewMQ) -> error;   % Ref may be too high   
reInsertBlock(Block, QueueNum, [H|T], NewMQ) ->
  case QueueNum =:= 0 of
    false ->
      reInsertBlock(Block, QueueNum-1, T, [H|NewMQ]);    
    true ->
      NewH = [Block|H],
      lists:reverse([NewH|NewMQ]) ++ T
  end.

getQueueNum(Ref) -> trunc(math:log10(Ref)/math:log10(2)).  

  
% find if FileID is in MQ; if yes, remove and return it
% para: (FileID, MQ, NewMQ)
% return {true/false, Evicted Item, MQ} 
blockInMQ(_FileID, [], NewMQ) -> {false, null, lists:reverse(NewMQ)}; 
blockInMQ(FileID, [H|T], NewMQ) ->
  {Ret, Item, Q} = blockInQ(FileID, H, []),
  case Ret of 
    false -> blockInMQ(FileID, T, [Q|NewMQ]);
    true -> {true, Item, lists:reverse([Q|NewMQ]) ++ T}
  end.

% find if FileID is in Q; if yes, remove and return it
% return {true/false, Evicted Item, Q} 
blockInQ(_FileID, [], NewQ) -> {false, null, lists:reverse(NewQ)};
blockInQ(FileID, [H|T], NewQ) ->
  if 
    FileID =:= H#cacheline.fileid -> {true, H, lists:reverse(NewQ) ++ T};
    true -> blockInQ(FileID, T, [H|NewQ])
  end. 


% ret: {MQ, Qout, EvictedItem}
evictBlock(MQs, Qout, CacheSizeLimit, QoutLimit) ->  
  case CacheSizeLimit > lengthOfMQ(MQs, 0) of
    true -> % cache not full
      {MQs, Qout, null};
    _ -> % cache full, need to evict
      {NewMQs, Evicted} = evictFirstInMQ(MQs, []),
      NewQout = qoutReplace(Qout, QoutLimit, Evicted),
      {NewMQs, NewQout, Evicted}
  end.

qoutReplace(Qout, QoutLimit, TimeoutItem) ->
  case TimeoutItem#cacheline.fileid < 0 of
    true -> Qout;
    _ ->
      case length(Qout) =:= QoutLimit of
        true -> 
          [_|T] = lists:reverse(Qout),
          [TimeoutItem|lists:reverse(T)];
        _ ->
          [TimeoutItem|Qout]
      end
  end.

lengthOfMQ([], Sum) -> Sum;
lengthOfMQ([H|T], Sum) -> lengthOfMQ(T, Sum+length(H)).      

evictFirstInMQ([], _NewMQ) -> error;
evictFirstInMQ([H|T], NewMQ) -> 
  case length(H) =:= 0 of
    true -> evictFirstInMQ(T, [H|NewMQ]); % blank Q, go on
    _ -> % Q not null, evict tail 
      [Evicted|Others] = lists:reverse(H),
      Crt = lists:reverse(Others),
      {lists:reverse([Crt|NewMQ]) ++ T, Evicted}  
  end.


% MQs - [], 每一个元素是一个[], 低级别的（0号Queue）在前面，高级别的在后面
% ret: {SysTime, MQ, Qout}
adjust(MQs, Qout, SysTime, QoutLimit, LifeTime) ->
  NullTimeout = #cacheline{},
  {NewMQs, TimeoutItem} = processQs(lists:reverse(MQs), [], NullTimeout, SysTime, LifeTime),
  NewQout = qoutReplace(Qout, QoutLimit, TimeoutItem),
  {NewMQs, NewQout}.


% process all the queues in MQ
% MQ中最低级别的Q应该不会timeout
% return {NewMQs, TimeoutItem}
processQs([], NewMQs, TimeoutItem, _SysTime, _LifeTime) -> {NewMQs, TimeoutItem};
processQs([H], NewMQs, TimeoutItem, _SysTime, LifeTime) ->
  H1 = addTimeoutItem(H, TimeoutItem, LifeTime), 
  {[H1|NewMQs], #cacheline{}};
processQs([H|T], NewMQs, TimeoutItem, SysTime, LifeTime) ->
  {NewQ, TimeoutItem1} = processQ(SysTime, H, TimeoutItem, LifeTime),
  processQs(T, [NewQ|NewMQs], TimeoutItem1, SysTime, LifeTime).

% paras: (SysTime, one queue in MQ, timeout time evicted from higher level queue, Lifetime)
% ops: (1) may add TimeoutItem to the head of Q;
%      (2) may evict the tail of Q (timeout) and return
% return {NewQ, TimeoutItem} 
processQ(SysTime, Q, TimeoutItem, LifeTime) ->
  case Q of 
    [] -> 
      {addTimeoutItem(Q, TimeoutItem, LifeTime), #cacheline{}};
    _ ->
      NewQ = addTimeoutItem(Q, TimeoutItem, LifeTime),
      [Tail|T] = lists:reverse(NewQ),
      ExpT = Tail#cacheline.expireTime,
      case ExpT > SysTime of
        true -> {NewQ, #cacheline{}};       % there's no timeout item (only need to check Tail)  
        _ -> {lists:reverse(T), Tail}   % evict timeoutItem
      end
  end.

addTimeoutItem(Q, TimeoutItem, LifeTime) ->
  if 
    TimeoutItem#cacheline.fileid >= 0 -> 
      TimeoutItem1 = TimeoutItem#cacheline{expireTime = TimeoutItem#cacheline.expireTime + LifeTime},
      [TimeoutItem1|Q]; 
    true -> Q
  end.


markMqQ(SsdQ, N) ->
  {Mq, Qout} = SsdQ,  
  {Left, NewMq} = markMqQ(Mq, N, []),
  {Left, {NewMq, Qout}}.

markMqQ([], Left, NewQs) -> {Left, lists:reverse(NewQs)};
markMqQ(Qs, 0, NewQs) -> {0, lists:reverse(NewQs) ++ Qs};
markMqQ([H|T], Left, NewQs) -> 
  {Left1, Q1} = markOneQ(H, Left, []),
  markMqQ(T, Left1, [Q1|NewQs]).
  
markOneQ([], Left, NewQ) -> {Left, lists:reverse(NewQ)};
markOneQ(Q, 0, NewQ) -> {0, lists:reverse(NewQ) ++ Q};
markOneQ([H|T], Left, NewQ) -> 
  case H#cacheline.location of
    hdd ->
      H1 = H#cacheline{location=ssd},
      markOneQ(T, Left-1, [H1|NewQ]);
    _ -> 
      markOneQ(T, Left, [H|NewQ])
  end.

% change one item's location to hdd
markQout(Qout, CachelineID) -> markQout(Qout, CachelineID, []).
  
markQout([], _CachelineID, Qout) -> lists:reverse(Qout);
markQout([H|T], CachelineID, Qout) ->
  case H#cacheline.fileid of
    CachelineID ->
      H1 = H#cacheline{location=hdd}, 
      lists:reverse([H1|Qout]) ++ T;
    _ ->
      markQout(T, CachelineID, [H|Qout])
  end.


getSsdItemNumFromMq(Qs) ->
  {Mq, _Qout} = Qs, 
  getSsdItemNumFromMq(Mq, 0).

getSsdItemNumFromMq([], Num) -> Num; 
getSsdItemNumFromMq([H|T], Num) ->
  N = getSsdItemNumFromOneQ(H, 0),
  getSsdItemNumFromMq(T, Num+N).

getSsdItemNumFromOneQ([], N) -> N;
getSsdItemNumFromOneQ([H|T], N) ->
  case H#cacheline.location of
    ssd -> getSsdItemNumFromOneQ(T, N+1);
    _ -> getSsdItemNumFromOneQ(T, N)
  end.       
