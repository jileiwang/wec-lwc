-module(test_wec_lru_lwc).
-export([test/0]).

-record(cacheline, {fileid=-1, access_num=1, last_access_time=-1, expireTime=-1, location=hdd, type=undefined}). 

test() ->
  %testInit(),
  %testInQueue(),
  %testInQueue2().
  testInQueue3().
  %testInsertToWhiteList().
  %testUniqueWindow().
  %testUpdateWhiteList().
  %testInQueue4().

testInit() ->
  wec_lru_lwc:initLruLwc().

testInQueue() ->
  Q = wec_lru_lwc:initLruLwc(),
  wec_lru_lwc:inLruLwcQueue(Q, 1, 1).

testInQueue2() ->
  Q = wec_lru_lwc:initLruLwc(5,5,3,3,2),
  inQueueN(Q, 15).
  
inQueueN(Q, 0) -> Q;
inQueueN(Q, N) ->
  {Ret, Q1, HitItem} = wec_lru_lwc:inLruLwcQueue(Q, N, 1),
  io:format("N = ~p, Q = ~p~n", [N, Q1]), 
  inQueueN(Q1, N-1).

testInQueue3() ->
  Q = wec_lru_lwc:initLruLwc(5,5,3,3,2),
  Q1 = inQueueList(Q, [1,1,1,2,2,3,3,3,2,4,4,5,6,7,8]),
  io:format("Q = ~p~n", [Q1]), 
  inQueueList(Q1, [1]).

inQueueList(Q, []) -> Q;
inQueueList(Q, [H|T]) ->
  {Ret, Q1, HitItem} = wec_lru_lwc:inLruLwcQueue(Q, H, 1),
  inQueueList(Q1, T).



testInsertToWhiteList() ->
  put("lwc_whitelist_length", 4),
  wec_lru_lwc:insertToWhiteList(4, [1,2,3,4]).

testUniqueWindow() ->
  put("lwc_whitelist_length", 4),
  wec_lru_lwc:uniqueWindow([1,1,2,2,3,3,3,4,4], 3, []).


testUpdateWhiteList() ->
  Q = wec_lru_lwc:initLruLwc(5,5,3,3,2),
  W1 = wec_lru_lwc:updateWhitelist([1], {0, [[1,1,1,2,3], [4,4,5,4,2], [6,7,8,9,0]]}),
  io:format("~p~n", [W1]).

testInQueue4() ->
  Q = wec_lru_lwc:initLruLwc(5,5,3,3,2).
