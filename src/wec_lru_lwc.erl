-module(wec_lru_lwc).
-export([initLruLwc/0, initLruLwc/5, inLruLwcQueue/3, addtoLruLwcQueue/4, deleteFromLruLwcQueue/2, markLruLwcQ/2, getSsdItemNumFromLruLwc/1,
  insertToWhiteList/2, uniqueWindow/3, uniqueEachWindow/2, updateWhitelist/2, initParamaters/5, getParamaters/0]).

-record(cacheline, {fileid=-1, access_num=1, last_access_time=-1, expireTime=-1, location=hdd, type=undefined}). 

%% -----------------------------------
%%            LRU_LWC Module
%% -----------------------------------

% In this module, we implement lwc on lru.
% Each requirement comes, first check lwc's array, 
% then call corresponding functions in wec_lru.erl.


initLruLwc() ->
  % ==== code for debug ===
  %io:format("function called : initLruLwc~n", []),
  read_lru_lwc_config(),
  % TODO =============================== find out why in wec_fr calling initParamaters() cannot use in this file.
  % ===== end of debug ====

  %put("lwc_whitelist_length", 50),
  %put("lwc_window_length", 52),
  %put("lwc_window_num", 3),           % window Number
  %put("lwc_threshold_1", 3),          % threshold in 1 windows, from array to whitelist
  %put("lwc_threshold_multi", 2),      % threshold for multi windows, from array to whitelist
  %initParamaters(50, 52, 3, 3, 2),    % this parameters is also the default in flashcache_lwc
  {[], {0, []}, {0, [[]]}}.                % {LruQ, WhiteList, Array}


read_config_integer(Filename) ->
  {ok, S} = file:open(Filename, read),
  Line = io:get_line(S, ''),
  file:close(S),
  {N, _} = string:to_integer(Line),
  N.

read_lru_lwc_config() ->
  WhitelistLen = read_config_integer("0.config"),
  WindowLen = read_config_integer("1.config"),
  WindowNum = read_config_integer("2.config"),
  Thre1 = read_config_integer("3.config"),
  ThreMulti = read_config_integer("4.config"),
  initParamaters(WhitelistLen, WindowLen, WindowNum, Thre1, ThreMulti).







initLruLwc(WhitelistLen, WindowLen, WindowNum, Thre1, ThreMulti) ->  
  % ==== code for debug ===
  %io:format("function called : initLruLwc~n", []),
  % ===== end of debug ====

  initParamaters(WhitelistLen, WindowLen, WindowNum, Thre1, ThreMulti),
  {[], [], {0, [[]]}}.                        % {LruQ, WhiteList, Array}



initParamaters(WhitelistLen, WindowLen, WindowNum, Thre1, ThreMulti) ->  
  % ==== code for debug ===
  %io:format("function called : initParamaters(~p, ~p, ~p, ~p, ~p)~n", [WhitelistLen, WindowLen, WindowNum, Thre1, ThreMulti]),
  % ===== end of debug ====

  put("lwc_whitelist_length", WhitelistLen),
  put("lwc_window_length", WindowLen),
  put("lwc_window_num", WindowNum),           % window Number
  put("lwc_threshold_1", Thre1),              % threshold in 1 windows, from array to whitelist
  put("lwc_threshold_multi", ThreMulti),      % threshold for multi windows, from array to whitelist
  io:format("put ~p~n", [getParamaters()]),
  ok.
 
getParamaters() ->
  {get("lwc_whitelist_length"), get("lwc_window_length"), get("lwc_window_num"), get("lwc_threshold_1"), get("lwc_threshold_multi")}.  
  

% lookup if CachelineID is in LruQ, 
% if isRead and miss, update array and whitelist.
%  Q : LRU_LWC Paramaters
%  CachelineID
%  CacheCap: if -1, write method, do nothing
inLruLwcQueue(Q, CachelineID, CacheCap) ->
  % ==== code for debug ===
  %io:format("function called : inLruLwcQueue~n", []),
  %io:format("  parameters : ~p~n", [getParamaters()]),
  % ===== end of debug ====

  {WhitelistLen, WindowLen, WindowNum, Thre1, ThreMulti} = getParamaters(),
  {LruQ, WhiteList, Array} = Q,
  {Ret, LruQ1, HitItem} = wec_lru:inLRUQueue(LruQ, CachelineID),
  if 
    CacheCap /= -1 andalso Ret =:= miss ->
      {Count, _} = Array,
      if
        Count >= WindowLen * WindowNum ->
          WhiteList1 = updateWhitelist(WhiteList, Array),
          Array1 = {1, [[CachelineID]]};
        true ->
          WhiteList1 = WhiteList,
          Array1 = addToArray(Array, CachelineID, WindowLen)
      end;
    true ->
      WhiteList1 = WhiteList,
      Array1 = Array
  end,
  Q1 = {LruQ1, WhiteList1, Array1},
  {Ret, Q1, HitItem}.


updateWhitelist(WhiteList, Array) ->
  %{WhitelistLen, WindowLen, WindowNum, Thre1, ThreMulti} = getParamaters(),
  {_, ArrayContent} = Array,
  {ArrayContent1, WhiteList1} = uniqueEachWindow(ArrayContent, WhiteList),
  ThreMulti = get("lwc_threshold_multi"),
  {_, WhiteList2} = uniqueWindow(lists:merge(ArrayContent1), ThreMulti, WhiteList1),
  WhiteList2.



% for each windows in array, call uniqueWindow 
uniqueEachWindow(ArrayContent, WhiteList) ->
  uniqueEachWindow(ArrayContent, [], WhiteList).

uniqueEachWindow([], Uniqued, WhiteList) -> 
  {Uniqued, WhiteList};
uniqueEachWindow([H|T], Uniqued, WhiteList) ->
  Thre1 = get("lwc_threshold_1"),
  {Window, WhiteList1} = uniqueWindow(H, Thre1, WhiteList),
  uniqueEachWindow(T, [lists:reverse(Window)|Uniqued], WhiteList1).
  


% insert elements appears more the Thre1 times into whitelist,
% unique each windows's remaining elements, and return
% for example, if ArrayCount = [1,2,2,3,3,3], Thre1 = 3, 
% then insert 3 into whitlist, return [1,2] 
uniqueWindow([], _, WhiteList) -> 
  {[], WhiteList};
uniqueWindow(Window, Threshold, WhiteList) ->
  [H|T] = lists:sort(Window),
  uniqueWindow(T, Threshold, [], H, 1, WhiteList).

uniqueWindow([], Threshold, Uniqued, Last, LastCount, WhiteList) -> 
  if
    LastCount >= Threshold ->
      WhiteList1 = insertToWhiteList(Last, WhiteList),
      Uniqued1 = Uniqued;
    true ->
      WhiteList1 = WhiteList,
      Uniqued1 = [Last|Uniqued]
  end,
  {Uniqued1, WhiteList1};
uniqueWindow([H|T], Threshold, Uniqued, Last, LastCount, WhiteList) ->
  if
    H =:= Last ->
      uniqueWindow(T, Threshold, Uniqued, Last, LastCount+1, WhiteList);
    true ->
      if
        LastCount >= Threshold ->
          WhiteList1 = insertToWhiteList(Last, WhiteList),
          uniqueWindow(T, Threshold, Uniqued, H, 1, WhiteList1);
        true ->
          uniqueWindow(T, Threshold, [Last|Uniqued], H, 1, WhiteList)
      end
  end.




insertToWhiteList(CachelineID, WhiteList) ->
  % ==== code for debug ===
  %io:format("function called : insertToWhiteList~n", []),
  % ===== end of debug ====

  %io:format("==== insertToWhiteList: ~p, ~p~n", [CachelineID, WhiteList]),
  {N, List} = WhiteList,
  WhitelistLen = get("lwc_whitelist_length"),
  case lists:member(CachelineID, List) of
    true ->
      WhiteList;
    _ ->
      if 
        length(WhiteList) >= WhitelistLen ->
          NewList = lists:sublist([CachelineID|List], WhitelistLen),
          {N+1, NewList};
        true ->
          {N+1, [CachelineID|List]}
      end
  end.






addToArray(Array, CachelineID, WindowLen) ->
  % ==== code for debug ===
  %io:format("function called : addToArray(,,~p)~n", [WindowLen]),
  % ===== end of debug ====

  {Count, ArrayContent} = Array,
  [H|T] = ArrayContent,
  if 
    length(H) >= WindowLen ->
      ArrayContent1 = [[CachelineID]|ArrayContent];
    true ->
      ArrayContent1 = [[CachelineID|H]|T]
  end,
  {Count + 1, ArrayContent1}.


% when AddType is hit, add directly;
% when AddType is push, and CacheCap is full, check the whitelist first
% TODO : count white list hit rate
addtoLruLwcQueue(Q, Item, CacheCap, AddType) -> 
  % ==== code for debug ===
  %io:format("function called : addtoLruLwcQueue(~p,~p,~p,~p)~n", [Q, Item, CacheCap, AddType]),
  % ===== end of debug ====

  {LruQ, WhiteList, Array} = Q,
  case AddType of
    hit ->
      {LruQ1, Evicted} = wec_lru:addtoLRUQueue(LruQ, Item, CacheCap),
      {{LruQ1, WhiteList, Array}, Evicted};
    _ ->
      CachelineID = Item#cacheline.fileid,
      case length(LruQ) < CacheCap of
        true ->
              {LruQ1, Evicted} = wec_lru:addtoLRUQueue(LruQ, Item, CacheCap),
              {{LruQ1, WhiteList, Array}, Evicted};
        false ->
          {_, List} = WhiteList,
          case lists:member(CachelineID, List) of
            true ->
              {LruQ1, Evicted} = wec_lru:addtoLRUQueue(LruQ, Item, CacheCap),
              {{LruQ1, WhiteList, Array}, Evicted};
            _ ->
              {Q, []}
          end
      end
  end.

deleteFromLruLwcQueue(Q, DelItem) -> 
  {LruQ, WhiteList, Array} = Q,
  LruQ1 = wec_lru:deleteFromLruQueue(LruQ, DelItem),
  {LruQ1, WhiteList, Array}.  
  
markLruLwcQ(SsdQ, N) ->
  {LruQ, WhiteList, Array} = SsdQ,
  {Left, SsdQ1} = wec_lru:markLruQ(LruQ, N),
  {Left, {SsdQ1, WhiteList, Array}}.


getSsdItemNumFromLruLwc(SsdQ) -> 
  {LruQ, _, _} = SsdQ,
  wec_lru:getSsdItemNumFromLru(LruQ).

