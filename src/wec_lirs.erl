-module(wec_lirs).
-export([initLIRS/1, inLIRS/2, addtoLIRSQueue/4, deleteFromLIRS/2, getLIRSQTail/2, markLirsQ/2, getSsdItemNumFromLirs/1, markNrhir/2, stackPruning/1]).

% type may be lir, rhir, nonrhir, undefined
-record(cacheline, {fileid=-1, access_num=1, last_access_time=-1, expireTime=-1, location=hdd, type=undefined}).

%% -----------------------------------
%%            LIRS Module
%% -----------------------------------
initLIRS(HirPercentage) ->
  put("hir_percentage", HirPercentage),
  {[], []}.

% HitType may be lir, rhir, nonrhir (in Stack), hirq or miss
inLIRS(Q, CachelineID) ->
  {Stack, HirQ} = Q, 
  R = lists:keyfind(CachelineID, 2, Stack),
  case R of
    false ->
      R1 = lists:keyfind(CachelineID, 2, HirQ),
      case R1 of
        false ->
          {miss, Q, null, miss};
        _ ->
          HirQ1 = lists:delete(R1, HirQ),
          {hit, {Stack, HirQ1}, R1, hirq}
      end; 
    _ ->
      L = lists:delete(R, Stack),          
      case R#cacheline.type of 
        lir -> 
          {hit, {L, HirQ}, R, lir};
        rhir ->
          {hit, {L, HirQ}, R, rhir};
        nonrhir ->
          {miss, {L, HirQ}, R, nonrhir}
      end
  end.

% LRU Queue hit, item may be lir, rhir, or nonrhir
%     when it is lir, item is still lir, and promote to the head of LRU Queue;
%     when it is rhir, item->LIR, del item from HirQ, promote item to head of LRU Queue,
%                       last LIR (Y) -> RHIR, add Y to HirQ, stackPruning
%     when X is nonrhir, X->LIR, promote X to LRU Q head; LRU Q's last LIR (Y) -> RHIR,
%                       Y->HirQ; StackPruning  
% LRU Queue miss, HirQ may hit or not.
%     when HirQ hit, promote it the the head of HirQ (last one to be evicted);
%     when HirQ not hit, i.e. ALL MISS, item becomes rhir, and add to the tail of HirQ.

% when LIR is not full, all added item is lir
addtoLIRSQueue(Q, Item, HitType, CacheCap) ->
  {Stack, HirQ} = Q,   
  HirPercentage = get("hir_percentage"),
  HirSize = round(HirPercentage*CacheCap + 0.499999), 
  LirSize = CacheCap-HirSize,
  %io:format("in addtoLIRSQueue(), HitType = ~p, LirSize = ~p, length(Stack)=~p~n", [HitType, LirSize, length(Stack)]),
  %io:format("Stack = ~p~n", [Stack]),
  if
    length(Stack) < LirSize ->   
      Item1 = Item#cacheline{type=lir},
      Stack1 = stackPruning([Item1|Stack]),
      {{Stack1, HirQ}, []};   
    true ->
      ActualLirNum = getLirNum(Stack),
      %io:format("ActualLirNum = ~p~n", [ActualLirNum]),
      case ActualLirNum < LirSize of
        true ->
          Item1 = Item#cacheline{type=lir},
          Stack1 = stackPruning([Item1|Stack]),
          {{Stack1, HirQ}, []};   
        _ ->   
          case HitType of
            lir ->
              Stack1 = stackPruning([Item|Stack]),
              {{Stack1, HirQ}, []};        
            rhir ->
              Item1 = Item#cacheline{type = lir},
              Stack1 = [Item1|Stack],
              HirQ1 = delFromHirQ(HirQ, Item1#cacheline.fileid),
              case getLirNum(Stack) < LirSize of
                true -> 
                  {{Stack1, HirQ1}, []};
                false ->
                  {Stack2, LastLIR} = setLastLIRasRHIR(Stack1),
                  {Stack3, HirQ2, Evicted} = hirQReplace(HirQ1, LastLIR, Stack2, CacheCap),
                  Stack4 = stackPruning(Stack3),
                  {{Stack4, HirQ2}, Evicted}
              end;        
            nonrhir ->
              Item1 = Item#cacheline{type = lir},
              Stack1 = [Item1|Stack],
              case getLirNum(Stack) < LirSize of
                true -> 
                  {{Stack1, HirQ}, []};
                false ->
                  {Stack2, LastLIR} = setLastLIRasRHIR(Stack1),
                  {Stack3, HirQ1, Evicted} = hirQReplace(HirQ, LastLIR, Stack2, CacheCap),
                  Stack4 = stackPruning(Stack3),
                  {{Stack4, HirQ1}, Evicted}
              end;        
            hirq ->  % Stack miss, but HirQ hit, type remains "rhir"
              HirQ1 = hirQHit(HirQ, Item),
              {{[Item|Stack], HirQ1}, []};        
            miss ->  % All Miss, evict
              %io:format("   all miss, evict, call hirQReplace()~n"),
              Item1 = Item#cacheline{type = rhir},
              {Stack1, HirQ1, Evicted} = hirQReplace(HirQ, Item1, Stack, CacheCap),
              {{[Item1|Stack1], HirQ1}, Evicted}        
          end
      end
  end.
  

deleteFromLIRS(Q, DelItem) ->     
  {Stack, HirQ} = Q,
  Stack1 = lists:delete(DelItem, Stack),
  HirQ1 = lists:delete(DelItem, HirQ),
  {Stack1, HirQ1}.


getLIRSQTail(Q, TailPercentage) ->
  {Stack, HirQ} = Q,
  Num = wec_misc:ceil((getLirNum(Stack)+length(HirQ))*TailPercentage), 
  case length(HirQ) >= Num of
    true ->
      lists:sublist(lists:reverse(HirQ), Num);
    false ->
      HirQ ++ getStackTail(lists:reverse(Stack), Num-length(HirQ), [])
  end.

getStackTail(_L, 0, Result) -> Result;
getStackTail([], _Left, Result) -> Result;
getStackTail([H|T], Left, Result) -> 
  case H#cacheline.type of
    lir ->
      getStackTail(T, Left-1, [H|Result]);      
    _ ->
      getStackTail(T, Left, Result)
  end.


setLastLIRasRHIR(Stack) ->
  RStack = lists:reverse(Stack),
  LastLIR = getFirstLIR(RStack),
  Ite = getListIte(Stack, LastLIR#cacheline.fileid),
  NewLastLIR = LastLIR#cacheline{type=rhir},
  Stack1 = lists:sublist(Stack, Ite-1) ++ [NewLastLIR] ++ lists:sublist(Stack, Ite+1, length(Stack)),
  {Stack1, NewLastLIR}.

getFirstLIR([]) -> error;
getFirstLIR([H|T]) ->  
  case H#cacheline.type =:= lir of
    true -> H;
    _ -> getFirstLIR(T)
  end.
    
delFromHirQ(HirQ, FileID) ->
  R = lists:keyfind(FileID, 2, HirQ),
  case R of
    false -> HirQ;
    _ -> lists:delete(R, HirQ)
  end.

% promote Item the head of HirQ (will be evict latest)
hirQHit(HirQ, Item) ->
  Q1 = lists:delete(Item, HirQ),
  [Item|Q1].

% may evict the tail item of HirQ
hirQReplace(HirQ, NewItem, Stack, CacheCap) ->
  HirPercentage = get("hir_percentage"),
  HirSize = round(HirPercentage*CacheCap + 0.49999),
  case length(HirQ) < HirSize of
    true -> 
      {Stack, [NewItem|HirQ], []}; 
    false ->
      Last = lists:last(HirQ),
      R = lists:keyfind(Last#cacheline.fileid, 2, Stack),
      case R of
        false -> 
          {Stack, [NewItem|lists:sublist(HirQ, length(HirQ)-1)], [Last]};
        _ -> % set R as nonrhir in Stack 
          Ite = getListIte(Stack, R#cacheline.fileid),
          R1 = R#cacheline{type = nonrhir},
          Stack1 = lists:sublist(Stack, Ite-1) ++ [R1] ++ lists:sublist(Stack, Ite+1, length(Stack)),  
          {Stack1, [NewItem|lists:sublist(HirQ, length(HirQ)-1)], [Last]}          
      end
  end. 

getListIte(L, FileID) -> getListIte(L, FileID, 1).
  
getListIte([], _FileID, _Ite) -> error;
getListIte([H|T], FileID, Ite) ->
  case H#cacheline.fileid =:= FileID of
    true -> Ite;
    _ -> getListIte(T, FileID, Ite+1)
  end.

% to make sure the tail of LRU Queue is LIR, del all the HIRs until a LIR.
stackPruning(Stack) -> lists:reverse(stackPruning1(lists:reverse(Stack))). 

stackPruning1([]) -> [];
stackPruning1([H|T]) ->    
  case H#cacheline.type =:= lir of
    false -> stackPruning1(T); 
    true -> [H|T]
  end.

getLirNum(Stack) -> getLirNum(Stack, 0).

getLirNum([], Num) -> Num;
getLirNum([H|T], Num) -> 
  case H#cacheline.type of
    lir -> getLirNum(T, Num+1);
    _ -> getLirNum(T, Num)
  end.   


markLirsQ(SsdQ, N) ->
  {Stack, HirQ} = SsdQ,
  {_Left, ExceptLir, Stack1} = markStack(Stack, N, N, []),
  %io:format("markLirsQ, after markStack(), Left = ~p, ExceptLir = ~p~n", [Left, ExceptLir]),
  {Left1, HirQ1} = markHirQ(HirQ, ExceptLir, []),
  {Left1, {Stack1, HirQ1}}.
 
markStack([], Left, ExceptLir, Q) -> {Left, ExceptLir, lists:reverse(Q)};
markStack(Stack, 0, ExceptLir, Q) -> {0, ExceptLir, lists:reverse(Q) ++ Stack};
markStack([H|T], Left, ExceptLir, Q) ->
  case H#cacheline.location of
    hdd ->
      case H#cacheline.type of
        lir ->
          H1 = H#cacheline{location=ssd},
          {Left-1, ExceptLir-1, lists:reverse([H1|Q]) ++ T};        
        rhir ->
          H1 = H#cacheline{location=ssd},
          {Left-1, ExceptLir, lists:reverse([H1|Q]) ++ T};        
        _ ->
          markStack(T, Left, ExceptLir, [H|Q])
      end;    
    _ ->
      markStack(T, Left, ExceptLir, [H|Q])
  end.

markHirQ([], Left, Q) -> {Left, lists:reverse(Q)};
markHirQ(HirQ, 0, Q) -> {0, lists:reverse(Q) ++ HirQ};
markHirQ([H|T], Left, Q) -> 
  C1 = H#cacheline.type =:= rhir,
  C2 = H#cacheline.location =:= hdd, 
  case C1 andalso C2 of
    true ->
      H1 = H#cacheline{location=ssd},
      {Left-1, lists:reverse([H1|Q]) ++ T};
    _ -> 
      markHirQ(T, Left, [H|Q])
  end.
  
% mark the item in shadow cache (non-rhir item in Stack)'location to hdd
markNrhir(Q, CachelineID) ->
  {Stack, HirQ} = Q,
  Stack1 = markNrhir(Stack, CachelineID, []),
  {Stack1, HirQ}.
  
markNrhir([], _CachelineID, Stack) -> lists:reverse(Stack);
markNrhir([H|T], CachelineID, Stack) ->
  case H#cacheline.type of
    nonrhir ->
      case H#cacheline.fileid of
        CachelineID ->
          H1 = H#cacheline{location=hdd}, 
          lists:reverse([H1|Stack]) ++ T;
        _ ->
          markNrhir(T, CachelineID, [H|Stack])
      end;
    _ ->
      markNrhir(T, CachelineID, [H|Stack])
  end.        
  

getSsdItemNumFromLirs(SsdQ) -> 
  {Stack, HirQ} = SsdQ,
  N1 = getSsdItemNumFromStack(Stack, 0),
  N2 = getSsdItemNumFromHirQ(HirQ, 0),
  N1+N2.


getSsdItemNumFromStack([], N) -> N;
getSsdItemNumFromStack([H|T], N) ->
  case H#cacheline.type of 
    lir -> 
      case H#cacheline.location of
        ssd -> getSsdItemNumFromStack(T, N+1);
        _ -> getSsdItemNumFromStack(T, N)
      end;
    _ ->
      getSsdItemNumFromStack(T, N)
  end.

getSsdItemNumFromHirQ([], N) -> N;
getSsdItemNumFromHirQ([H|T], N) ->
  case H#cacheline.type of 
    rhir -> 
      case H#cacheline.location of
        ssd -> getSsdItemNumFromHirQ(T, N+1);
        _ -> getSsdItemNumFromHirQ(T, N)
      end;
    _ ->
      getSsdItemNumFromHirQ(T, N)
  end.
  
