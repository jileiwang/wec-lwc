-module(wec_arc).
-export([initARC/0, inARC/3, addtoARC/4, deleteFromARC/2, getARCQTail/2, markArcQ/2, getSsdItemNumFromArc/1, markBottom/2]).

-record(cacheline, {fileid=-1, access_num=1, last_access_time=-1, expireTime=-1, location=hdd, type=undefined}). 

%% -----------------------------------
%%            ARC Module
%% -----------------------------------
initARC() ->
  {[], [], 0, 0, 0.5}.   % initP = 0.5


inARC(Q, CachelineID, CacheCap) ->
  {L1, L2, T1, T2, P} = Q,
  R1 = lists:keyfind(CachelineID, 2, L1),
  R2 = lists:keyfind(CachelineID, 2, L2),
  if
    R1=:=false andalso R2=:=false ->
      {miss, Q, null, miss};
    R1=:=false andalso R2=/=false ->    % L2 Hit
      R3 = lists:keyfind(CachelineID, 2, lists:sublist(L2, T2)),
      case R3 of
        false ->   % L2's Bottom Hit          
          NewP = updateP_Negative(P, length(L1)-T1, length(L2)-T2),
          {miss, {L1, lists:delete(R2, L2), T1, T2, NewP}, R2, b2};
        _ ->       % L2's Top Hit
          {hit, {L1, lists:delete(R2, L2), T1, T2-1, P}, R3, t2}
      end;
    R1=/=false andalso R2=:=false ->    % L1 Hit
      R4 = lists:keyfind(CachelineID, 2, lists:sublist(L1, T1)),
      case R4 of
        false ->   % L1's Bottom Hit
          NewP = updateP_Positive(P, CacheCap, length(L1)-T1, length(L2)-T2),
          {miss, {lists:delete(R1, L1), L2, T1, T2, NewP}, R1, b1};
        _ ->       % L1's Top Hit
          {hit, {lists:delete(R1, L1), L2, T1-1, T2, P}, R4, t1}
      end;
    true ->  % should never happen
      error
  end.         


addtoARC(Q, Item, HitType, CacheCap) ->
  {L1, L2, T1, T2, P} = Q,
  case HitType of 
    miss ->     % put Item at the head of L1, for MISS item
      NewL1 = [Item|L1],
      case T1+1+T2 > CacheCap of
        true ->
          {NewT1, NewT2, New1L1, NewL2, Evicted} = replace(Item, P, T1+1, T2, NewL1, L2),
          {New1T1, New1T2, New2L1, New1L2} = check2L(CacheCap, NewT1, NewT2, New1L1, NewL2),
          case Evicted of
            null ->
              {{New2L1, New1L2, New1T1, New1T2, P}, []};        
            _ ->
              {{New2L1, New1L2, New1T1, New1T2, P}, [Evicted]}      
          end;
        false ->
          {NewT1, NewT2, New1L1, NewL2} = check2L(CacheCap, T1+1, T2, NewL1, L2),
          {{New1L1, NewL2, NewT1, NewT2, P}, []}
      end;    
    _ ->      % put Item at the head of L2, for T1/T2/B1/B2 HIT item, no evicted item
      NewL2 = [Item|L2],
      case T1+1+T2 > CacheCap of
        true ->
          {NewT1, NewT2, NewL1, New1L2, Evicted} = replace(Item, P, T1, T2+1, L1, NewL2),
          case Evicted of
            null ->
              {{NewL1, New1L2, NewT1, NewT2, P}, []};        
            _ ->
              {{NewL1, New1L2, NewT1, NewT2, P}, [Evicted]}      
          end;      
        false ->
          {{L1, NewL2, T1, T2+1, P}, []}
      end
  end.
    

deleteFromARC(Q, DelItem) ->
  {LruQ, LfuQ, T1, T2} = Q,
  LruQ1 = lists:delete(DelItem, LruQ),
  LfuQ1 = lists:delete(DelItem, LfuQ),
  Top1 = lists:sublist(LruQ, T1),
  Top2 = lists:sublist(LfuQ, T2), 
  C1 = lists:member(DelItem, Top1),
  C2 = lists:member(DelItem, Top2),  
  if 
    C1 -> {LruQ1, LfuQ1, T1-1, T2};
    C2 -> {LruQ1, LfuQ1, T1, T2-1};
    true -> {LruQ1, LfuQ1, T1, T2} 
  end.


getARCQTail(Q, TailPercentage) ->
  {L1, L2, T1, T2, _P} = Q,
  Num = wec_misc:ceil((T1+T2)*TailPercentage),
  Top1 = lists:sublist(L1, T1),
  Top2 = lists:sublist(L2, T2),  
  Tail1 = lists:sublist(lists:reverse(Top1), trunc(Num/2)),
  Tail2 = lists:sublist(lists:reverse(Top2), Num-trunc(Num/2)),
  % io:format("wec_arc:getARCQTail:num = ~p Tail1 = ~p Tail2 = ~p~n",[Num,Tail1,Tail2]),
  Tail1 ++ Tail2.


updateP_Positive(P, C, LenB1, LenB2) -> 
	if 
		LenB1 < LenB2 ->
			D = LenB2 div LenB1, 
			mymin(P+D, C);
		true ->
			mymin(P+1, C)
	end.
	
updateP_Negative(P, LenB1, LenB2) -> 
	if 
		LenB1 > LenB2 ->
			D = LenB1 div LenB2, 
			mymax(P-D, 0);
		true ->
			mymax(P-1, 0)
	end.

mymin(A, B) ->
	case A =< B of
		true -> A;
		_ -> B
	end.

mymax(A, B) ->
	case A >= B of
		true -> A;
		_ -> B
	end.

%replace(_Item, _P, T1, T2, [], []) -> {T1, T2, [], [], null};
%replace(_Item, _P, T1, T2, [], L2) -> {T1, T2-1, [], L2, lists:nth(T2, L2)};
%replace(_Item, _P, T1, T2, L1, []) -> {T1-1, T2, L1, [], lists:nth(T1, L1)};
replace(_Item, _P, 0, 0, L1, L2) -> {0, 0, L1, L2, null};
replace(_Item, _P, 0, T2, L1, L2) -> {0, T2-1, L1, L2, lists:nth(T2, L2)};
replace(_Item, _P, T1, 0, L1, L2) -> {T1-1, 0, L1, L2, lists:nth(T1, L1)};
replace(_Item, P, T1, T2, L1, L2) when T1>P -> {T1-1, T2, L1, L2, lists:nth(T1, L1)};
replace(Item, P, T1, T2, L1, L2) -> 
	R1 = lists:member(Item, lists:sublist(L2, T2+1, length(L2))),
	R2 = (T1 =:= P),
	case R1 andalso R2 of
		true -> {T1-1, T2, L1, L2, lists:nth(T1, L1)};
		_ -> {T1, T2-1, L1, L2, lists:nth(T2, L2)}
	end.

check2L(CacheCap, T1, T2, L1, L2) ->
	case length(L1) > CacheCap of
		true -> 
			case T1 > CacheCap of
				true ->
          {CacheCap, T2, lists:sublist(L1, CacheCap), L2};
				_ -> 
          {T1, T2, lists:sublist(L1, CacheCap), L2}
			end;
		_ -> 
			case length(L1) + length(L2) > 2*CacheCap of
				true -> 
					case T2 > 2*CacheCap of
						true ->
              {T1, 2*CacheCap, L1, lists:sublist(L2, 2*CacheCap-length(L1))};
						_ -> 
              {T1, T2, L1, lists:sublist(L2, 2*CacheCap-length(L1))}
					end;
				_ -> 
          {T1, T2, L1, L2}
			end
	end.


markArcQ(SsdQ, N) -> 
  {L1, L2, T1, T2, P} = SsdQ,
  {Left, NewL2} = markQ(lists:sublist(L2, T2), N, []),
  case Left of
    0 -> 
      {0, {L1, NewL2 ++ lists:nthtail(T2, L2), T1, T2, P}};
    _ ->
      {Left1, NewL1} = markQ(lists:sublist(L1, T1), N, []),
      {Left1, {NewL1 ++ lists:nthtail(T1, L1), NewL2 ++ lists:nthtail(T2, L2), T1, T2, P}}
  end.

markQ([], Left, SsdQ) -> {Left, lists:reverse(SsdQ)};
markQ(Q, 0, SsdQ) -> {0, lists:reverse(SsdQ) ++ Q};
markQ([H|T], Left, SsdQ) -> 
  case H#cacheline.location of
    hdd ->
      H1 = H#cacheline{location=ssd},
      markQ(T, Left-1, [H1|SsdQ]);
    _ -> 
      markQ(T, Left, [H|SsdQ])
  end.

% change the related item's location to hdd
markBottom(Q, CachelineID) ->
  {L1, L2, T1, T2, P} = Q,
  NewL1 = lists:sublist(L1, T1) ++ markList(lists:nthtail(T1, L1), CachelineID, []),
  NewL2 = lists:sublist(L2, T2) ++ markList(lists:nthtail(T2, L2), CachelineID, []),
  {NewL1, NewL2, T1, T2, P}.
  
markList([], _CachelineID, L) -> lists:reverse(L);
markList([H|T], CachelineID, L) -> 
  case H#cacheline.fileid of
    CachelineID ->
      H1 = H#cacheline{location=hdd}, 
      lists:reverse([H1|L]) ++ T;
    _ ->
      markList(T, CachelineID, [H|L])
  end.
  

getSsdItemNumFromArc(SsdQ) -> 
  {L1, L2, T1, T2, _P} = SsdQ,
  N1 = getSsdItemNumFromArc(lists:sublist(L1, T1), 0),
  N2 = getSsdItemNumFromArc(lists:sublist(L2, T2), 0),
  N1+N2.

getSsdItemNumFromArc([], N) -> N;
getSsdItemNumFromArc([H|T], N) ->
  case H#cacheline.location of
    ssd -> getSsdItemNumFromArc(T, N+1);
    _ -> getSsdItemNumFromArc(T, N)
  end.


