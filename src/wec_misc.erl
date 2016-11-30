-module(wec_misc).
-export([calHitElem/1,calHit/1,printHit/2,addHit/1,printIndex/1,calIndex/2,setAverage/0, set0/3,checkUCLN/2, ceil/1, initStat/0, 
  ssdAddinto/0, ssdAddinto/1, ssdEvict/1, ssdEvict/0, logSsdCachedItem/1, ramHit/0, ssdHit/0, ebHit/0, ebWrite/0, isReadReq/0, printHitRate/1, printSsdWrite/1]).

-record(cacheline, {fileid=-1, access_num=1, last_access_time=-1, expireTime=-1, location=hdd, type=undefined}). 
-record (dictElem, {acc = {1, 0, 0, 0, 0, 0, 0, 0, 0, 0}, p = 1, location = hdd, last_acc_period = 0, waitq = 0, last_gcon = 0,
  last_acc_time = 0, first_acc_time = 0}).


%% -----------------------------------
%%            Tools Module
%% -----------------------------------

set(N) ->
  
  {P, L} = get("ucln"),
  L1 = lists:map(fun(A)->
      Tmp =  (P - A + 11) rem 10,
      case Tmp =< N of
        true->
          if
            Tmp =:= 0 ->
              if
                N =:= 10 ->
                  1;
                true ->
                  0
              end;
            true ->
              1
          end;
        _->
          0
      end
  end, [1,2,3,4,5,6,7,8,9,10]),
  L2 = lists:zipwith(fun(X, Y) -> X+Y end, L, L1),
  put("ucln",{P, L2}).



getcor(Num) ->  
  if
    Num =:= 11->
      1;    
    true->
      Num
  end.



checkUCLN(Cache, Dict) ->

    case dict:find(Cache, Dict) of
      {ok, X} ->
        % io:format("~p~n",[X]),
        case  X#dictElem.last_acc_period <  get("period_num") of
          true->
            set(get("period_num") - X#dictElem.last_acc_time);
          _->          
            ok
        end;         
      _->        
        set(min(get("period_num"),10))
        
    end.

set0(_, [], L) ->
  lists:reverse(L);
set0(1, [_H|T], L2) ->
  set0(0, T, [0|L2]);
set0(N,[H|T],L) ->
  set0(N-1, T, [H|L]).

setAverage() ->    
   {P, L} = get("ucln"),
   U = lists:nth(P,L),  
   
   if U=:= 0 -> ok;
      true ->
   A = get("period_length") * 10/U,
   put("average", A),
   % {ok, S} = file:open("../log/ucln.csv",[append]),
   % io:format(S,"~p,~p~n",[U,A]),
   % file:close(S),
   put("ucln",{getcor(P+1), set0(getcor(P+1),L,[])})
 end.
   
   

ceil(Num) ->
  Num1 = round(Num),
  case Num1 >= Num of
    true -> Num1;
    false -> Num1+1
  end.  
  
%% -----------------------------------
%%            Stat Module
%% -----------------------------------  
initStat() ->
  put("ssd_addinto_num", 0),
  put("ssd_evict_num", 0),
  put("ram_hit_num", 0),
  put("ssd_hit_num", 0),
  put("eb_hit_num", 0),
  put("eb_write_num", 0),
  put("read_req_num", 0),
  put("ssd_evict_noaccessblock_num", 0),
  put("ssd_total_access_num", 0).

  
ssdAddinto() ->
  Old = get("ssd_addinto_num"),
  put("ssd_addinto_num", Old+1).

ssdAddinto(N) ->
  Old = get("ssd_addinto_num"),
  put("ssd_addinto_num", Old+N).
  


% called from wec_fr:postProcessQ:          
% if
%           LastAcc =:= 1 ->
%             wec_misc:ssdEvict();
% TODO: should it be called?
ssdEvict() ->
  ok.



%ssdEvict() ->
%  Old = get("ssd_evict_num"),
%  put("ssd_evict_num", Old+1).

% register the evict information
ssdEvict(Item) ->
  Old = get("ssd_evict_num"),
  put("ssd_evict_num", Old+1),
  case Item#cacheline.access_num of
    1 ->
      Old1 = get("ssd_evict_noaccessblock_num"),
      put("ssd_evict_noaccessblock_num", Old1+1);
    _ ->
      Old1 = get("ssd_total_access_num"),
      put("ssd_total_access_num", Old1+Item#cacheline.access_num-1)
  end.

logSsdCachedItem(Item) ->
  case Item#cacheline.access_num of
    1 ->
      Old = get("ssd_evict_noaccessblock_num"),
      put("ssd_evict_noaccessblock_num", Old+1);
    _ ->
      Old = get("ssd_total_access_num"),
      put("ssd_total_access_num", Old+Item#cacheline.access_num-1)
  end.



%ssdEvict(N) ->
%  Old = get("ssd_evict_num"),
%  put("ssd_evict_num", Old+N).
  
ramHit() ->
  Old = get("ram_hit_num"),
  put("ram_hit_num", Old+1).
  
ssdHit() ->
  Old = get("ssd_hit_num"),
  put("ssd_hit_num", Old+1). 
  
ebHit() ->
  Old = get("eb_hit_num"),
  put("eb_hit_num", Old+1). 

ebWrite() ->
  Old = get("eb_write_num"),
  put("eb_write_num", Old+1). 

isReadReq() ->
  Old = get("read_req_num"),
  put("read_req_num", Old+1).

printHitRate(S) ->
  ReadReq = get("read_req_num"),
  RamHit = get("ram_hit_num"),
  SsdHit = get("ssd_hit_num"),
  EbHit = get("eb_hit_num"),  
  EbWrite = get("eb_write_num"),
  io:format("****************************~n"),
  io:format("read req = ~p, ram hit = ~p, ssd hit = ~p, eb hit = ~p, eb write = ~p~n", [ReadReq, RamHit, SsdHit, EbHit, EbWrite]),
  io:format("Total Hit Rate = ~p%~nRam Hit Rate = ~p%~nSsd Hit Rate = ~p%~nEB Hit Rate = ~p%~n", [100*(RamHit+SsdHit)/ReadReq, 100*RamHit/ReadReq, 100*SsdHit/(ReadReq-RamHit), 100*EbHit/(ReadReq-RamHit)]),
  io:format(S, "****************************~n", []),
  io:format(S, "read req = ~p, ram hit = ~p, ssd hit = ~p, eb hit = ~p, eb write = ~p~n", [ReadReq, RamHit, SsdHit, EbHit, EbWrite]),
  io:format(S, "Total Hit Rate = ~p%~nRam Hit Rate = ~p%~nSsd Hit Rate = ~p%~nEB Hit Rate = ~p%~n", [100*(RamHit+SsdHit)/ReadReq, 100*RamHit/ReadReq, 100*SsdHit/(ReadReq-RamHit), 100*EbHit/(ReadReq-RamHit)]),
  case EbWrite of
    0 -> ok;
    _ ->
      io:format("EbHit/EbWrite = ~p%~n", [100*EbHit/EbWrite]),
      io:format(S, "EbHit/EbWrite = ~p%~n", [100*EbHit/EbWrite])
  end.
  
printSsdWrite(S) ->
  SsdAddIn = get("ssd_addinto_num"),
  SsdEvict = get("ssd_evict_num"),
  SsdSize = get("ssd_cache_cap"),
  SsdNoaccessblockNum = get("ssd_evict_noaccessblock_num"),
  TotalAccessNum = get("ssd_total_access_num"),
  io:format("****************************~n"),
  io:format("SsdAddIn = ~p, SsdEvict = ~p, SsdSize = ~p~n", [SsdAddIn, SsdEvict, SsdSize]),
  if
    SsdAddIn=:=0 ->
      ok;
    true->
    io:format("Write Amply Rate = ~p%~n", [100*SsdAddIn/SsdSize]),
  io:format("ssd_evict_noaccessblock_num = ~p, percentage = ~p%~n", [SsdNoaccessblockNum, 100*SsdNoaccessblockNum/SsdAddIn]),
  io:format("TotalAccessNum = ~p~n", [TotalAccessNum]),
  % io:format("mean access num of accessed cache data = ~p~n", [TotalAccessNum/(SsdAddIn-SsdNoaccessblockNum)]),
  io:format(S, "****************************~n", []),
  io:format(S, "SsdAddIn = ~p, SsdEvict = ~p, SsdSize = ~p~n", [SsdAddIn, SsdEvict, SsdSize]),
  io:format(S, "Write Amply Rate = ~p%~n", [100*SsdAddIn/SsdSize]),
  io:format(S, "ssd_evict_noaccessblock_num = ~p, percentage = ~p~n", [SsdNoaccessblockNum, 100*SsdNoaccessblockNum/SsdAddIn]),
  io:format(S, "mean hits = ~p~n", [get("ssd_total_access_num")/(get("ssd_addinto_num") - SsdNoaccessblockNum)])
  % case length(get("listForIndex")) =:= 0 of
  %   true ->
  %     ok;
  %   _->
  %     io:format(S, "Average Index: ~p~n",[lists:sum(get("listForIndex"))/length(get("listForIndex"))]),
  %     io:format("Average Index: ~p~n",[lists:sum(get("listForIndex"))/length(get("listForIndex"))])
  % end
  
  end.
  
  %io:format(S, "mean access num of accessed cache data = ~p~n", [TotalAccessNum/(SsdAddIn-SsdNoaccessblockNum)]).
calIndex(List, Trace) ->
case length(List) of
  0 ->
    ok;
  _->

  put("index", 0),
  lists:foreach(fun(X) -> {Cache, _} = X, 
    % io:format("~p~n", [Cache]),
    {value, {_, Index}} = lists:keysearch(Cache, 1, ta_getRanklist:getRanklist(Trace)),
    put("index", get("index") + Index) end, List),
  Index = get("index")/length(List),
  put("listForIndex", [Index|get("listForIndex")])
end.
  
printIndex(S) ->
  List = get("listForIndex"),
  case 0 =:= length(List) of
    true ->
      Average = "length(List)=0";
    false ->
      Average = lists:sum(List) / length(List)
  end,
  io:format(S, "Index = ~p~n", [Average]).

calHit([]) -> ok;

calHit([H|T]) ->
  {Cache, _} = H,
  case get([Cache]) of
    undefined ->
      put("nohit", get("nohit") + 1);
    Num ->
      erase([Cache]),
      put("meanhit", get("meanhit") + Num)
  end,
  calHit(T).

calHitElem(Cache) ->
case get([Cache]) of
    undefined ->
      put("nohit", get("nohit") + 1);
    Num ->
      erase([Cache]),
      put("meanhit", get("meanhit") + Num)
  end.
printHit(S,Trace) ->
    Ucln = trace_getTracePars:getUCLN(Trace),
    Nohit = get("nohit"),
    io:format(S, "Mean Hits = ~p, No Hits = ~p,~p%~n", [get("meanhit")/(get("ssd_addinto_num")-Nohit), Nohit,100 * Nohit / Ucln]).

addHit(CacheId) ->
  case get([CacheId]) of
        undefined ->
          put([CacheId], 1);
        _->
            put([CacheId], get([CacheId]) + 1)
  end.