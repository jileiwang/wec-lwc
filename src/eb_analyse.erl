-module (eb_analyse).
-export([start/0, start/15]).

-record(throttling, {do, epoch, update_limit, left_updates}).


getFilePath2() -> "../log/".


getLogFile(Trace) ->
  
        getFilePath2() ++  atom_to_list(Trace) ++ "-0704.log".
   



loopSize(_Trace, _Alg, _RAM_Size, [], _SieveMode, _SieveThr, _SievePeroid, _EBType, _IdleTimeThr, _S, _VirtualRamFlag, _DoThrottling, _Epoch, _UpdateLimit) -> ok;
loopSize(Trace, Alg, [Hram|Tram], [H|T], SieveMode, SieveThr, SievePeroid, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit) ->
  io:format("~n    Trace=~p, Alg=~p, ram size=~p, ssd size=~p, EBType=~p  IdleTimeThr=~p, Throttling={~p,~p,~p}~n", [Trace, Alg, Hram, H, EBType, IdleTimeThr, DoThrottling, Epoch, UpdateLimit]),
  io:format(S, "~n    Trace=~p, Alg=~p, ram size=~p, ssd size=~p, EBType=~p  IdleTimeThr=~p, Throttling={~p,~p,~p}~n", [Trace, Alg, Hram, H, EBType, IdleTimeThr, DoThrottling, Epoch, UpdateLimit]),
  start(Trace, lru, Alg, Hram, H, SieveMode, SieveThr, SievePeroid, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit),
  loopSize(Trace, Alg, Tram, T, SieveMode, SieveThr, SievePeroid, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit).

loopAlg(_Trace, [], _RAM_Size, _SSD_Sizes, _SieveMode, _SieveThr, _SievePeroid, _EBType, _IdleTimeThr, _S, _VirtualRamFlag, _DoThrottling, _Epoch, _UpdateLimit)
 -> ok;
loopAlg(Trace, [H|T], RAM_Size, SSD_Sizes, SieveMode, SieveThr, SievePeroid, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit) ->
  loopSize(Trace, H, RAM_Size, SSD_Sizes, SieveMode, SieveThr, SievePeroid, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit),
  loopAlg(Trace, T, RAM_Size, SSD_Sizes, SieveMode, SieveThr, SievePeroid, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit).
  
loopTrace(_Algs, [], _RAM_Perc, _SSD_Percs, _SieveMode, _SieveThr, _SievePeroid, _EBType, _IdleTimeThr, _S, _VirtualRamFlag, _DoThrottling, _Epoch, _UpdateLimit) 
-> ok;
loopTrace(Algs, [H|T], RAM_Perc, SSD_Percs, SieveMode, SieveThr, SievePeroid, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit) ->
  RAM_Sizes = lists:map(fun(X)->max(5,round(X*trace_getTracePars:getUCLN(H)+0.49999)) end, RAM_Perc),
  % RAM_Sizes =lists:map(fun(X)->0 end, RAM_Perc),
  SSD_Sizes = lists:map(fun(X)->round(X*trace_getTracePars:getUCLN(H)+0.49999) end, SSD_Percs),
  loopAlg(H, Algs, RAM_Sizes, SSD_Sizes, SieveMode, SieveThr, SievePeroid, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit),
  loopTrace(Algs, T, RAM_Perc, SSD_Percs, SieveMode, SieveThr, SievePeroid, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit).

start() ->
  SieveMode = false, %miss_sieve, %time_sieve,  %false,
  Thr = 7,  
  %Traces = [build1, build2, as1, as2, finance, websearch1, websearch2, cctv1, cctv2],
  Traces = [d1hs],    
  [H|T] = Traces,
  {ok, S} = file:open(getLogFile(H), write),
  %Algs = [lru, lfu, mq, lirs, arc],
  Algs = [lru],
  RAM_Perc = [0.005],
  SSD_Percs = [0.08],
  SieveThr = Thr,
  SievePeroid = 1000,
  EBType = no, % yes or no  
  IdleTimeThr = 1000, 
  VirtualRamFlag = 0, % =1 means ram cache but don't hit, every hit is processed as miss.
  DoThrottling = false, %true or false,
  Epoch = 3000,
  UpdateLimit = 4000,
  loopTrace(Algs, Traces, RAM_Perc, SSD_Percs, SieveMode, SieveThr, SievePeroid, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit),
  file:close(S).

 
start(Trace, Alg1, Alg2, RamCacheSize, SsdCacheSize, SieveMode, SieveThr, SievePeriod, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit) ->
  % io:format("Ram Size = ~p~n", [RamCacheSize]),
  {RamQ, SsdQ, Throttling} = init(Trace, Alg1, Alg2, RamCacheSize, SsdCacheSize, SieveMode, SieveThr, SievePeriod, 0.01, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit), 
  
  
  {SysTime, _RamQ1, SsdQ1, EvictBuffer}  = loadTraces(trace_getTracePars:getTraceFile(Trace), S, RamQ, SsdQ, Throttling),
  % io:format("~p~n",[get("tail_percentage")]),    
  % io:format("process finish~n"),
  % io:format("finished, Ssd1 = ~p~n", [SsdQ1]),
  % io:format(S, "finished, Ssd1 = ~p~n", [SsdQ1]),
  {EvictBuffer1, SsdQ2} = wec_se:smartEvictProcess(EvictBuffer, SsdQ1, SysTime), % last round: for last req
  postProcess(EvictBuffer1),
  postProcessQ(SsdQ2, Alg2),
  printResult().

printResult() ->
  initForTenBuckets(),
  lists:foreach(fun(X) -> {Cache, Order} = X, 
    case get("nohit" ++ integer_to_list(Cache)) of
      undefined ->
        ok;
      Value ->
        plus(Order, Value)
    end end, ta_getRanklist:getRanklist(get("trace"))),
  {ok, S} = file:open(getFilePath2() ++ "nohit-" ++ atom_to_list(get("trace")) ++ ".csv", write),
  Sum = getSum(10,0),
  
  writeResults(1, S, Sum),
  file:close(S).

getSum(0, Num) -> Num;
getSum(Num, Value) ->
  
  Value1 = Value + get("bucket" ++ integer_to_list(Num)),
  getSum(Num - 1, Value1).

writeResults(11, _, _) -> ok;
writeResults(Num, S, Sum) ->
  io:format(S, "~p%~n", [100 * get("bucket" ++ integer_to_list(Num))/Sum]),
  writeResults(Num+1, S, Sum).

initForTenBuckets()->
  initBuckets(10).

initBuckets(0) -> ok;
initBuckets(Number) ->
  put("bucket" ++ integer_to_list(Number), 0),
  initBuckets(Number - 1).

plus(Order, Value) ->
  Percent = Order / trace_getTracePars:getUCLN(get("trace")),
  
  
  if
    Percent =< 0.1 ->
      put("bucket1", get("bucket1") + Value);
    Percent =< 0.2 ->
      put("bucket2", get("bucket2") + Value);
    Percent =< 0.3 ->
      put("bucket3", get("bucket3") + Value);
    Percent =< 0.4 ->
      put("bucket4", get("bucket4") + Value);
    Percent =< 0.5 ->
      put("bucket5", get("bucket5") + Value);
    Percent =< 0.6 ->
      put("bucket6", get("bucket6") + Value);
    Percent =< 0.7 ->
      put("bucket7", get("bucket7") + Value);
    Percent =< 0.8 ->
      put("bucket8", get("bucket8") + Value);
    Percent =< 0.9 ->
      put("bucket9", get("bucket9") + Value);
    true ->     
      put("bucket10", get("bucket10") + Value)

  end.

postProcess([]) -> ok;
postProcess([H|T]) ->
  wec_misc:logSsdCachedItem(H),
  postProcess(T).

postProcessQ(Q, Alg) ->
  case Alg of
    fifo -> postProcess(Q);  
    lru -> postProcess(Q); 
    lfu -> postProcess(Q);
    mq -> error;
    lirs -> error;
    _ -> error

  end,
  case get("ssd_alg") of        
         mq ->
            {LforI1, _} = Q,
            LforI = lists:append(LforI1);
         lirs ->
            {L1, L2} = Q,
            LforI =L1 ++ L2;
         arc ->
            {L1, L2, T1, T2, _P} = Q,
            LforI = lists:sublist(L1, T1) ++ lists:sublist(L2, T2);
          _->
            LforI = Q
     end,

    List = lists:map(fun(X)->
         {_, Id, Acc, LastAcc, _, _, _} = X,
         if
           LastAcc =:= 1 ->
             wec_misc:ssdEvict();
          true ->
             ok
         end,
         {Id, Acc} 
       end, LforI),
    % io:format("~p~n",[List]),
     wec_misc:calIndex(List, get("trace")).
 

% return cache pool (one queue or multiple queue)
init(Trace, Alg1, Alg2, RamCacheSize, SsdCacheSize, SieveMode, SieveThr, SievePeriod, TailPercentage, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit) ->
  erase(),
  put("ram_cache_cap", RamCacheSize),
  put("ssd_cache_cap", SsdCacheSize),
  put("virtual_ram", VirtualRamFlag),
  put("ram_alg", Alg1),
  put("ssd_alg", Alg2),
  put("listForIndex",[]),
  put("trace", Trace),
  put("meanhit", 0),
  put("nohit", 0),  
  wec_misc:initStat(),
  wec_sv:initSV(SieveMode, SieveThr, SievePeriod, TailPercentage),
  wec_se:initEB(EBType, IdleTimeThr),
  Throttling = wec_throttling:init(DoThrottling, Epoch, UpdateLimit),  
  RamQ = wec_cr:initCacheReplacement(Alg1, 1, 50, hdd, 0.01, RamCacheSize),   % location is useless for RAM Cache   
  SsdQ = wec_cr:initCacheReplacement(Alg2, 2, 50, hdd, 0.01, SsdCacheSize),
  {RamQ, SsdQ, Throttling}.

checkVRam(IfHit_Origin) ->
  Flag = get("virtual_ram"),
  case Flag of
    1 -> miss;
    _ -> IfHit_Origin
  end.

% params: Reqs, SysTime, RamQ, SsdQ, EvictBuffer 
process(_S,[], SysTime, RamQ, SsdQ, EvictBuffer, Throttling) -> {SysTime, RamQ, SsdQ, EvictBuffer, Throttling};
process(S,[H|T], SysTime, RamQ, PreSsdQ, EvictBuffer, Throttling) ->

 % io:format("ramQ = ~p~nPreSsdq = ~p~n",[RamQ,PreSsdQ]),
 % io:format("entrance : ssd_addinto_num = ~p~n", [get("ssd_addinto_num")]),
  {Type, _DiskID, CachelineID} = H, 
  {EvictBuffer1, SsdQ} = wec_se:smartEvictProcess(EvictBuffer, PreSsdQ, SysTime), 
  
  case SysTime/trace_getTracePars:getTraceLen(get("trace")) =< 0.25 of
      true ->
        
  
    
  case Type of 
    1 ->  % write
      RamQ1 = wec_cr:writeReq2Cache(RamQ, ram, CachelineID),
      SsdQ1 = wec_cr:writeReq2Cache(SsdQ, ssd, CachelineID),      
      EvictBuffer2 = wec_se:delFromEvictBuffer(EvictBuffer1, CachelineID),
      wec_sv:resetSieve(CachelineID),   
      process(S,T, SysTime, RamQ1, SsdQ1, EvictBuffer2, Throttling);      
    0 ->  % read
      
      wec_misc:isReadReq(),
      wec_sv:tick(),
      Throttling1 = wec_throttling:time_tick(SysTime, Throttling),     
      % io:format("~p,~p~n", [get("ssd_addinto_num"),length(SsdQ)]),
      % io:format("intitial : ssd_addinto_num = ~p~n", [get("ssd_addinto_num")]),
      % 1. check ram cache
      {IfHit_Origin, _, NewRamQ, _} = wec_cr:readReq2Cache(RamQ, ram, CachelineID, SysTime, false, null),
      IfHit = checkVRam(IfHit_Origin),
      case IfHit of
        hit -> % ram hit               
          wec_misc:ramHit(),
          % io:format("ram hit : ssd_addinto_num = ~p~n", [get("ssd_addinto_num")]),
          process(S,T, SysTime+1, NewRamQ, SsdQ, EvictBuffer1, Throttling1);
        miss -> % ram miss
          {EvictBuffer2, HitItem} = wec_se:inEvictBuffer(EvictBuffer1, CachelineID),
          case HitItem of
            null -> % EB miss, lookup SSD Q              
              IfSieve = get("sieve_mode"),
              {IfHit2, Location, NewSsdQ, Evicts} = wec_cr:readReq2Cache(SsdQ, ssd, CachelineID, SysTime, IfSieve, NewRamQ),  
              % io:format("ssd readReq2Cache : ssd_addinto_num = ~p~n", [get("ssd_addinto_num")]),            
              case IfHit2 of
                hit ->
                  wec_sv:resetSieve(CachelineID),
                  if 
                    Location =:= ssd ->
                      %io:format("[[SSD HIT]]~n"),                    
                      wec_misc:ssdHit(),
                      % io:format("ssd hit : ssd_addinto_num = ~p~n", [get("ssd_addinto_num")]),
                      process(S,T, SysTime+1, NewRamQ, NewSsdQ, EvictBuffer2, Throttling1);
                    true ->                      
                      {CanUpdate, Throttling2} = wec_throttling:consume(Throttling1),
                      % io:format(" 1   THROTTLING: Do=~p,CanUpdate=~p, Left=~p~n", [Throttling2#throttling.do, CanUpdate, Throttling2#throttling.left_updates]),
                      % io:format("consume : ssd_addinto_num = ~p~n", [get("ssd_addinto_num")]),
                      case CanUpdate of
                        0 -> % don't update contents in SSD
                          process(S,T, SysTime+1, NewRamQ, SsdQ, EvictBuffer2, Throttling2);
                        _ -> % update SSD Queue
                          if
                            length(Evicts) > 0 ->
                              
                              {EvictBuffer3, SsdQ1} = processEvicts(EvictBuffer2, Evicts, NewSsdQ);
                            true ->
                              EvictBuffer3 = EvictBuffer2,
                              SsdQ1 = NewSsdQ
                          end,                     
                          
                          process(S,T, SysTime+1, NewRamQ, SsdQ1, EvictBuffer3, Throttling2)
                      end
                  end;
                miss ->
                       
                  wec_sv:accumSieve(CachelineID),
                  {CanUpdate, Throttling2} = wec_throttling:consume(Throttling1),                  
                  case CanUpdate of
                    0 ->

                      process(S,T, SysTime+1, NewRamQ, SsdQ, EvictBuffer2, Throttling2);
                    _ ->
                      if
                        length(Evicts) > 0 ->
                          % Evict Buffer, process Evicts
                          
                          evictRecord(Evicts),
                          {EvictBuffer3, SsdQ1} = processEvicts(EvictBuffer2, Evicts, NewSsdQ);
                        true ->
                          EvictBuffer3 = EvictBuffer2,
                          SsdQ1 = NewSsdQ
                      end,
                      % io:format("miss: ssd_addinto_num = ~p, SsdQ1 = ~p~n", [get("ssd_addinto_num"),SsdQ1]),
                      process(S,T, SysTime+1, NewRamQ, SsdQ1, EvictBuffer3, Throttling2)
                  end
              end;               
            _ ->  % EB hit, has removed HitItem from EB; needto put it at the head of SSD Q
            % io:format("[[SSD HIT (Evict Buffer)]]~n"),       
              wec_misc:ebHit(),              
              wec_misc:ssdHit(),
              %inQueue()»áÏÈ½«shaddow cacheÖÐÓëHitItemÖØ¸´µÄÏîÉ¾µô£¬ÔÙ½«HitItem²åÈëSSD Cache
              Alg = get("ssd_alg"),
              {_, NewSsdQ, _, _} = wec_cr:inQueue(SsdQ, CachelineID, Alg, ssd),
              %io:format("HitItem = ~p~n", [HitItem]),             
              {NewSsdQ1, Evicts} = wec_cr:hitQueue(NewSsdQ, ssd, HitItem, SysTime, miss),  % the last para HitType is only for LIRS alg
              if 
                length(Evicts) > 0 ->
                  % Evict Buffer, process Evicts
                  evictRecord(Evicts),
                  {EvictBuffer3, NewSsdQ2} = processEvicts(EvictBuffer2, Evicts, NewSsdQ1);
                true -> 
                  EvictBuffer3 = EvictBuffer2,
                  NewSsdQ2 = NewSsdQ1  
              end,              
              process(S,T, SysTime+1, NewRamQ, NewSsdQ2, EvictBuffer3, Throttling1)
          end  
      end    
  end;
  _->
    {SysTime, RamQ, SsdQ, EvictBuffer, Throttling}
  end.


evictRecord([]) -> ok;
evictRecord([He|T]) ->
  {_, H, _ , _ , _, _ , _} = He,
  case get("nohit" ++ integer_to_list(H)) of
    undefined ->
      put("nohit" ++ integer_to_list(H), 1);
      % io:format("~p~n", [get("nohit" ++ integer_to_list(H))]);
    Value ->
      put("nohit" ++ integer_to_list(H), Value + 1)
  end,
  evictRecord(T).

processEvicts(EvictBuffer, [], SsdQ) -> {EvictBuffer, SsdQ};
processEvicts(EvictBuffer, [H|T], SsdQ) ->
  %io:format("processEvicts, H = ~p~n", [H]),
  {EvictBuffer1, SsdQ1} = wec_se:push2EvictBuffer(EvictBuffer, H, SsdQ),
  processEvicts(EvictBuffer1, T, SsdQ1).
  

%% -------------------------
%%     Load Trace Module
%% -------------------------
readlines(S, Reqs, SysTime,RamQ,SsdQ, EvictBuffer, Throttling) ->
  
  case length(Reqs) rem 10000 of
    9999 ->
    % process(S,[], SysTime, RamQ, SsdQ, EvictBuffer, _Throttling) -> {SysTime, RamQ, SsdQ, EvictBuffer}
      io:format("read ~p~n",[length(Reqs)]),
      
      {SysTime1, RamQ1, SsdQ1, EvictBuffer1, Throttling1} = process(S,lists:reverse(Reqs),SysTime,RamQ,SsdQ, EvictBuffer, Throttling),
      
      readlines(S,[],SysTime1,RamQ1,SsdQ1, EvictBuffer1, Throttling1);
    _->
       Line = io:get_line(S, ''),
  case Line of 
    eof -> 
         file:close(S),
         
         {SysTime1, RamQ1, SsdQ1, EvictBuffer1, _Throttling1}  = process(S,lists:reverse(Reqs),SysTime,RamQ,SsdQ, EvictBuffer, Throttling),
         
         {SysTime1, RamQ1, SsdQ1, EvictBuffer1};
    _ ->
         Res = string:tokens(Line, " ,\t\n{}"),
         case Res=:=[] of 
           true -> readlines(S, Reqs, SysTime,RamQ,SsdQ, EvictBuffer, Throttling);
           _ ->  
             % line format: <Type(0-read, 1-write) DiskID(no use now) CachelineID>
             [Type_str, DiskID_str, CachelineID_str|_] = Res,
             {Type, _} = string:to_integer(Type_str),
             {DiskID, _} = string:to_integer(DiskID_str),
             {CachelineID, _} = string:to_integer(CachelineID_str),
             readlines(S, [{Type, DiskID, CachelineID}|Reqs],SysTime,RamQ,SsdQ, EvictBuffer, Throttling)
         end
       end
  end.  

loadTraces(TraceFile, S,  RamQ, SsdQ, Throttling) -> 
  io:format("tracefile = ~p~n", [TraceFile]),
  io:format(S, "tracefile = ~p~n", [TraceFile]),
  {ok, S1} = file:open(TraceFile, read),
  io:format("open trace file ok~n"),
  readlines(S1, [], 0,  RamQ, SsdQ, [], Throttling).
  
