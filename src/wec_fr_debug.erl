-module(wec_fr_debug).
-export([start/0, start/1, start/2, start/15, start/5, start_config/0]).

-record(throttling, {do, epoch, update_limit, left_updates}).


% getFilePath2() -> "C:/Program Files/erl6.2/log/".
getFilePath2() -> "../log/".

getLogFile(Trace) ->
  
        getFilePath2() ++  atom_to_list(Trace) ++ ".log".
   



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



% start/2, for python code to call this function
% TODO: this function cannot be called as "erl -noshell -s wec_fr start as1 lru -s init stop"
%       guess if there is 2 args, in ubuntu, cannot call. to be find out the reason.
start(Trace, Alg) ->
  SieveMode = miss_sieve, %miss_sieve, %time_sieve,  %false,
  Thr = 5,
  Traces = [Trace],
  {ok, S} = file:open(getLogFile(Trace), write),
  Algs = [Alg],
  RAM_Perc = [0.001],
  SSD_Percs = [0.05],
  SieveThr = Thr,
  SievePeroid = 10000,
  EBType = no, % yes or no  
  IdleTimeThr = 1000, 
  VirtualRamFlag = 0, % =1 means ram cache but don't hit, every hit is processed as miss.
  DoThrottling = false, %true or false,
  Epoch = 3000,
  UpdateLimit = 4000,
  loopTrace(Algs, Traces, RAM_Perc, SSD_Percs, SieveMode, SieveThr, SievePeroid, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit),
  file:close(S).


start(Trace) ->
  SieveMode = miss_sieve, %miss_sieve, %time_sieve,  %false,
  Thr = 5,
  Traces = [Trace],
  {ok, S} = file:open(getLogFile(Trace), write),
  Algs = [lru, lfu, mq, arc, larc, lru_lwc],
  RAM_Perc = [0.001],
  SSD_Percs = [0.05],
  SieveThr = Thr,
  SievePeroid = 10000,
  EBType = no, % yes or no  
  IdleTimeThr = 1000, 
  VirtualRamFlag = 0, % =1 means ram cache but don't hit, every hit is processed as miss.
  DoThrottling = false, %true or false,
  Epoch = 3000,
  UpdateLimit = 4000,
  loopTrace(Algs, Traces, RAM_Perc, SSD_Percs, SieveMode, SieveThr, SievePeroid, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit),
  file:close(S).



% This entrance function will be called by run_wec.py
start_config() ->
  SieveMode = false, %miss_sieve, %time_sieve,  %false,
  Thr = 5,
  read_lru_lwc_config(),
  Traces = [read_config("trace.config")],
  [H|_] = Traces,
  {ok, S} = file:open(getLogFile(H), write),
  %Algs = [lru, lfu, mq, arc, larc, lru_lwc],
  Algs = [read_config("algorithm.config")],
  % print arguments to debug
  [A|_] = Algs,
  io:format("Config: trace = ~p, algo = ~p~n", [H, A]),
  Para = wec_lru_lwc:getParamaters(),
  io:format("Config: ~p~n", [Para]),
  RAM_Perc = [0.001],
  SSD_Percs = [0.05],
  SieveThr = Thr,
  SievePeroid = 10000,
  EBType = no, % yes or no  
  IdleTimeThr = 1000, 
  VirtualRamFlag = 0, % =1 means ram cache but don't hit, every hit is processed as miss.
  DoThrottling = false, %true or false,
  Epoch = 3000,
  UpdateLimit = 4000,
  loopTrace(Algs, Traces, RAM_Perc, SSD_Percs, SieveMode, SieveThr, SievePeroid, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit),
  file:close(S),
  io:format("Config: trace = ~p, algo = ~p~n", [H, A]),
  io:format("Config: ~p~n", [Para]).

read_config(Filename) ->
  {ok, S} = file:open(Filename, read),
  Line = io:get_line(S, ''),
  file:close(S),
  Item = string:sub_string(Line, 1, length(Line)-1),
  list_to_atom(Item).

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
  wec_lru_lwc:initParamaters(WhitelistLen, WindowLen, WindowNum, Thre1, ThreMulti).


start() ->
  SieveMode = miss_sieve, %miss_sieve, %time_sieve,  %false,
  Thr = 5,
  
  %Traces = [build1, build2, as1, as2, finance, websearch1, websearch2, cctv1, cctv2],
  %Traces = [metanodej, fbrfa, fbnw, cctv1],    
  %Traces = [fbnw, cctv1],    
  %Traces = [metanodej],

  %Traces = [as2, websearch1, cctv2],
  Traces = [as2],

  [H|T] = Traces,
  {ok, S} = file:open(getLogFile(H), write),
  %Algs = [lru, lfu, mq, lirs, arc],
  %Algs = [lru, lru_lwc],
  Algs = [lru],
  RAM_Perc = [0.001],
  SSD_Percs = [0.05],
  % RAM_Perc = [0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001],
  % SSD_Percs = [0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1],
  SieveThr = Thr,
  SievePeroid = 10000,
  EBType = no, % yes or no  
  IdleTimeThr = 1000, 
  VirtualRamFlag = 0, % =1 means ram cache but don't hit, every hit is processed as miss.
  DoThrottling = false, %true or false,
  Epoch = 3000,
  UpdateLimit = 4000,
  loopTrace(Algs, Traces, RAM_Perc, SSD_Percs, SieveMode, SieveThr, SievePeroid, EBType, IdleTimeThr, S, VirtualRamFlag, DoThrottling, Epoch, UpdateLimit),
  file:close(S).

start(Trace, Alg, RamCacheSize, SsdCacheSize, Logname) ->
  {ok, S} = file:open(getLogFile(Logname), write),
  {RamQ, SsdQ, Throttling} = init(Trace, lru, Alg, RamCacheSize, SsdCacheSize, false, 0, 0, 0.01, no, 0, S, 0, no, 0, 0),   
  {SysTime, _RamQ1, SsdQ1, EvictBuffer}  = loadTraces(trace_getTracePars:getTraceFile(Trace), S, RamQ, SsdQ, Throttling),
  % io:format("~p~n",[get("tail_percentage")]),    
  % io:format("process finish~n"),
  % io:format("finished, Ssd1 = ~p~n", [SsdQ1]),
  % io:format(S, "finished, Ssd1 = ~p~n", [SsdQ1]),
  {EvictBuffer1, SsdQ2} = wec_se:smartEvictProcess(EvictBuffer, SsdQ1, SysTime), % last round: for last req
  postProcess(EvictBuffer1),
  postProcessQ(SsdQ2, Alg),

  io:format("finished, SysTime = ~p~n", [SysTime]),
  io:format(S, "finished, SysTime = ~p~n", [SysTime]),
  %io:format("finally, RamQ = ~p~n", [RamQ1]),
  % io:format("         SsdQ = ~p~n", [SsdQ2]),
  %io:format("         EvictBuffer = ~p~n", [EvictBuffer1]),
  wec_misc:printIndex(S),
  wec_misc:printHitRate(S),
  wec_misc:printSsdWrite(S).

 
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

  io:format("finished, SysTime = ~p~n", [SysTime]),
  io:format(S, "finished, SysTime = ~p~n", [SysTime]),
  %io:format("finally, RamQ = ~p~n", [RamQ1]),
  % io:format("         SsdQ = ~p~n", [SsdQ2]),
  %io:format("         EvictBuffer = ~p~n", [EvictBuffer1]),
  wec_misc:printIndex(S),
  wec_misc:printHitRate(S),
  wec_misc:printSsdWrite(S).

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

          % TODO ==== waht is this for?
          lru_lwc ->
            {LforI, _, _} = Q;

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

  io:format("RAM: size = ~p, Alg = ~p~n", [RamCacheSize, Alg1]),
  io:format("SSD: size = ~p, Alg = ~p~n", [SsdCacheSize, Alg2]),
  io:format(S, "RAM: size = ~p, Alg = ~p~n", [RamCacheSize, Alg1]),
  io:format(S, "SSD: size = ~p, Alg = ~p~n", [SsdCacheSize, Alg2]),
  wec_misc:initStat(),
  wec_sv:initSV(SieveMode, SieveThr, SievePeriod, TailPercentage),
  wec_se:initEB(EBType, IdleTimeThr),
  Throttling = wec_throttling:init(DoThrottling, Epoch, UpdateLimit),
  io:format("Sieve Mode = ~p (Thr = ~p, Period = ~p)~n", [SieveMode, SieveThr, SievePeriod]),
  io:format(S, "Sieve Mode = ~p (Thr = ~p, Period = ~p)~n", [SieveMode, SieveThr, SievePeriod]),
  io:format("EBType = ~p, IdleTimeThr = ~p~n", [EBType, IdleTimeThr]),
  io:format(S, "EBType = ~p, IdleTimeThr = ~p~n", [EBType, IdleTimeThr]),
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
  
  case SysTime rem 10000 of
    9999 -> 
      % SsdAddIn = get("ssd_addinto_num"),
      % SsdEvict = get("ssd_evict_num"),
      % io:format("Ssd hit = ~p~n",[0/0]),
      case get("ssd_alg") of
       
        mq ->
          {LforI1, _} = SsdQ,
          LforI = lists:append(LforI1);
        lirs ->
          {L1, L2} = SsdQ,
          LforI =L1 ++ L2;
        arc ->
          {L1, L2, _, _, _} = SsdQ,
          LforI = L1 ++ L2;
        % TODO ==== what is this for?
        lru_lwc ->
          {LforI, _, _} = SsdQ;
        _->
          LforI = SsdQ
      end,
      List = lists:map(fun(X)->
        {_, Id, Acc, _, _, _, _} = X,
        {Id, Acc} end, LforI),
      % io:format("~p~n",[List]),
      wec_misc:calIndex(List, get("trace"));
    _ ->
      ok
  end,  
    
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
                              % Evict Buffer, process Evicts
                              %io:format("Evicts = ~p~n", [Evicts]), 
                              {EvictBuffer3, SsdQ1} = processEvicts(EvictBuffer2, Evicts, NewSsdQ);
                            true ->
                              EvictBuffer3 = EvictBuffer2,
                              SsdQ1 = NewSsdQ
                          end,                     
                          % io:format("process evict : ssd_addinto_num = ~p~n", [get("ssd_addinto_num")]),     
                          process(S,T, SysTime+1, NewRamQ, SsdQ1, EvictBuffer3, Throttling2)
                      end
                  end;
                miss ->
                  %io:format("[[MISS]]~n"),                
                  wec_sv:accumSieve(CachelineID),
                  {CanUpdate, Throttling2} = wec_throttling:consume(Throttling1),                  
                  case CanUpdate of
                    0 ->

                      process(S,T, SysTime+1, NewRamQ, SsdQ, EvictBuffer2, Throttling2);
                    _ ->
                      if
                        length(Evicts) > 0 ->
                          % Evict Buffer, process Evicts
                          %io:format("Evicts = ~p~n", [Evicts]), 
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
              %inQueue()禄谩脧脠陆芦shaddow cache脰脨脫毛HitItem脰脴赂麓碌脛脧卯脡戮碌么拢卢脭脵陆芦HitItem虏氓脠毛SSD Cache
              Alg = get("ssd_alg"),
              {_, NewSsdQ, _, _} = wec_cr:inQueue(SsdQ, CachelineID, Alg, ssd),
              %io:format("HitItem = ~p~n", [HitItem]),             
              {NewSsdQ1, Evicts} = wec_cr:hitQueue(NewSsdQ, ssd, HitItem, SysTime, miss),  % the last para HitType is only for LIRS alg
              if 
                length(Evicts) > 0 ->
                  % Evict Buffer, process Evicts
                  %wec_misc:cacheEvict(length(Evicts)),
                  %io:format("Evicts = ~p~n", [Evicts]), 
                  {EvictBuffer3, NewSsdQ2} = processEvicts(EvictBuffer2, Evicts, NewSsdQ1);
                true -> 
                  EvictBuffer3 = EvictBuffer2,
                  NewSsdQ2 = NewSsdQ1  
              end,              
              process(S,T, SysTime+1, NewRamQ, NewSsdQ2, EvictBuffer3, Throttling1)
          end  
      end    
  end.
    
processEvicts(EvictBuffer, [], SsdQ) -> {EvictBuffer, SsdQ};
processEvicts(EvictBuffer, [H|T], SsdQ) ->
  %io:format("processEvicts, H = ~p~n", [H]),
  {EvictBuffer1, SsdQ1} = wec_se:push2EvictBuffer(EvictBuffer, H, SsdQ),
  processEvicts(EvictBuffer1, T, SsdQ1).
  

%% -------------------------
%%     Load Trace Module
%% -------------------------
readlines(S, Reqs, SysTime,RamQ,SsdQ, EvictBuffer, Throttling) ->
  
  % DEBUG : read first 50 lines to look what happend
  case length(Reqs) rem 10000 of
    100 ->
    % process(S,[], SysTime, RamQ, SsdQ, EvictBuffer, _Throttling) -> {SysTime, RamQ, SsdQ, EvictBuffer}
      io:format("read ~p~n",[length(Reqs)]),
      
      {SysTime1, RamQ1, SsdQ1, EvictBuffer1, Throttling1} = process(S,lists:reverse(Reqs),SysTime,RamQ,SsdQ, EvictBuffer, Throttling),
      {SysTime1, RamQ1, SsdQ1, EvictBuffer1};
      % readlines(S,[],SysTime1,RamQ1,SsdQ1, EvictBuffer1, Throttling1);
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
  
