-module(wec_frlgb).
-export([start/0, start/13]).




getLogFile(Trace) -> 
  case Trace of
    websearch2 ->
      getFilePath2() ++ "websearch2.log";
    websearch1 ->
      getFilePath2() ++ "websearch1.log";
    test ->
      getFilePath2() ++ "test.log";
    fbrd ->
      getFilePath2() ++ "fbrd.log";
    cctv1 ->
      getFilePath2() ++ "cctv1.log";
    generate ->
      getFilePath2() ++ "generate.log";
    mds1 ->
      getFilePath2() ++ "mds1.log";
      lm -> getFilePath2() ++ "lm.log";
    finance ->
      getFilePath2() ++ "finance.log";
    datanode3 ->
      getFilePath2() ++ "node3.log";
    _->
      getFilePath2() ++ atom_to_list(Trace) ++ ".log"
    
  end.



getPeriodLength(_Trace) ->
  
  case _Trace of
    cctv1 ->
      10000;
    test -> 
      100;
    _->
      10000
    end
    % sd ->
    %   Ssdsize = get("ssd_size"),
    %   Throttling = get("throttling"),
    %   Ssdsize * 10000 div Throttling
 .

getFilePath2() -> "C:/Program Files/erl6.2/log/".



	
loopThrott(_Alg,_Trace, _RAM_Sizes, _SSD_Sizes, [], _S, _Paras, _AlgOfSSD, _EnableMC) -> ok;
loopThrott(Alg,Trace, RAM_Sizes, SSD_Sizes, [H|T], S, Paras, AlgOfSSD, EnableMC) ->
  if
    SSD_Sizes > H ->
      loopParas(Alg,Trace, RAM_Sizes, SSD_Sizes,H,S,Paras, AlgOfSSD, EnableMC);
    true ->
      ok

  end,  
  
  loopThrott(Alg,Trace, RAM_Sizes, SSD_Sizes, T, S, Paras, AlgOfSSD, EnableMC). 

loopParas(Alg,Trace, RAM_Sizes, SSD_Sizes,Throttling,S, Paras, AlgOfSSD, EnableMC) ->
    
   {Coefficient, Warmup, Sleep} = Paras,
   loopCoe(Alg, Trace, RAM_Sizes, SSD_Sizes,Throttling,S, Coefficient, Warmup, Sleep, AlgOfSSD, EnableMC).

loopCoe(_Alg,_Trace, _RAM_Sizes, _SSD_Sizes, _Throttling,  _S, [], _Warmup, _Sleep, _AlgOfSSD, _EnableMC) ->
  ok;
loopCoe(Alg, Trace, RAM_Sizes, SSD_Sizes,Throttling,S, [H|T], Warmup, Sleep, AlgOfSSD, EnableMC) ->
  loopWarmup(Alg, Trace, RAM_Sizes, SSD_Sizes,Throttling,S, H, Warmup, Sleep, AlgOfSSD, EnableMC),
  loopCoe(Alg, Trace, RAM_Sizes, SSD_Sizes,Throttling,S, T, Warmup, Sleep, AlgOfSSD, EnableMC).

loopWarmup(_Alg,_Trace, _RAM_Sizes, _SSD_Sizes, _Throttling, _S, _Coe, [], _Sleep, _AlgOfSSD, _EnableMC) -> ok;
loopWarmup(Alg, Trace, RAM_Sizes, SSD_Sizes,Throttling,S, Coefficient, [H|T], Sleep, AlgOfSSD, EnableMC) ->
  loopSleep(Alg, Trace, RAM_Sizes, SSD_Sizes,Throttling,S, Coefficient, H, Sleep, AlgOfSSD, EnableMC),
  loopWarmup(Alg, Trace, RAM_Sizes, SSD_Sizes,Throttling,S, Coefficient, T, Sleep, AlgOfSSD, EnableMC).

loopSleep(_Alg,_Trace, _RAM_Sizes, _SSD_Sizes, _Throttling,  _S, _Coe, _Warmup, [], _AlgOfSSD, _EnableMC) -> ok;
loopSleep(Alg, Trace, RAM_Sizes, SSD_Sizes,Throttling,S, Coefficient, Warmup, [H|T], AlgOfSSD, EnableMC) ->
   io:format("start~n"),
  start(Alg,Trace, RAM_Sizes, SSD_Sizes,Throttling,S, Coefficient, Warmup, H, AlgOfSSD, EnableMC),
  loopSleep(Alg, Trace, RAM_Sizes, SSD_Sizes,Throttling,S, Coefficient, Warmup, T, AlgOfSSD, EnableMC).

loopSize(_Alg,_Trace, _RAM_Size, [], _Throttling ,_S, _Paras, _AlgOfSSD, _EnableMC) -> ok;
loopSize(Alg,Trace, [Hram|Tram], [H|T], Throttling, S, Paras, AlgOfSSD, EnableMC) ->
  RAM_Sizes = max(5, round(Hram*trace_getTracePars:getUCLN(Trace)+0.49999)),
  % RAM_Sizes = 0,
  % SSD_Sizes = 5,
  SSD_Sizes = round(H*trace_getTracePars:getUCLN(Trace)+0.49999),
  io:format("~n    Trace=~p, ram size=~p, ssd size=~p~n", [Trace, RAM_Sizes, SSD_Sizes]),
  io:format(S, "~n    Trace=~p, ram size=~p, ssd size=~p~n", [Trace, RAM_Sizes, SSD_Sizes]),
  loopThrott(Alg,Trace, RAM_Sizes, SSD_Sizes, Throttling, S, Paras, AlgOfSSD, EnableMC),  
  loopSize(Alg,Trace, Tram, T, Throttling, S, Paras, AlgOfSSD, EnableMC).



loopTrace(_Algs, [], _RAM_Perc, _SSD_Percs, _Throttling, _S, _Paras, _AlgOfSSD, _EnableMC) 
-> ok;
loopTrace(Algs,[H|T], RAM_Perc, SSD_Percs, Throttling, S, Paras, AlgOfSSD, EnableMC) ->  
  loopAlg(Algs,H,RAM_Perc, SSD_Percs, Throttling, S, Paras, AlgOfSSD, EnableMC),  
  loopTrace(Algs,T, RAM_Perc, SSD_Percs,Throttling, S, Paras, AlgOfSSD, EnableMC).

loopAlg([],_Trace,_Ram,_Ssd,_Throttling,_S, _Paras, _AlgOfSSD, _EnableMC) -> ok;
loopAlg([H|T],Trace, RAM_Perc, SSD_Percs, Throttling, S, Paras, AlgOfSSD, EnableMC) ->
  
  loopSize(H,Trace,RAM_Perc,SSD_Percs,Throttling,S, Paras, AlgOfSSD, EnableMC),
  loopAlg(T,Trace, RAM_Perc, SSD_Percs, Throttling, S, Paras, AlgOfSSD, EnableMC).

start() ->
  TraceForLog = mix_p2,
   {ok, S} = file:open(getLogFile(TraceForLog), [append]),  
  % lgb or sd or idea
  Algs = [lgb],
  Traces = [mix],    
 
  % RAM_Perc = [0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001],
  % SSD_Percs = [0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1],
  RAM_Perc = [0.001],
  SSD_Percs = [0.05],

  
  WaitQ_Alg = fifo,
  SSDQ_Alg = fifo,
  AlgOfSSD = {WaitQ_Alg, SSDQ_Alg},
  EnableMC = false,
  % {good block,acc}
  %{0.1, 3, 3, 0.4}
  Coefficient = [{0.1,3,3,0.4} ], 
  % warm up = 0 mean no warm up
  Warmup = [3],
  % {Start sleep, period}, period = 1 means no sleep
  Sleep = [{50, 10}],
  Paras = {Coefficient, Warmup, Sleep},
  % per 10000
  % Throttling = [1,10,100,200],   
  Throttling = [800],   
  loopTrace(Algs,Traces, RAM_Perc, SSD_Percs, Throttling, S, Paras, AlgOfSSD, EnableMC),
  file:close(S).

% for outside call
 start(Alg,Trace, RAM_Perc, SSD_Perc,Throttling, Log, Coefficient, Warmup, Sleep, AlgOfSSD, EnableMC, VQ, VQAlg) ->
 {ok, S} = file:open(getLogFile(Log), [append]), 
 RAM_Sizes = max(5, round(RAM_Perc*trace_getTracePars:getUCLN(Trace)+0.49999)),
  % RAM_Sizes = 0,
  % SSD_Sizes = 5,
  SSD_Sizes = round(SSD_Perc*trace_getTracePars:getUCLN(Trace)+0.49999),
  io:format("Ram Size = ~p~n", [RAM_Sizes]),  
  Ram = lru,
  Ssd = Alg,  
  % Through true means, when a write quest comes, the cache block is unvalid.
  Through = true,
  {RamQ, SsdQ} = init(Ram,Ssd,Trace,RAM_Sizes, SSD_Sizes,Throttling, S, Through, Coefficient, Warmup, Sleep, AlgOfSSD, EnableMC, VQ, VQAlg), 
  io:format("throttling=~p~n",[get("throttling")]),
  % io:format("test throttling: period_length = ~p, throttling = ~p~n",[get("period_length"),get("throttling")]),
  io:format("RamQ, SsdQ : ~p~n",[{RamQ, SsdQ}]),  

   {S1, SysTime, _RamQ1, _SsdQ1, _Dict, _WaitQ} = loadTraces(trace_getTracePars:getTraceFile(Trace), S, RamQ, SsdQ),  
   io:format("~p~n", [get("ssd_evict_noaccessblock_num")]),
    io:format(S1, "RamQ, SsdQ : ~p~n",[AlgOfSSD]), 
    io:format(S1, "VQ = ~p, VQalg = ~p~n", [VQ, VQAlg]),
  case EnableMC of
    true ->
      SSDQ_result = lists:append(_SsdQ1);
    false ->
      SSDQ_result = _SsdQ1
  end,
  wec_misc:calIndex(SSDQ_result, Trace),
  wec_misc:printIndex(S1),
  wec_misc:calHit(SSDQ_result),
  wec_misc:printHit(S1, Trace),
  io:format("process finish~n"), 
  io:format("finished, SysTime = ~p~n", [SysTime]),
  io:format(S1, "throttling=~p~n",[get("throttling")]),
  io:format(S1, "finished, SysTime = ~p~n", [SysTime]),

  %io:format("finally, RamQ = ~p~n", [RamQ1]),
  %io:format("         SsdQ = ~p~n", [SsdQ2]),    
  wec_misc:printHitRate(S1),
  wec_misc:printSsdWrite(S1),  

  io:format(S1, "****************************~n~n~n~n", []).


 start(Alg,Trace, RAM_Sizes, SSD_Sizes,Throttling,S, Coefficient, Warmup, Sleep, AlgOfSSD, EnableMC) ->
  io:format("Ram Size = ~p~n", [RAM_Sizes]),  
  Ram = lru,
  Ssd = Alg,
  VQ = false,
  VQAlg = lfu,
  % Through true means, when a write quest comes, the cache block is unvalid.
  Through = true,
  {RamQ, SsdQ} = init(Ram,Ssd,Trace,RAM_Sizes, SSD_Sizes,Throttling, S, Through, Coefficient, Warmup, Sleep, AlgOfSSD, EnableMC, VQ, VQAlg), 
  io:format("throttling=~p~n",[get("throttling")]),
  % io:format("test throttling: period_length = ~p, throttling = ~p~n",[get("period_length"),get("throttling")]),
  io:format("RamQ, SsdQ : ~p~n",[{RamQ, SsdQ}]),  
 
   {S1, SysTime, _RamQ1, _SsdQ1, _Dict, _WaitQ} = loadTraces(trace_getTracePars:getTraceFile(Trace), S, RamQ, SsdQ),  
   io:format("~p~n", [get("ssd_evict_noaccessblock_num")]),
    io:format(S1, "RamQ, SsdQ : ~p~n",[AlgOfSSD]),  
    io:format(S1, "VQ = ~p, VQalg = ~p~n", [VQ, VQAlg]),
  case EnableMC of
    true ->
      SSDQ_result = lists:append(_SsdQ1);
    false ->
      SSDQ_result = _SsdQ1
  end,
  wec_misc:calIndex(SSDQ_result, Trace),
  wec_misc:printIndex(S1),
  wec_misc:calHit(SSDQ_result),
  wec_misc:printHit(S1, Trace),
  io:format("process finish~n"), 
  io:format("finished, SysTime = ~p~n", [SysTime]),
  io:format(S1, "throttling=~p~n",[get("throttling")]),
  io:format(S1, "finished, SysTime = ~p~n", [SysTime]),

  %io:format("finally, RamQ = ~p~n", [RamQ1]),
  %io:format("         SsdQ = ~p~n", [SsdQ2]),    
  wec_misc:printHitRate(S1),
  wec_misc:printSsdWrite(S1),  

  io:format(S1, "****************************~n~n~n~n", []).


  init(Ram,Ssd,Trace,RAM_Sizes, SSD_Sizes,Throttling, S, Through, Coefficient, Warmup, Sleep, AlgOfSSD, EnableMC, VQ, VQAlg) ->
  	erase(),
    put("throttling",Throttling),
  	put("ram_cache_cap", RAM_Sizes),
  	put("ssd_cache_cap", SSD_Sizes),  	
  	put("ram_alg", Ram),
  	put("ssd_alg", Ssd),    
    put("through", Through),
    put("trace",Trace),
    put("ucln",{1,lists:duplicate(10,0)}),    
    put("ssd_size",SSD_Sizes),
    put("average", 0),
    put("ideaSsdQ",[]),
    put("actualSsdQ",[]),
    put("listForIndex",[]),
    {GoodCoe, AccCoe, Cgp, Gpn} = Coefficient,
    put("logfile", "../log/debeging.log"),
    put("goodcoe", GoodCoe),
    put("acccoe", AccCoe),
    put("cgp", Cgp),
    put("gpn", Gpn),
    put("warmup", Warmup),
    {SleepStart, SleepPeriod} = Sleep,
    put("sleepstart", SleepStart),
    put("sleepperiod", SleepPeriod),
    put("lowWritePeriod",0),
    put("lessWriteNum",0),
    put("meanhit",0),
    put("nohit",0),

    {WaitQ_Alg, SSDQ_Alg} = AlgOfSSD,
    put("waitq_alg", WaitQ_Alg),
    put("ssdq_alg", SSDQ_Alg),

    put("enableMC", EnableMC),
  	io:format("RAM: size = ~p, Alg = ~p~n", [RAM_Sizes, Ram]),
  	io:format("SSD: size = ~p, Alg = ~p~n", [SSD_Sizes, Ssd]),
    io:format(S, "EnableMc = ~p, RAM Alg = ~p, SSD Alg = ~p~n", [EnableMC,WaitQ_Alg, SSDQ_Alg]),
  	io:format(S, "RAM: size = ~p, Alg = ~p~n", [RAM_Sizes, Ram]),
  	io:format(S, "SSD: size = ~p, Alg = ~p~n", [SSD_Sizes, Ssd]),
    io:format(S, "EnableMc = ~p, RAM Alg = ~p, SSD Alg = ~p~n", [EnableMC,WaitQ_Alg, SSDQ_Alg]),
  
    case Ssd of
      lgb ->
        put("period_length",getPeriodLength(Trace)),
        io:format("P = ~p~n",[round(get("period_length")*get("throttling")/10000 + 0.49999)]);
      sd ->
        put("period_length", getPeriodLength(Trace));
      idea ->
        put("period_length",trace_getTracePars:getTraceLen(Trace))
    end,

    case VQ of
      true ->
        put("vq", []),
        put("vqalg", VQAlg);
      false ->
        ok
    end,
  	
  	put("period_num",1),
  	put("unique_block",0),

    io:format("period_length: ~p~n", [get("period_length")]),
    io:format(S, "period_length: ~p~n", [get("period_length")]),
    io:format("coefficient = ~p, warm up = ~p, sleep = ~p~n", [Coefficient, Warmup, Sleep]),
    io:format(S,"coefficient = ~p, warm up = ~p, cpg =~p, gpn = ~p, sleep = ~p~n", [Coefficient, Warmup, Cgp, Gpn, Sleep]),
  	wec_misc:initStat(),   	  
    % io:format("wec_frlgb:init:Ssd=~p~n",[Ssd]),
  	SsdQ = wec_cr:initCacheReplacement(Ssd, 2, 50, hdd, 0.01, SSD_Sizes),
  	{[], SsdQ}.


  loadTraces(TraceFile, S, RamQ, SsdQ) ->
  	io:format("tracefile = ~p~n", [TraceFile]),
  	io:format(S, "tracefile = ~p~n", [TraceFile]),
  	{ok, S1} = file:open(TraceFile, read),
  	io:format("open trace file ok~n"),
    case get("enableMC") of
      true ->
        WaitQ = [[],[],[]];
      false ->
        WaitQ = []
    end,
  	readlines(S1, S, [], 0, RamQ, SsdQ, dict:new(), WaitQ).



readlines(S, S1, Reqs, SysTime, RamQ, SsdQ, Dict, WaitQ) ->
% Ã¿10000¸öÇëÇó¾Í´¦ÀíÒ»´Î£¬±ÜÃâÁÐ±í¹ý³¤
  % io:format("readlines: systime ~p~n",[SysTime]),
  
  case length(Reqs) rem 10000 =:= 9999 of
          true->
             io:format("Req Number = ~p~n",[SysTime]),
            {S2, SysTime1, RamQ1, SsdQ1, Dict1, WaitQ1} = process(S1,lists:reverse(Reqs), SysTime, RamQ, SsdQ, Dict, WaitQ),
            readlines(S, S2,[],SysTime1,RamQ1,SsdQ1,Dict1,WaitQ1);
          _->
            Line = io:get_line(S, ''),  
            
            
            case Line of 
              eof ->                 
                file:close(S),
                io:format("Req Number = ~p~n",[length(Reqs)]),
                process(S1,lists:reverse(Reqs), SysTime, RamQ, SsdQ, Dict, WaitQ);         
              _ ->             
                Res = string:tokens(Line, " ,\t\n{}"),
                case Res=:=[] of 
                  true -> readlines(S,S1, Reqs,SysTime,RamQ,SsdQ,Dict,WaitQ);
                  _ ->  
                    % line format: <Type(0-read, 1-write) DiskID(no use now) CachelineID>
                    [Type_str, DiskID_str, CachelineID_str|_] = Res,
                    {Type, _} = string:to_integer(Type_str),
                    {DiskID, _} = string:to_integer(DiskID_str),
                    {CachelineID, _} = string:to_integer(CachelineID_str),
                    readlines(S, S1, [{Type, DiskID, CachelineID}|Reqs],SysTime,RamQ,SsdQ,Dict,WaitQ)
                end
            end
  end.

process(S, [], SysTime, RamQ, SsdQ, Dict, WaitQ) -> {S, SysTime, RamQ, SsdQ, Dict,WaitQ};
process(S, [H|T], SysTime, RamQ, SsdQ, Dict, WaitQ) -> 

  
  % io:format("~p  :  ReadNum = ~p~n",[H,get("read_req_num")]),
  % io:format("Req Number = ~p~n",[length([H|T])]),
  {Type, _DiskID, CachelineID} = H,
  
  case SysTime rem get("period_length") of
    0 when SysTime > 0 ->      
      % io:format("Req Number = ~p~n",[length([H|T])]),
      % io:format(S, "Before, SsdQ = ~p~n",[SsdQ]),
      % printmq(S,SsdQ),           
      % io:format("~p,~p~n",[get("period_num"),SysTime]),
      wec_misc:setAverage(),
      % {ok, Slog} = file:open(get("logfile"), [append]),
      % io:format(Slog, "~p, ~p, ~p, ~p, ~p~n", [get("goodcoe") , get("average"), get("acccoe"), get("cgp"), get("gpn")]),
      % io:format(Slog, "waitq = {~p,~p,~p}~n", [length(lists:nth(1, WaitQ)), length(lists:nth(2, WaitQ)), length(lists:nth(3, WaitQ))]),
      % file:close(Slog),
      io:format("Period number = ~p, Waitq length = ~p~n", [get("period_num"),wec_lgb:getWaitQLength(WaitQ)]),
      % debug
      % case lists:keyfind(548584464, 1, lists:append(WaitQ)) of
      %   false ->
      %     ok;
      %   _->
      %     io:format("in waitq~n")
      % end,
      {SsdQ2, Dict2} = wec_crl:exchange(S,SsdQ, Dict, WaitQ, SysTime),
      io:format("ssdq length = ~p~n",[wec_lgb:getWaitQLength(SsdQ2)]),
      io:format("goodcoe = ~p, acccoe = ~p, average = ~p~n", [get("goodcoe"),get("acccoe") ,get("average")]),
      % case get("writessd_addinto_num") > 0 of
      %   true ->
      %     case lists:keyfind( 548584464, 1, lists:append(SsdQ2)) of
      %       false ->
      %         ok;
      %       _->
      %         io:format("in ssdq~n")
      %     end;
           
      %   false->
      %     ok
      % end,
     
      % case io:fread("input:", "~d") of
      %   -1 ->
      %     exit(0);
      %   _->
      %     ok
      % end,
      case get("enableMC") of
        true ->
          SSDQ_flat = lists:append(SsdQ2);
          false ->
            SSDQ_flat = SsdQ2
      end,
      
      wec_misc:calIndex(SSDQ_flat, get("trace")),
      % io:format("Req Number = ~p~n",[length(T)]),
      % io:format(S, "SsdQ = ~p~n",[SsdQ2]),
      case get("enableMC") of
      true ->
        WaitQ_ini = [[],[],[]];
      false ->
        WaitQ_ini = []
    end,
      process(S,[H|T], SysTime+1, RamQ, SsdQ2, Dict2, WaitQ_ini);
    _-> 
    
  case Type of 

    1 ->  % write    
      RamQ1 = wec_cr:writeReq2Cache(RamQ, ram, CachelineID),
      % io:format(S, "Before Write ,WaitQ1 = ~p~n",[WaitQ]),
      {SsdQ1,Dict1,WaitQ1} = wec_crl:writeReq2Cache(SsdQ, Dict, CachelineID,WaitQ), 
      % io:format(S,"After write: ~p~n~p,~p~n",[SsdQ1,dict:find(CachelineID,Dict1),WaitQ1]),       
      process(S,T, SysTime, RamQ1, SsdQ1, Dict1, WaitQ1);      
    0 ->  % read   
      wec_misc:isReadReq(), 
      wec_misc:checkUCLN(CachelineID,Dict),           
      {Dict1,WaitQ1} = wec_crl:register(CachelineID,SysTime,Dict,WaitQ), 

      % 1. check ram cache
      {IfHit, _, NewRamQ, _} = wec_cr:readReq2Cache(RamQ, ram, CachelineID, SysTime, false, null),      
      case IfHit of
        hit -> % ram hit                
          wec_misc:ramHit(),          
          process(S,T, SysTime+1, NewRamQ, SsdQ, Dict1, WaitQ1);
        miss -> % ram miss
          IfHit2 = wec_crl:readReq2Cache(CachelineID, SsdQ, Dict1),              
          case IfHit2 of
            hit ->
              wec_misc:ssdHit(),
              process(S,T, SysTime+1, NewRamQ, SsdQ, Dict1, WaitQ1);
            miss ->
              process(S,T, SysTime+1, NewRamQ, SsdQ, Dict1, WaitQ1)
          end
      end
    end
    % end
  end.             

