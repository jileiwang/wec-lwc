-module(wec_time).
-export([getSSDLatency/1,getSsdTime/4]).

%-------------获取每个SSD_Cacheline的时间信息-------------				  
   getSsdTime(Type, CachelineID, AccessTime, SNST) ->
              CachelineStartTime = max(AccessTime,SNST),%-----获取块开始执行的时间，SNST
			  {Latency}=getSSDLatency(Type),%-----根据访问模式获取延时 coolling time为操作延时
			  TLatency = CachelineStartTime+Latency-AccessTime,%-----总延时=开始处理时间+延时-访问时间
			  {CD}=getSsdcd(Type),%----得到ssd的冷却时间
			  NewSNST =  CachelineStartTime + CD,%-------得到新的SNST			  
	         {NewSNST, TLatency}.
			  
   getSSDLatency(QuestType) -> %---返回SSD的读/写延时
        case QuestType of
             1 -> %-----write-----
                  Latency=0;%0.433;%--------随机写延时--0.433ms------
             0 -> %-----read-----
                  Latency=0%0.13%----随机读延时--0.13ms------
		end,
        {Latency}.

    
   getSsdcd(Type) ->
        case Type of
		1 ->
			 CdWrite=0,%0.054,
			 {CdWrite};
		0 ->
			 CdRead=0,%0.016,
			 {CdRead}
        end.
   
   
   
   
   
   
              
