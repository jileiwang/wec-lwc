-module (wec_crl).
-export ([exchange/5,register/4,writeReq2Cache/4,readReq2Cache/3]).




exchange(S,SsdQ, Dict, WaitQ,SysTime) ->
	Alg = get("ssd_alg"),
	case Alg of
		lgb ->
			wec_lgb:exchange(S,SsdQ, Dict, WaitQ, SysTime);
		sd ->
			wec_sd:exchange(S,SsdQ, Dict, WaitQ)
	end.

% {Dict1,WaitQ1}
register(CachelineID,SysTime,Dict,WaitQ) ->
	Alg = get("ssd_alg"),
	
	case Alg of
		lgb ->
			wec_lgb:register(CachelineID,SysTime,Dict,WaitQ);
		sd ->
			wec_sd:register(CachelineID,SysTime,Dict,WaitQ);
		idea ->
			{Dict,WaitQ}
	end.

% {SsdQ1,Dict1,WaitQ1}
writeReq2Cache(SsdQ, Dict, CachelineID,WaitQ) ->
	Alg = get("ssd_alg"),
	case Alg of
		lgb ->
			wec_lgb:writeReq2Cache(SsdQ, Dict, CachelineID,WaitQ);
		sd ->
			wec_sd:writeReq2Cache(SsdQ, Dict, CachelineID,WaitQ);
		idea->
			wec_sd:writeReq2Cache(SsdQ, Dict, CachelineID,WaitQ)
	end.

% hit or miss 
readReq2Cache(CachelineID,SsdQ, Dict) ->
	Alg = get("ssd_alg"),
	case Alg of
		lgb ->
			wec_lgb:readReq2Cache(CachelineID,SsdQ, Dict);
		sd ->
			wec_sd:readReq2Cache(CachelineID,SsdQ, Dict);
		idea ->
			wec_idea:readReq2Cache(CachelineID,SsdQ) 
	end.