-module(wec_lgb).
-export([start/0,register/4,initLGB/0,readReq2Cache/3,writeReq2Cache/4,exchange/5,changeWaitQ/5, getGoodPeriod/0, getAccThrehold/0,
		calCondition/1, getWaitQLength/1, concat/3, updateSSDQitem/1, getTargetSSDQ/2]).
% -record(dictElem,{acc=1,per_acc=1,last_per_acc=0,cgp=0,present_cgp=0,gpn=0,last_acc_period=0,location=hdd,waitq=0,last_gcon=0,badblock=0}).
-record (dictElem, {acc = {1, 0, 0, 0, 0, 0, 0, 0, 0, 0}, p = 1, location = hdd, last_acc_period = 0, waitq = 0, last_gcon = 0,
	last_acc_time = 0, first_acc_time = 0}).
% p is the pointer of the present period in acc queue

% the format of dict -- dictElem
% the format of ssdq and waitq, {Cacheline, acc}
% waitq中的整数取值为0~n,表示在ssd或者waitq的第几个元素中。如果为0，则不在ssd，waitq中。
% badblock在SSD中才计算

getGoodPeriod() -> 
	case get("period_num") =< 1 of
		true ->
			3;
		false ->
			get("goodcoe") * get("average")
	end.
% getGoodPeriod() -> 3.
getBadBlock() -> 0.
getDeadWindow() ->  5.
getAccThrehold() -> 
	case get("period_num") =< 1 of
		true ->
			15;
		false ->
			get("acccoe") * get("average")
	end.
getCgpThrehold() -> get("cgp").
getGpnThrehold() -> get("gpn").
getN() -> get("warmup").



start() ->
	put("period_num",10),
	X = #dictElem{acc = {22,27,34,33,42,46,43,41,46,40},p=1,location=ssd,last_acc_period=10,waitq=6,last_gcon=2},
	io:format("~p~n",[updateSSDQitem(X)]).

getcor(Num) ->
	if
		Num =< 0 ->
			getcor(Num + 10);
		Num > 10 ->
		    getcor(Num - 10);
		true->
			Num
	end.

initLGB() ->
case get("enableMC") of
	true ->
		put("lgb_ssd_queue_num",3),	
	[[],[],[]];
	false->
		[]
end.
	

% get new pointer and acc queue
% suppose the present acc is 1, and the former periods are 0
set_elements(P, 0, Tuple) -> {P, setelement(P, Tuple, 1)};
set_elements(P, Num, Tuple) ->
	T1 = setelement(P, Tuple, 0),
	set_elements(getcor(P+1) , Num - 1, T1).

set0(P, 0, Tuple) -> {P, Tuple};
set0(P, Num, Tuple) ->
	P1 = getcor(P+1),
	T1 = setelement(P1, Tuple, 0),
	set0(P1, Num-1, T1).

updateSSDQitem(Item) ->
    Period = get("period_num"),
    Last_acc_period = Item#dictElem.last_acc_period,
    {P, Acc} = set0(Item#dictElem.p, Period - Last_acc_period, Item#dictElem.acc),
    % change the last_acc_period is to avoid repeatedly set 0
    % for example, if a block last_acc_period = 2
    % for 3: set 0
    % for 4: should set 4 as 0 and avoid setting 3 as 0 again
    % so the last_acc_period means the last time you register all the information rather than accessed
    Item#dictElem{p = P, acc = Acc, last_acc_period = Period}.



register(CacheId, SysTime, Dict, WaitQ) ->
	EnableMc = get("enableMC"),	
	Period = get("period_num"),
	VQ = get("vq"),
	% io:format("debug vq = ~p, result = ~p~n", [VQ, VQ=/=undefined]),
	% io:fread("input:", "~d"),
	% generate the new Dict item X1
	case dict:find(CacheId,Dict) of
		{ok, X} ->
			if
				X # dictElem.last_acc_period < Period-1 ->  
					{P, T} = set_elements( getcor(X#dictElem.p+1) , Period - X#dictElem.last_acc_period - 1, X#dictElem.acc),
					X1 = X#dictElem{acc = T, p = P, last_gcon = 0, last_acc_period = Period, last_acc_time=SysTime};
				X # dictElem.last_acc_period < Period ->
					P = getcor(X#dictElem.p + 1),
					T = setelement( P, X#dictElem.acc, 1),
					X1 = X#dictElem{acc = T, p = P, last_acc_period = Period, last_acc_time=SysTime};
				true ->
					T = setelement(X#dictElem.p, X#dictElem.acc, element(X#dictElem.p, X#dictElem.acc) + 1),
					X1 = X#dictElem{acc = T, last_acc_time=SysTime}
			end;
		error ->
			X1 = #dictElem {last_acc_period = Period, last_acc_time=SysTime, first_acc_time = SysTime} 
	end,
	if
		 X1#dictElem.location =:= ssd  ->
		     X2=X1,
		     WaitQ_new = WaitQ,
		     Dict_new = dict:store(CacheId,X1,Dict);
			
		true ->			
			if
				EnableMc =:= true ->					
					Target = (4 - calCondition(X1)) rem 4,    % the target queue of waitq
					Source = X1#dictElem.waitq,		 	
					X2 = X1#dictElem{waitq = Target, last_gcon = (4-Source) rem 4},
					Dict_new = 	dict:store(CacheId,X2,Dict),
					WaitQ_new= changeWaitQ(CacheId,Target,Source,WaitQ, Dict_new),
					if
						VQ /= undefined ->					
							VQ_new = mc_algorithm:updateWaitQ(CacheId, VQ, get("vqalg"), Dict_new),					
							% io:format("debug cacheid = ~p, vq = ~p~n", [CacheId, VQ_new]),
							% io:fread("input:", "~d"),
							put("vq", VQ_new);							
						true ->
							ok
					end;
				true ->     % disable MC, requests dirctly enter waitq
				    Alg = get("waitq_alg"),
				    X2 = X1#dictElem{waitq = 1},
				    Dict_new = dict:store(CacheId,X2,Dict),
				    WaitQ_new = mc_algorithm:updateWaitQ(CacheId, WaitQ, Alg, Dict_new)
			end

			
	end,
	% if
	% 	CacheId =:= 548584464->
	% 		io:format("~p~n", [X2]);
	% 	true->
	% 		ok
	% end,
	{Dict_new, WaitQ_new}.
	
	

check_acc(Acc) ->
	case Acc<getAccThrehold() of
		true ->
			0;
		_->
			1
	end.

check_cgp(X) ->	
	
	N = calCGP(X#dictElem.acc, X#dictElem.p, 0),
	case N < getCgpThrehold() of
		true ->
			0;
		_->
		    1
	end.

check_gpn(X) ->
	N = calGoodPeriod(X#dictElem.acc, 10, 0),
	case N/min(get("period_num"),10) < getGpnThrehold() of
		true ->
			0;
		_->
		    1
	end.

calCGP(_, _, 10) -> 10;
calCGP(Tuple, P, Num) ->	
	case element(P, Tuple) < getGoodPeriod()  of
		true ->
			Num;
		_->
			calCGP(Tuple, getcor(P-1), Num + 1)
	end.
		 
calGoodPeriod(_, 0, Num) -> Num;			
calGoodPeriod(T, P, Num) ->
	case element(P, T) < getGoodPeriod() of
		true->
			calGoodPeriod(T, P-1, Num);
		_->
			calGoodPeriod(T, P-1, Num + 1)
	end.
	



% check the number of satisfied conditions, return a integer 0~3
calCondition(X) ->	
	
	% io:format("begin calCondition~n"),

	% io:format("~p~n", [X#dictElem.acc]),
	% io:format("~p~n", [tuple_to_list(X#dictElem.acc)]),
	% io:format("~p~n", [lists:sum(tuple_to_list(X#dictElem.acc))]),
	% try check_acc(lists:sum(tuple_to_list(X#dictElem.acc))) of
	% 	_ ->
	% 		ok
	% catch
	% 	Exception : Reason ->
	% 		io:format("~p, ~p, ~p~n", [Exception, Reason, X])
	
	% end,
	
	% io:format("check_acc:~p~n",[A]),
	B = check_cgp(X),
	% io:format("check_cgp:~p~n",[B]),
	C = check_gpn(X),
	% io:format("check_gpn:~p~n",[C]),						
	check_acc(lists:sum(tuple_to_list(X#dictElem.acc)))+B+C.


% 将WaitQ中原来在M级的元素放到N级
% N is the present order, M is the original order
changeWaitQ(CacheId,Target,Source,WaitQ, Dict) ->	
			if
				Source=:=0 ->
					WaitQ1 = WaitQ;
				true ->  % delete the original element in WaitQ
					WaitQ1 = concat(Source,lists:keydelete(CacheId,1,lists:nth(Source,WaitQ)),WaitQ)
					% io:format("WaitQ1 = ~p~n",[WaitQ1])
			end,
			if
				Target=:=0 ->
					WaitQ1;
				true->
					Alg = get("waitq_alg"),
					List_target = lists:nth(Target, WaitQ1),
					List_target_update = mc_algorithm:updateWaitQ(CacheId, List_target, Alg, Dict),
					concat(Target, List_target_update, WaitQ1)
			end	
	.	
	% io:format("WaitQ=~p~nWaitQ1=~p~n",[WaitQ,WaitQ1]).


readReq2Cache(CacheId, _SsdQ, Dict) ->
	{ok,X} = dict:find(CacheId,Dict),
	if
	 	X#dictElem.location=:=ssd ->
	 		wec_misc:addHit(CacheId),
	 		hit;
	 	true ->
	 		miss
	 end.

writeReq2Cache(SsdQ, Dict, CachelineID,WaitQ) ->
	Through = get("through"),
	if
		Through=:=true ->    % invalib the block in SSD
			case dict:find(CachelineID,Dict) of
				{ok, X} ->
					if
	 				X#dictElem.location=:=ssd ->
	 					N = X#dictElem.waitq,	 		
	 					X1 = X#dictElem{location=hdd,waitq = 0},
	 					SSDQ_new = deleteElementFromSSDQ(CachelineID, N, SsdQ),	 					
	 					Dict1 = dict:store(CachelineID,X1,Dict),
	 					wec_misc:calHitElem(CachelineID),
	 					% debug
	 					% if
	 					% 	CachelineID =:= 548584464 ->
	 					% 		io:format("~p, ~p~n", [lists:keyfind(CachelineID, 1, lists:append(SsdQ)), lists:keyfind(CachelineID, 1, lists:append(SSDQ_new))]),
	 					% 		{ok, S} = file:open("./test.log",[append]),
	 					% 		io:format(S, "~p~n", [lists:nth(N,SsdQ)]),
	 					% 		file:close(S),
	 					% 		io:format("X=~p, length of old ssdq = ~p, length of new ssdq = ~p~n",[X, getWaitQLength(SsdQ), getWaitQLength(SSDQ_new)]);

	 					% 	true ->
	 					% 		ok
	 					% end,
	 					{SSDQ_new,Dict1,WaitQ};
	 				true ->
	 					{SsdQ,Dict,WaitQ}
	 				end;
				error ->
	 				{SsdQ,Dict,WaitQ}
			end;
		true ->
			case dict:find(CachelineID,Dict) of
				error->
					{SsdQ, Dict, WaitQ};
				{ok, X}->
					if
	 				X#dictElem.location=:=ssd ->
				    	wec_misc:ssdAddinto(),
				    	{SsdQ, Dict, WaitQ};
				    true ->
				    	{SsdQ, Dict, WaitQ}
					end		
			end	
	end.
	

% 将Mq中第N个位置替换为NthElement
concat(N, NthElement, Mq) ->

	lists:sublist(Mq, N-1) ++ [NthElement] ++ lists:sublist(Mq, N+1, length(Mq)-N).



deleteElementFromSSDQ(CachelineID, N, SsdQ) ->
    case N=:=0 of
     	true ->
     		lists:delete(CachelineID, SsdQ);
     	false ->
     	
     	    ListNth = lists:keydelete(CachelineID,1,lists:nth(N,SsdQ)),
	 		concat(N,ListNth,SsdQ)
     end.



checkDeadWindow(X) -> 
	M = calBL(X#dictElem.acc, X#dictElem.p, 0),			
	case M rem getDeadWindow() =:= 0 of
		true->
			if
				M =:= 0 ->
					A = 0;
				true ->
					A = 1
			end;
		_->
		    A = 0
	end,	
	A.
calBL(_, _, 10) -> 10;

calBL(Acc, P, Num) ->
	case element(P, Acc) =< getBadBlock() of
		true ->
			calBL(Acc, getcor(P+1), Num+1);
		_->
			Num
	end.

		




sort([],L) -> lists:reverse(L);
sort([H|T],L) ->
	sort(T,[lists:reverse(lists:keysort(2,H))|L]).

			



getList([], _Dict, Ele) -> 
	Ele;
getList([H|T], D, {La,Lc,Lg,Lac,Lag,Lcg,Lacg}) -> 
	{Cache, _} = H,
	{ok, X} = dict:find(Cache, D),
	A = check_acc(lists:sum(tuple_to_list(X#dictElem.acc))),
	B = check_cgp(X),
	C = check_gpn(X),
	case {A,B,C} of
		{0,0,0} ->
			getList(T, D, {La,Lc,Lg,Lac,Lag,Lcg,Lacg});
		{1,0,0} ->
			getList(T, D, {[H|La],Lc,Lg,Lac,Lag,Lcg,Lacg});
		{0,1,0} ->
			getList(T, D, {La,[H|Lc],Lg,Lac,Lag,Lcg,Lacg});
		{0,0,1} ->
			getList(T, D, {La,Lc,[H|Lg],Lac,Lag,Lcg,Lacg});
		{1,1,0} ->
			getList(T, D, {[H|La],[H|Lc],Lg,[H|Lac],Lag,Lcg,Lacg});
		{1,0,1} ->
			getList(T, D, {[H|La],Lc,[H|Lg],Lac,[H|Lag],Lcg,Lacg});
		{0,1,1} ->
			getList(T, D, {La,[H|Lc],[H|Lg],Lac,Lag,[H|Lcg],Lacg});
		{1,1,1} ->
			getList(T, D, {[H|La],[H|Lc],[H|Lg],[H|Lac],[H|Lag],[H|Lcg],[H|Lacg]})
	end.

% was in SSDQ
getTargetSSDQ(Item, true) -> 

	Gcon_old = Item#dictElem.last_gcon,
	Gcon_new = calCondition(Item),
	Dead = checkDeadWindow(Item),
	Queue_old = Item#dictElem.waitq,
	Target = Queue_old - Gcon_new + Gcon_old + Dead,
	case Target > get("lgb_ssd_queue_num") of
		true ->
			put("lgb_ssd_queue_num", Target);
		_->
		    ok
	end,
	% io:format("Item = ~p,old gc = ~p, new gc = ~p, dead = ~p, target = ~p~n", [Item, Gcon_old, Gcon_new, Dead, Target]),
	max(Target, 1);
% was in waitq
getTargetSSDQ(Item, false) -> 	
	4 - calCondition(Item).


	

exchange(_S,SsdQ, Dict, WaitQ, _SysTime) ->
	P = get("period_num"),	
	SleepStart = get("sleepstart"),
	SleepPeriod = get("sleepperiod"),
	EnableMc = get("enableMC"),
	WaitQ_alg = get("waitq_alg"),
	SSDQ_alg = get("ssdq_alg"),
	VQ = get("vq"),
	
	if
		WaitQ =:= [] ->
	 		{SsdAns,DictAns} = {SsdQ, Dict};
	 	true ->
	 	if
		% sleep period, no update
	 	(P > SleepStart) and (P rem SleepPeriod /= 0) ->
	 		% io:format("attention!!~n"),
	 		put("vq", []),
	 		{SsdAns,DictAns} = {SsdQ, Dict};
	 	% waitq is empty, no update
	 	
	 	true ->    
			case get("ssd_addinto_num")=:=0  of
				true->
					% io:format("true~n"),
					ok;
				false ->
					% wec_sd:printIndex(_S,lists:append(SsdQ)),			
					put("actualSsdQ",SsdQ)
			end,	
			% io:format("beginning of function~n"),
			% the beginning of exchange function
			% get the enter queue and actual update number
			case get("period_num") =< getN() of
				true ->	
					% warm up 					
					N = get("ssd_cache_cap") div getN();
				_ ->					
					N = round(get("period_length")*get("throttling")/10000 + 0.49999)
			end,
			Length = getWaitQLength(WaitQ),
			case Length < N of
						true ->
							put("lowWritePeriod",get("lowWritePeriod")+1),
							put("lessWriteNum",get("lessWriteNum")+N-Length),			
							N1 = Length;
						_->				
							N1 = N
			end,
			Enter = mc_algorithm:getNHeadWaitQ(N1, WaitQ, EnableMc),
			case VQ of
				undefined ->
					Enter_vq = [];
				_->

					Enter_vq = mc_algorithm:getNHeadWaitQ(N - N1, VQ--Enter, false),
					put("vq", [])
					% io:format("debug number = ~p, vq = ~p~n",[N-N1, Enter_vq]),
					% io:fread("input:", "~d")
			end,
			
			{Ssd_Ordered, Dict_new} = mc_algorithm:orderSSDQ(SsdQ, Dict, EnableMc, SSDQ_alg),

			case getWaitQLength(Ssd_Ordered) + N =< get("ssd_cache_cap") of
				true ->
					Evict = [];
				false ->
					Evict = mc_algorithm:getNTailSSDQ(N + getWaitQLength(Ssd_Ordered) - get("ssd_cache_cap"), Ssd_Ordered, EnableMc)
			end,
			{SsdAns,DictAns} = mc_algorithm:updateSSDQ(Enter ++ Enter_vq, Evict,  Ssd_Ordered, Dict_new, SSDQ_alg, EnableMc),

			% debug
			% io:format("~p~n", [dict:find(20647, DictAns)]),

			% case lists:keyfind(20647, 1, Enter++Enter_vq) of
			% 				false->
			% 					ok;
			% 				_ ->
			% 					io:format("in enter queue~n")
								
							
			% 			end,
			% case lists:keyfind(20647, 1, Evict) of
			% 				false ->
			% 					ok;
			% 				_->
			% 					io:format("in evict queue~n")
			% 			end,		

			% io:fread("input:", "~d"),	
			% io:format(_S,"exchange:Evict = ~p~n",[Evict]),

			wec_misc:calHit(Evict),
			case N1 < N of
				true ->
					put("lowWritePeriod",get("lowWritePeriod")+1),
					put("lessWriteNum",get("lessWriteNum")+N-N1);
				_->
					ok
			end,
	
			io:format("actual update number = ~p~n", [N]),
			wec_misc:ssdAddinto(N)
	
	

	end,
	put("period_num",get("period_num")+1),
	LowWritePeriod = get("lowWritePeriod"),
	LessWriteNum = get("lessWriteNum"),
	PeriodTemp = get("period_num") - 1,
	if
		P < 4 ->
			ok;  % warm up stage, the n is abnormal
		LowWritePeriod >= 3 ->
			put("goodcoe", get("goodcoe")*0.8),
			put("acccoe", get("acccoe")*0.8),
			put("cgp", get("cgp")*0.8),
			put("gpn", get("gpn")*0.8),
			put("lowWritePeriod",0),
			put("lessWriteNum",0);
		LessWriteNum >= 300 ->
			put("goodcoe", get("goodcoe")*0.8),
			put("acccoe", get("acccoe")*0.8),
			put("cgp", get("cgp")*0.8),
			put("gpn", get("gpn")*0.8),
			put("lowWritePeriod",0),
			put("lessWriteNum",0);
		true ->
			ok
	end,
	{SsdAns,DictAns}
end.


% multiqueue
getWaitQLength(WaitQ) ->
	case get("enableMC") of
		true ->
			length(lists:append(WaitQ));
		false ->
			length(WaitQ)
	end.



% 从SSD中删除List包含的元素，List = [Cacheid]。
delList_Ssd([],Ssd,Dict) -> {Dict,Ssd};
delList_Ssd([H|T],Ssd,Dict) ->
	{CacheId,_} = H, 
	{ok, X} = dict:find(CacheId,Dict),
	N = X#dictElem.waitq,
	X1 = X#dictElem{location=hdd,waitq=0},
	Dict1 = dict:store(CacheId,X1,Dict),	
	Ssd1 = concat(N,lists:keydelete(CacheId,1,lists:nth(N,Ssd)),Ssd),
	delList_Ssd(T,Ssd1,Dict1).



