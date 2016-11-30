-module (wec_sd).
-export ([exchange/4,register/3,writeReq2Cache/4,readReq2Cache/3,getRankList/1]).
% -record(cacheline, {fileid=-1, access_num=1, last_access_time=-1, expireTime=-1, location=hdd, type=undefined}).
-record (dictElem, {acc = {1, 0, 0, 0, 0, 0, 0, 0, 0, 0}, p = 1, location = hdd, last_acc_time = 0,  waitq = undefined, last_gcon = undefined}).


getExchangeMode() ->
	% oncebyten, sd+throttling, normal
	throttling.


readReq2Cache(CachelineID, SsdQ, _Dict)->
	case lists:keyfind(CachelineID,1,SsdQ) of
		false ->
			miss;
		_->
			hit
	end.


writeReq2Cache(SsdQ, Dict, CachelineID,WaitQ) ->
	Through = get("through"),
	if
		Through=:=true ->
			SsdQ1 = lists:keydelete(CachelineID,1,SsdQ),
			{SsdQ1, Dict, WaitQ};
		true ->
			case lists:keyfind(CachelineID,1,SsdQ) of
				false->
					ok;
				_->
				    wec_misc:ssdAddinto()
			end,
			{SsdQ, Dict, WaitQ}
	end.
	

exchange(_S,SsdQ, Dict, _WaitQ) ->
	case getExchangeMode() of
		oncebyten ->
			case get("period_num") of
				10 ->
					StoreDic = dict:to_list(Dict),
					Store = lists:map(fun(X) -> {Id,{_,Acc,_P,_Loc,_Time,_,_}} = X,
					{Id,lists:sum(tuple_to_list(Acc))}
					end,StoreDic),	
					Order = lists:keysort(2,Store),		
					N = get("ssd_size"),
					if
						length(Order)<N ->
							SsdQ1 = Order;
						true ->
							SsdQ1 = lists:sublist(Order,length(Order)-N+1,N)
					end,
					printIndex(_S, SsdQ1),
					lists:foreach(fun(X) -> {CacheId,_} = X,case lists:keyfind(CacheId,1,SsdQ) of
						false ->
							wec_misc:ssdAddinto();			
						_->
							ok
					end end, SsdQ1);	

				_->
					SsdQ1 = SsdQ
			end;
		throttling->
			case get("ssd_addinto_num") >= get("throttling") of
				true ->
					SsdQ1 = SsdQ;
			_->
				StoreDic = dict:to_list(Dict),
					Store = lists:map(fun(X) -> {Id,{_,Acc,_P,_Loc,_Time,_,_}} = X,
					{Id,lists:sum(tuple_to_list(Acc))}
					end,StoreDic),	
					Order = lists:keysort(2,Store),		
					N = get("ssd_size"),
					if
						length(Order)<N ->
							SsdQ1 = Order;
						true ->
							SsdQ1 = lists:sublist(Order,length(Order)-N+1,N)
					end,
					printIndex(_S, SsdQ1),
					lists:foreach(fun(X) -> {CacheId,_} = X,case lists:keyfind(CacheId,1,SsdQ) of
						false ->
							wec_misc:ssdAddinto();			
						_->
							ok
					end end, SsdQ1)
			end;
		_->
			StoreDic = dict:to_list(Dict),
					Store = lists:map(fun(X) -> {Id,{_,Acc,_P,_Loc,_Time,_,_}} = X,
					{Id,lists:sum(tuple_to_list(Acc))}
					end,StoreDic),	
					Order = lists:keysort(2,Store),		
					N = get("ssd_size"),
					if
						length(Order)<N ->
							SsdQ1 = Order;
						true ->
							SsdQ1 = lists:sublist(Order,length(Order)-N+1,N)
					end,
					printIndex(_S, SsdQ1),
					lists:foreach(fun(X) -> {CacheId,_} = X,case lists:keyfind(CacheId,1,SsdQ) of
						false ->
							wec_misc:ssdAddinto();			
						_->
							ok
					end end, SsdQ1)
	end,
	put("period_num", get("period_num")+1),
	{SsdQ1,Dict}.



getcor(Num) ->
	if
		Num =< 0 ->
			getcor(Num + 10);
		Num > 10 ->
		    getcor(Num - 10);
		true->
			Num
	end.

set_elements(P, 0, Tuple) -> {P, setelement(P, Tuple, 1)};
set_elements(P, Num, Tuple) ->
	T1 = setelement(P, Tuple, 0),
	set_elements(getcor(P+1) , Num - 1, T1).

register(CacheId,Dict,_WaitQ) ->
	Period = get("period_num"),	
	case dict:find(CacheId,Dict) of
		{ok, X} ->
			if
				X # dictElem.last_acc_time < Period-1 ->
					{P, T} = set_elements( getcor(X#dictElem.p+1) , Period - X#dictElem.last_acc_time - 1, X#dictElem.acc),
					X1 = X#dictElem{acc = T, p = P, last_acc_time = Period};
				X # dictElem.last_acc_time < Period ->
					P = getcor(X#dictElem.p + 1),
					T = setelement( P, X#dictElem.acc, 1),
					X1 = X#dictElem{acc = T, p = P, last_acc_time = Period};
				true ->
					T = setelement(X#dictElem.p, X#dictElem.acc, element(X#dictElem.p, X#dictElem.acc) + 1),
					X1 = X#dictElem{acc = T}
			end;
		error ->
			X1 = #dictElem {last_acc_time = Period} 
	end,
	Dict1 = dict:store(CacheId,X1,Dict),
	{Dict1,[]}.