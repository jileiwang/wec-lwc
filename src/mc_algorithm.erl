-module(mc_algorithm).
-export([updateWaitQ/4, updateSSDQ/6, getNHeadWaitQ/3, orderSSDQ/4, getNTailSSDQ/3]).

-record (dictElem, {acc = {1, 0, 0, 0, 0, 0, 0, 0, 0, 0}, p = 1, location = hdd, last_acc_period = 0, waitq = 0, last_gcon = 0,
	last_acc_time = 0, first_acc_time = 0}).

% input: CacheID, WaitQ(a flat list, no inner list structure), Alg(lfu, lru, fifo), 
% Dict(used to get information, no change)
% output: WaitQ
% function : when a new quest CacheID arrives, update WaitQ based on Alg
updateWaitQ(CacheID, WaitQ, Alg, Dict) ->


case Alg of
	lfu ->
	    WaitQ1 = lists:keydelete(CacheID, 1, WaitQ),
		{ok,X} = dict:find(CacheID, Dict),		
		AccTuple = X#dictElem.acc,
		Acc = lists:sum(tuple_to_list(AccTuple)),
		No_Order_WaitQ = [{CacheID,Acc}|WaitQ1],
		% debug
		% io:format("~p~n",[length(No_Order_WaitQ)]),
		Order_WaitQ = lists:reverse(lists:keysort(2, No_Order_WaitQ)),
		Order_WaitQ;
	lru ->
	    WaitQ1 = lists:delete(CacheID, WaitQ),
		[CacheID|WaitQ1];
	fifo ->
	    % io:format("fifo"),
	    case lists:member(CacheID, WaitQ) of
	    	true ->
	    		WaitQ;
	    	false ->
	    	    [CacheID|WaitQ]
	    end		
end.

% multi queue
getNHeadWaitQ(N, WaitQ, EnableMC) ->
case EnableMC of
	% multi queue
	true -> 
		lists:sublist(lists:append(WaitQ), N);
	% flat queue
	false ->
	    lists:sublist(WaitQ, N)
end.


getNTailSSDQ(N, SSDQ, EnableMC) ->
case EnableMC of
	% multi queue
	true -> 
		L = lists:append(SSDQ),
		lists:sublist(L, length(L) - N + 1, N);
	% flat queue
	false ->
	    lists:sublist(SSDQ, length(SSDQ) - N + 1, N)
end.



% input : SSDQ(no order, the information out of time), Dict(information is correct, but need to update new queue number for MC)
% output : new dict(for period algorithm, no need to change), new SSDQ
orderSSDQ(SSDQ, Dict, EnableMC, Alg) ->
Period = get("period_num"),
Length = wec_lgb:getWaitQLength(SSDQ),
% debug
% io:format("~p,~p~n",[Length, SSDQ]),
if
	Length=:=0 ->
		{SSDQ, Dict};
	true ->

case EnableMC of
	% multi queue
	true -> 
				
		% io:format("~p~n", [Tag_list]),
		{Dict_new, Tag_list} = getTagList(lists:append(SSDQ),Dict, Alg, []),
		SSDQ_ordered = dealwith(Tag_list, Alg),

		{SSDQ_ordered, Dict_new};
	% flat queue
	false ->
		List_update = lists:map(fun(A) ->
				
				{CacheID,Acc} = A,				
				{ok, X} = dict:find(CacheID, Dict),				
				X_update=wec_lgb:updateSSDQitem(X),
				case Alg of
					lfu ->
						Order_sign = lists:sum(tuple_to_list(X_update#dictElem.acc));
					lru-> 
						Order_sign = X_update#dictElem.last_acc_time;
					_-> 
						Order_sign = X_update#dictElem.first_acc_time
				end, {CacheID, Order_sign} end, SSDQ),
		{lists:reverse(lists:keysort(2, List_update)), Dict}    
end
end.

getTagList([], Dict, _, List) -> {Dict, List};
getTagList([H|T], Dict, Alg, List) ->		
		{CacheID,_} = H,				
		{ok, X} = dict:find(CacheID, Dict),	
		X_update=wec_lgb:updateSSDQitem(X),
		case Alg of
			lfu ->
				Order_sign = lists:sum(tuple_to_list(X_update#dictElem.acc));
			lru-> 
				Order_sign = X_update#dictElem.last_acc_time;
			_-> 
				Order_sign = X_update#dictElem.first_acc_time
		end,
		Target = wec_lgb:getTargetSSDQ(X_update, true),
		X_final = X_update#dictElem{waitq = Target},
		Dict_new = dict:store(CacheID, X_final, Dict),
				
					% debug
	% 	if
	% 		CacheID =:= 548584464 ->
	% 		io:format("sort this block in ssdq~n before : ~p~n, update : ~p~n, after : ~p~n",[X, X_update, X_final]),
	% 			io:format("~p~n", [dict:find(CacheID, Dict_new)]),
	% 			case io:fread("input:", "~d") of
 %        "-1" ->
 %          exit(0);
 %        _->
 %          ok
 %      end;
	% 	true ->
	% 		ok
	% end,		
	getTagList(T, Dict_new, Alg, [{Target, {CacheID, Order_sign}}|List]).		
			
				
		
dealwith(List, Alg) ->
	dealwith(get("lgb_ssd_queue_num"), List,[], Alg).

dealwith(0, _, SSDQ, _) -> SSDQ;
dealwith(Num, List, SSDQ, Alg) ->
	NList = lists:filter(fun(A)->
			{Target, Tuple} = A,
			if
				Target=:=Num ->
					true;
				true->
				    false
			end
	end, List),
	NList_no_order = lists:map(fun
		(A)->
			{_Target, Tuple} = A,
			Tuple
	end, NList),
	% all the algorithms will put the bigger sign at priority
	Nlist_Order = lists:reverse(lists:keysort(2, NList_no_order)),
	if
		SSDQ =:= [] ->
			Result = [Nlist_Order];
		true ->
			
		    Result = [Nlist_Order|SSDQ]

	end,
	% io:format("num = ~p, result = ~p~n", [Num, Result]),
	dealwith(Num-1, List, Result, Alg).

updateSSDQ(EnterQ, Evict, SSDQ,  Dict, Alg, EnableMc) -> 
% debug
	
	% io:format("before update : ssdq = ~p, ~n", [SSDQ]),
	{SSDQ_delete, Dict_delete} = deleteSSDQItem(Evict, SSDQ, Dict, EnableMc),
	% io:format("after delete : ssdq = ~p, ~n", [SSDQ_delete]),
	{SSDQ_enter, Dict_enter} = addSSDItem(EnterQ, SSDQ_delete, Dict_delete, Alg, EnableMc),
	% io:format("after update : ssdq = ~p, ~n", [SSDQ_enter]),
	{SSDQ_enter, Dict_enter}.


addSSDItem([], SSDQ, Dict, _Alg, _EnableMc) -> {SSDQ, Dict};
addSSDItem([H|T], SSDQ, Dict, Alg, EnableMc) ->
	
	case get("waitq_alg") of
		lfu ->
			{CacheID, Tag} = H,
			{ok, Inf} = dict:find(CacheID, Dict);
		lru->
			CacheID = H,
			{ok, Inf} = dict:find(CacheID, Dict),
			Tag = Inf#dictElem.last_acc_time;
		fifo ->
			CacheID = H,
			{ok, Inf} = dict:find(CacheID, Dict),
			Tag = Inf#dictElem.first_acc_time
	end,
	
	case EnableMc of
		true ->
		    
			Queue_Number = min(4 - wec_lgb:calCondition(Inf),3),
			
			
			case wec_lgb:getWaitQLength(SSDQ) of
				0 ->
					List_nth = [];
				_->
				    List_nth = lists:nth(Queue_Number, SSDQ)
			end,
			
			SSDQ_enter = wec_lgb:concat(Queue_Number, lists:reverse(lists:keysort(2, [{CacheID, Tag}|List_nth])), SSDQ),
			% io:format("debug ssdq = ~p~n", [SSDQ_enter]),
			Inf_new = Inf#dictElem{waitq = Queue_Number, location = ssd, last_gcon = 4-Queue_Number},
			Dict_enter = dict:store(CacheID, Inf_new, Dict);
		false ->
			SSDQ_enter = lists:reverse(lists:keysort(2, [{CacheID,Tag}|SSDQ])),
			Inf_new = Inf#dictElem{waitq = 0, location = ssd},
			Dict_enter = dict:store(CacheID, Inf_new, Dict)

	end,
	% debug
	% if
	% 	CacheID =:= 548584464 ->
	% 		io:format("add this block~n ~p~n",[Inf_new]);
	% 	true ->
	% 		ok
	% end,
	addSSDItem(T, SSDQ_enter, Dict_enter, Alg, EnableMc).


deleteSSDQItem([], SSDQ, Dict, _) -> {SSDQ, Dict};
deleteSSDQItem([H|T], SSDQ, Dict, true) ->
	{CacheID, _} = H,
	{ok, Inf} = dict:find(CacheID, Dict),
	Ssdq_number = Inf#dictElem.waitq,
	try wec_lgb:concat(Ssdq_number,lists:keydelete(CacheID,1,lists:nth(Ssdq_number,SSDQ)),SSDQ) 
	catch
		Exception : Reason ->
			io:format("~p, ~p, ~p, ~p~n", [CacheID, Inf, Exception, Reason]),
			exit(-1)
	end,
	SSDQ_delete = wec_lgb:concat(Ssdq_number,lists:keydelete(CacheID,1,lists:nth(Ssdq_number,SSDQ)),SSDQ), 
	Inf_new = Inf#dictElem{location=hdd, waitq = 0},
	Dict_delete = dict:store(CacheID, Inf_new, Dict),
	% debug
	if
		CacheID =:= 548584464 ->
			io:format("delete this block~n ~p~n",[Inf_new]);
		true ->
			ok
	end,
	deleteSSDQItem(T, SSDQ_delete, Dict_delete, true);
deleteSSDQItem([H|T], SSDQ, Dict, false) ->
	{CacheID, _} = H,
	{ok, Inf} = dict:find(CacheID, Dict),	
	SSDQ_delete = lists:keydelete(1, CacheID, SSDQ),
	Inf_new = Inf#dictElem{location=hdd, waitq = 0},
	Dict_delete = dict:store(CacheID, Inf_new, Dict),

	deleteSSDQItem(T, SSDQ_delete, Dict_delete, false).