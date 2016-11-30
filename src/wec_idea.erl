-module (wec_idea).
-export ([initIDEA/0,writeReq2Cache/4,readReq2Cache/2]).

getFilePath() -> "../trace/allrank_".

getRankFile(Trace) ->
case Trace of	
    finance -> getFilePath() ++ "web2.csv";
    websearch1 -> getFilePath() ++ "web1.csv";
    websearch2 -> getFilePath() ++ "web2.csv";
    build1 -> 927691;
    build2 -> 957596;
    cctv1 -> 550310;
    cctv2 -> 508280;
    as1 -> 409918;
    as2 -> 215678;
    mix -> 8296749;
    test -> 10
  
end.

getUCLNBlock(Trace) ->
case Trace of
    websearch1 -> 5518
end.


initIDEA() ->
	Trace = get("trace"),
	File = getRankFile(Trace),
    
	{ok, S} = file:open(File,read),
    % io:format("wec_idea:initIDEA:SsdSize = ~p~n",[get("ssd_cache_cap")]),
    {ok, S1} = file:open("../log/list.csv",write),
	% L = read(S,get("ssd_cache_cap"),[]),
    L = readList(S,getUCLNBlock(Trace)),
    wec_misc:ssdAddinto(length(L)),

    print(S1, 0, L),
    file:close(S1),
    io:format("~p",[0/0]),
	L.

print(S1, _, []) -> ok;
print(S1, 10, L) -> io:format(S1, "~n", []), print(S1, 0, L);
print(S1, Num, [H|L]) ->
    io:format(S1, "~p,", [H]),
    print(S1, Num+1, L).

read(S,0,L) -> file:close(S),L;
read(S,N,L) -> 
% io:format("wec_idea:read:Num = ~p~nS=~p~n",[N,S]),
	Line = io:get_line(S, ''),
    case Line of 
        eof ->                 
            file:close(S),
            L;         
        _ ->             
            Res = string:tokens(Line, " ,\t\n"),
            [Cache_str|_] = Res,
            {Cache, _} = string:to_integer(Cache_str),
            read(S,N-1,[Cache|L])
        end.


readList(S,N) -> 
% io:format("wec_idea:read:Num = ~p~nS=~p~n",[N,S]),
    Line = io:get_line(S, ''),
    case Line of 
        eof ->                 
            file:close(S),
            [];         
        _ ->             
            Res = string:tokens(Line, " ,\t\n"),
            [Cache_str,Acc_str|_] = Res,
            {Cache, _} = string:to_integer(Cache_str),
            {Acc,_} = string:to_integer(Acc_str),
            readList(S,N-1,[{Cache,1}],1,Acc)
        end.

readList(S,0,L,_Ord,_Acc) -> lists:reverse(L);
readList(S, Num, L, Ord, Acc) ->
    Line = io:get_line(S, ''),
    case Line of 
        eof ->                 
            file:close(S),
            lists:reverse(L);         
        _ ->             
            Res = string:tokens(Line, " ,\t\n"),
            [Cache_str,Acc_str|_] = Res,
            {Cache, _} = string:to_integer(Cache_str),
            {EAcc,_} = string:to_integer(Acc_str),
            if
                EAcc < Acc ->
                    readList(S, Num-1, [{Cache,length(L)+1}|L], length(L)+1, EAcc);
                true ->
                    readList(S, Num-1, [{Cache,Ord}|L], Ord, Acc)
            end
    end.

% {SsdQ1,Dict1,WaitQ1}
writeReq2Cache(SsdQ, Dict, CachelineID,WaitQ) ->
	case get("through") of
		true ->
			{lists:delete(CachelineID,SsdQ),Dict,WaitQ};
		false ->
			wec_misc:ssdAddinto(),
			{SsdQ, Dict, WaitQ}
	end.

% hit or miss
readReq2Cache(CachelineID,SsdQ) ->
	case lists:member(CachelineID,SsdQ) of
		true ->			
            hit;
        false->
            miss
	end.