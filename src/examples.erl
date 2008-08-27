-module(examples).

-include("hardwood.hrl").

-import(hardwood, [insert/2, create/0, create/1]).
-import(hardwood_render, [render_digraph/1, render_digraph/2]).
-import(io, [fwrite/2]).
-import(lists, [seq/2, map/2]).

-compile([export_all]).

%% foldl(Fun, Acc0, List) -> Acc1
%% Fun = fun(Elem, AccIn) -> AccOut

roll(L) ->
    fwrite("**********************************************~n", []),
    T0 = create(),
    F = fun(E,{TreeIn, N}) -> 
		io:fwrite("[~p] inserting ~p~n", [N, E]),
		TreeOut = insert(TreeIn, E),
		fwrite("root after insert: ~p~n", [TreeOut#btree.root]),
		fwrite("~s~n", [render_digraph(TreeOut)]),
		{TreeOut, N+1}
	end,
    {T1,_} = lists:foldl(F,{T0,1},L),
    fwrite("gv:~n~s~n", [render_digraph(T1)]),
    render_digraph(T1, N="a.gv"),
    png(N),
    T1.

i() ->
    T = create(),
    iloop(T).

iloop(T) ->
    iloop(io:get_line('> '), T).
    
iloop("e" ++ _Rest, _T) ->
    ok;
iloop(In, T) ->
    {N, _} = string:to_integer(In),
    T1 = insert(T, N),
    render_digraph(T1, Name="tmp_i.gv"),
    png(Name),
    iloop(io:get_line('> '), T1).

a() ->
    roll([1,2,3,6,7,8,9,12]).

b() ->
    roll([1,9,2,8,3,7,4,6,5]).

c() ->
    random:seed(0,2,7),
    Rand = fun(_) ->
		   random:uniform(100) + 255
	   end,
    L = map(Rand, seq(1,20)),
    roll(L).

basename(Filename) ->
    Dot = string:rchr(Filename, $.),
    string:substr(Filename, 1, Dot-1).

png(Filename) ->
    Basename = basename(Filename),
    os:cmd(io_lib:format("cat ~s | dot -Tpng -o ~s.png", [Filename, Basename])),
    os:cmd(io_lib:format("open ~s.png", [Basename])).

test() ->
    %% todo
    ok.
