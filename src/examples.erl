-module(examples).

-import(hardwood, [insert/2, create/0, create/1]).
-import(hardwood_render, [render_digraph/2]).

-compile([export_all]).

%% foldl(Fun, Acc0, List) -> Acc1
%% Fun = fun(Elem, AccIn) -> AccOut

a() ->
    T0 = create(),
    F = fun(E,TreeIn) -> 
		io:fwrite("inserting ~p~n", [E]),
		insert(TreeIn, E)
	end,
    L=[1,2,3,6,7,8,9, 12],
    T1 = lists:foldl(F,T0,L),
    render_digraph(T1, N="a.gv"),
    png(N),
    T1.

basename(Filename) ->
    Dot = string:rchr(Filename, $.),
    string:substr(Filename, 1, Dot-1).

png(Filename) ->
    Basename = basename(Filename),
    os:cmd(io_lib:format("cat ~s | dot -Tpng -o ~s.png", [Filename, Basename])),
    os:cmd(io_lib:format("open -g ~s.png", [Basename])).
