-module(utils).

-compile([export_all]).

%% Utilities

%% Fun = fun(A, Index) -> B
map_with_index(Fun, List) ->
    IncrFun = fun(A, Index) ->
		      {Fun(A, Index), Index + 1}
	      end,
    {ListM, _Acc} = lists:mapfoldl(IncrFun, 1, List),
    ListM.

%% print list elements as strings
puts(L) -> 
    lists:map(fun(E) -> io:fwrite("~s~n", [E]), 
			ok 
	      end, 
	      L).
