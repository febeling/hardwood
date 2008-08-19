-module(utils).

%% Utilities

%% Fun = fun(A, Index) -> B
map_with_index(Fun, List) ->
    IncrFun = fun(A, Index) ->
		      {Fun(A, Index), Index + 1}
	      end,
    {ListM, _Acc} = lists:mapfoldl(IncrFun, 1, List),
    ListM.
