-module(hardwood).

-compile([export_all]).

-include("hardwood.hrl").

%% Tree operations
%% TODO
%% Tree = insert(Tree, Key, Value)
%% Value = lookup(Tree, Key)
%% Tree = delete(Tree, Key)

%% Tree = create(T)
create() ->
    create(2).
create(T) ->
    #btree{t=T}.

%% Node operations
%% TODO
%% Node = insert_nonfull(Node, Key)

%% bool() = is_full(Node, T)
is_full(Node, T) ->
    #node{keys = Keys} = Node,
    length(Keys) == 2 * T - 1.

median_index(List) ->
    Length = length(List),
    round(Length / 2).

%% {Index, UpdatedL} = insert_sorted(List, Value)
insert_sorted(L, V) ->
    Predicate = fun(Element) -> V > Element end,
    {LowerL, UpperL} = lists:splitwith(Predicate, L),
    Index = length(LowerL),
    {Index, lists:append([LowerL, [V], UpperL])}.

%% PNode = split(PNode, CNode, T)
split(P, C, T) ->
    true = is_full(C, T),
    M = median_index(C#node.keys),
    %% find key to up-move, split keys
    {LowerKeys, [MoveUpKey|UpperKeys]} = lists:split(M-1, C#node.keys),

    %% insert moved-up keys in parent, remember it's index
    {NewPKeyIndex, UpdatedPKeys} = insert_sorted(P#node.keys, MoveUpKey),
    case split_childs(P, NewPKeyIndex - 1) of
	%% new, empty parent node (root)
	{LowerParentChilds=[], UpperParentChilds=[]} -> ok;
	%% update parent node: remove to split node as child before replace
	%% with 2 new ones
	{LowerParentChilds, [_Skip|UpperParentChilds]} -> ok
    end,
    
    SplitIndex = length(LowerKeys),
    {LowerChilds, UpperChilds} = split_childs(C, SplitIndex),
    UpperC = C#node{keys=UpperKeys, childs=UpperChilds},
    LowerC = C#node{keys=LowerKeys, childs=LowerChilds},
    UpdatedP = P#node{keys=UpdatedPKeys, 
		      childs=lists:append([LowerParentChilds, 
					   [LowerC, UpperC], 
					   UpperParentChilds]), 
		      leaf=false},
    {UpdatedP, LowerC, UpperC}.

split_childs(#node{leaf=false, childs=Childs}=_Node, SplitIndex) ->
    LowerChilds = lists:sublist(Childs, SplitIndex + 1),
    UpperChilds = lists:sublist(Childs, 
				SplitIndex + 2, 
				SplitIndex + length(Childs) - 1),
    {LowerChilds, UpperChilds};
split_childs(#node{leaf=true}, _SplitIndex) ->
    {[], []}.


%% Test cases
%%
%% TODO

test_is_full() ->
    F=#node{keys=[a,b,c]},
    true = is_full(F, 2),
    false = is_full(F, 3),
    E=#node{},
    false = is_full(E, 2),
    ok.

test_split() ->
    ok = test_split_leaf(),
    ok = test_split_node(),
    ok = test_split_under_nonempty_parent(),
    ok.

%%       [3       8]
%% [1 2]   [4 5 7]   [9 10]
%%
%% Split to insert 6:
%%
%%       [3   5     8]
%% [1 2]   [4] [7]   [9 10]
test_split_under_nonempty_parent() ->
    T = 2,
    P = #node{keys=[3, 8], childs=[C1=make_child(1), C2=make_child_with([4, 5, 7]), C3=make_child(9)], leaf=false},
    #node{keys=[1, 2], leaf=true}=C1,
    #node{keys=[4, 5, 7], leaf=true}=C2,
    #node{keys=[9, 10], leaf=true}=C3,
    {P1, SplitLow, SplitUpper} = split(P, C2, T),
    {node, [3, 5, 8], [C1, SplitLow, SplitUpper, C3], false} = P1, 
    {node, [4], [], true} = SplitLow,
    {node, [7], [], true} = SplitUpper,
    ok.

test_split_leaf() ->
    T = 2,
    %% Case: New root through split
    C = #node{keys=[d, e, f], leaf=true},
    %%   new created root
    P = #node{keys=[], leaf=true},
    {P1, LowerChild, UpperChild} = split(P, C, T),
    %%   median key moved up into new node
    {node, [e], [LowerChild, UpperChild], false} = P1, 
    {node, [d], [], true} = LowerChild,
    {node, [f], [], true} = UpperChild,
    ok.

make_child(N) ->
    #node{keys=[N, N+1], leaf=true}.
make_child_with(Keys) ->
    #node{keys=Keys, leaf=true}.

%%     3     6     9
%% 1 2   4 5   7 8   10 11
test_split_node() ->
    T = 2,
    Grandchilds = [G1=make_child(1), G2=make_child(4), G3=make_child(7), G4=make_child(10)],
    C = #node{keys=[3, 6, 9], leaf=false, childs=Grandchilds},
    P = #node{keys=[], leaf=true},
    {P1, LowerChild, UpperChild} = split(P, C, T),
    %% the parent node has gained one key (the median) and with two childs
    {node, [6], [LowerChild, UpperChild], false} = P1,
    %% the left child has the lower keys
    {node, [3], [G1, G2], false} = LowerChild,
    %% the right child has the upper keys
    {node, [9], [G3, G4], false} = UpperChild,
    ok.

test_median_index() ->
    2 = median_index([1,    2  ,3,4]),
    2 = median_index([1,    2  ,3]),
    3 = median_index([1,2,  3  ,4,5]),
    3 = median_index([1,2,  3  ,4,5,6]),
    ok.

test() ->
    {btree, _, 2} = create(),
    {btree, _, 3} = create(3),
    ok = test_split(),
    ok = test_is_full(),
    ok = test_median_index(),
    ok.
