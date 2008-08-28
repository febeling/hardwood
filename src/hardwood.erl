-module(hardwood).

-compile([export_all]).

-import(lists, [map/2, seq/2, seq/3, nth/2, split/2, append/1, append/2, sublist/2, sublist/3]).
-import(utils, [map_with_index/2]).
-import(io, [fwrite/2]).

-include("hardwood.hrl").

%% Tree operations

%% TODO
%% Value = lookup(Tree, Key)
%% Tree = delete(Tree, Key)

%% Tree = insert(Tree, Key, Value)
insert(Tree, Key) when is_record(Tree, btree) ->
    NewTree = case is_full(Tree#btree.root, Tree#btree.t) of
		  true ->
		      {NewRoot,_,_} = split(#node{}, Tree#btree.root, Tree#btree.t),
		      Tree#btree{root=NewRoot};
		  false ->
		      Tree
	      end,
    NewNode = insert_nonfull(NewTree#btree.root, Key, NewTree#btree.t),
    NewTree#btree{root=NewNode}.

%% Tree = create(T)
create() ->
    create(2).
create(T) ->
    #btree{t=T}.

%% Node operations

%% Node = insert_nonfull(Node, Key, T)
insert_nonfull(Node, Key, T) when is_record(Node, node) ->
    false = is_full(Node, T),
    case Node#node.leaf of
	true ->
	    Keys = Node#node.keys,
	    {_Index, NewKeys} = insert_sorted(Keys, Key),
	    Node#node{keys=NewKeys};
	false ->
	    Index = child_insert_index(Node#node.keys, Key),
	    InsertChild = nth(Index, Node#node.childs),
	    {NewNode, NewInsertChild} = 
		case is_full(InsertChild, T) of
		    true ->
			{NewNode2, _, _} = split(Node, InsertChild, T),
			{NewNode2, nth(child_insert_index(NewNode2#node.keys, Key), NewNode2#node.childs)};
		    false ->
			{Node, InsertChild}
		end,
	    UpdatedInsertChild = insert_nonfull(NewInsertChild, Key, T),
	    Index2 = child_insert_index(NewNode#node.keys, Key),
	    {Before, [_ReplaceChild|After]} = split(Index2-1, NewNode#node.childs),
	    NewChilds = append(Before, [UpdatedInsertChild|After]),
	    NewNode#node{childs=NewChilds}
    end.

child_insert_index(Keys, Key) ->
    child_insert_index(Keys, Key, 1).

child_insert_index([], _Key, Index) ->
    Index;
child_insert_index(Keys, Key, Index) ->
    [H|T] = Keys,
    case Key =< H of
	true ->
	    Index;
	false ->
	    child_insert_index(T, Key, Index + 1)
    end.

median_index(List) ->
    Length = length(List),
    round(Length / 2).

%% bool() = is_full(Node, T)
is_full(Node, T) when is_record(Node, node) ->
    #node{keys = Keys} = Node,
    length(Keys) == 2 * T - 1.

%% {Index, UpdatedL} = insert_sorted(List, Value)
insert_sorted(L, V) ->
    Predicate = fun(Element) -> V > Element end,
    {LowerL, UpperL} = lists:splitwith(Predicate, L),
    Index = length(LowerL),
    {Index, lists:append([LowerL, [V], UpperL])}.

%% Split child C and move one key up into P
%% PNode = split(PNode, CNode, T)
split(P, C, T) ->
    true = is_full(C, T),
    M = median_index(C#node.keys),
    %% find moving-up key of child (index = median)
    {LowerKeys, [MoveUpKey | UpperKeys]} = split(M-1, C#node.keys),
    %% insert at parent keys at index
    {NewPKeyIndex, UpdatedPKeys} = insert_sorted(P#node.keys, MoveUpKey),
    %% split parent childs at 
    case split_childs(P, NewPKeyIndex) of
	{[]=LowerParentChilds, []=UpperParentChilds} when length(LowerParentChilds) == 0, length(UpperParentChilds) == 0 -> 
	    ok;
	{LowerParentChilds, [_ | UpperParentChilds]} -> 
	    ok
    end,
    {LowerChilds, UpperChilds} = split_childs(C, M),
    UpperC = C#node{keys=UpperKeys, childs=UpperChilds},
    LowerC = C#node{keys=LowerKeys, childs=LowerChilds},    
    C2 = append([LowerParentChilds,  [LowerC, UpperC],  UpperParentChilds]),
    UpdatedP = P#node{keys=UpdatedPKeys, 
		      childs=C2,
		      leaf=false},
    {UpdatedP, LowerC, UpperC}.

%% Split the childs of an internal node, or do nothing when leaf
%% split_childs(#node{leaf=false, childs=Childs}, N) when N =< 0->
%%     {[], Childs};
split_childs(#node{leaf=false, childs=Childs}, SplitIndex) ->
    LowerChilds = sublist(Childs, SplitIndex),
    UpperChilds = sublist(Childs, 
			  SplitIndex+1, 
			  length(Childs)), %% ok to be longer
    {LowerChilds, UpperChilds};
split_childs(#node{leaf=true}, _SplitIndex) ->
    {[], []}.

%% Test cases
%%
%% TODO
%% insert into non-leaf node (only for extern insert)
%% find child to insert into

%% helpers

make_leaf(N) when is_integer(N) ->
    #node{keys=[N, N+1], leaf=true};
make_leaf(Keys) when is_list(Keys) ->
    #node{keys=Keys, leaf=true}.

make_subtree() ->
    #node{keys=[3, 8, 10], 
	  childs=[make_leaf(1), 
		  make_leaf([4, 5, 7]), 
		  make_leaf(9), 
		  #node{keys=[11,12], leaf=false, childs=make_leaf(200)}], 
	  leaf=false}.

%%                                        16       
%%       4       8         12                          20          24            28
%% 1.2.3   5.6.7   9.10.11    13.14.15        17.18.19    21.22.23     25.26.27      29.30.31
make_tree1() ->
    C1 = [make_leaf([1,2,3]),
	  make_leaf([5,6,7]),
	  make_leaf([9,10,11]),
	  make_leaf([13,14,15])],
    C2 = [make_leaf([17,18,19]),
	  make_leaf([21,22,23]),
	  make_leaf([25,26,27]),
	  make_leaf([29,30,31])],
    P1 = #node{leaf=false, 
	       keys=[4,8,12],
	       childs=C1},
    P2 = #node{leaf=false,
	       keys=[20,24,28],
	       childs=C2},
    G = #node{leaf=false,
	      keys=[16],
	      childs=[P1,P2]},
    make_tree(G).

make_tree() ->
    make_tree(make_subtree()).

make_tree(Node) ->
    #btree{root=Node}.

%% test cases

%% An internal node gets split, at median index of their keys
%% array. The childs get split as well and that depends on this same
%% median index.
test_split_childs() ->
    P0 = make_leaf([5, 9, 13]),
    P = P0#node{leaf=false, childs=[C1 = make_leaf([3]),
				    C2 = make_leaf([7]),
				    C3 = make_leaf([11]),
				    C4 = make_leaf([15])]},
    {[C1], [C2, C3, C4]} = split_childs(P, 1), %% unused case in btree
    {[C1, C2], [C3, C4]} = split_childs(P, 2), %% median index
    {[C1, C2, C3], [C4]} = split_childs(P, 3), %% unsed case in btree
    ok.

test_child_insert_index() ->
    4 = child_insert_index([2, 4, 6], 7),
    3 = child_insert_index([2, 4, 6], 5),
    2 = child_insert_index([2, 4, 6], 3),
    1 = child_insert_index([2, 4, 6], 1),
    ok.

test_insert_into_node_nonfull() ->
    T = 2,
    %% main success case
    NonFullNode = make_leaf(7),
    UpdatedNode = insert_nonfull(NonFullNode, 9, T),
    #node{keys=[7,8,9]} = UpdatedNode,
    %% insert provoking error
    FullNode = make_leaf([4, 5, 6]),
    {'EXIT', {{badmatch,true}, _}} = (catch insert_nonfull(FullNode, 9, T)),
    ok.

test_insert_nonfull_recursive() ->
    T = 2,
    A = {node,[4],
	 [{node,[3],[],true},
	  {node,[7],[],true}],
	 false},
    _B = {node,[4],
	  [{node,[3],[],true},
	   {node,[6,7],[],true}],
	  false},
    {node, [4], _, false} = insert_nonfull(A, 6, T), 
    ok.

test_is_full() ->
    F=#node{keys=[a,b,c]},
    true = is_full(F, 2),
    false = is_full(F, 3),
    E=#node{},
    false = is_full(E, 2),
    ok.

test_split() ->
    ok = test_split_childs(),
    ok = test_split_leaf(),
    ok = test_split_node(),
    ok = test_split_under_nonempty_parent(),
    ok = test_split_first_child_under_nonempty_parent(),
    ok.

test_insert() ->
    ok = test_insert_nonfull_recursive(),
    ok.

test_split_leaf() ->
    T = 2,
    C = #node{keys=[d, e, f]},
    P = #node{keys=[]},
    {P1, LowerChild, UpperChild} = split(P, C, T),
    {node, [e], [LowerChild, UpperChild], false} = P1, 
    {node, [d], [], true} = LowerChild,
    {node, [f], [], true} = UpperChild,
    ok.

test_split_under_nonempty_parent() ->
    T = 2,
    P = #node{keys=[3, 8], childs=[C1=make_leaf(1), C2=make_leaf([4, 5, 7]), C3=make_leaf(9)], leaf=false},
    #node{keys=[1, 2]} =    C1,
    #node{keys=[4, 5, 7]} = C2,
    #node{keys=[9, 10]} =   C3,
    {P1, SplitLow, SplitUpper} = split(P, C2, T),
    %% C1 [1, 2]
    #node{keys=[4], childs=[], leaf=true} = SplitLow,
    #node{keys=[7], childs=[], leaf=true} = SplitUpper,
    %% C3 [9, 10]
    #node{keys=[3, 5, 8], childs=[C1, SplitLow, SplitUpper, C3], leaf=false} = P1, 
    ok.

test_split_first_child_under_nonempty_parent() ->
    T = 2,
    C1 = #node{keys=[2, 4, 6]},
    C2 = #node{keys=[9, 10]},
    P = #node{keys=[7], childs=[C1, C2], leaf=false},
    {P1, LowerChild, UpperChild} = split(P, C1, T),
    {node, [4, 7], [LowerChild, UpperChild, C2], false} = P1, 
    {node, [2], [], true} = LowerChild,
    {node, [6], [], true} = UpperChild,
    %% C2 [9, 10]
    ok.

test_split_node() ->
    T = 2,
    Grandchilds = [G1=make_leaf(1), G2=make_leaf(4), G3=make_leaf(7), G4=make_leaf(10)],
    C = #node{keys=[3, 6, 9], leaf=false, childs=Grandchilds},
    P = #node{keys=[]},
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

test_map_with_index() ->
    Fn = fun(A, Index) -> A * 2 + Index end,
    L = [0, 1, 2, 3],
    [1, 4, 7, 10] = map_with_index(Fn, L),
    ok.

test() ->
    {btree, _, 2} = create(),
    {btree, _, 3} = create(3),
    ok = test_split(),
    ok = test_insert(),
    ok = test_is_full(),
    ok = test_median_index(),
    ok = test_child_insert_index(),
    ok = test_insert_into_node_nonfull(),
    ok = test_map_with_index(),
    ok.
