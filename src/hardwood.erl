-module(hardwood).

-compile([export_all]).

-import(lists, [map/2, seq/2, seq/3, nth/2, split/2, nthtail/2, append/1, append/2, sublist/2, sublist/3]).
-import(utils, [map_with_index/2]).
-import(io, [fwrite/2]).

-include("hardwood.hrl").

%% Tree operations

%% Value = lookup(Tree, Key)
%% Tree = delete(Tree, Key)

%% Tree = insert(Tree, Key, Value)
insert(Tree, Key, Value) when is_record(Tree, tree) ->
    NewTree = case is_full(Tree#tree.root, Tree#tree.t) of
		  true ->
		      NewRoot = split_child(#node{}, Tree#tree.root, Tree#tree.t),
		      Tree#tree{root=NewRoot};
		  false ->
		      Tree
	      end,
    NewNode = insert_nonfull(NewTree#tree.root, Key, Value, NewTree#tree.t),
    NewTree#tree{root=NewNode}.

%% Tree = create(T)
create() ->
    create(2).

create(T) ->
    #tree{t=T}.

%% Node operations

%% Node = insert_nonfull(Node, Key, T)
insert_nonfull(Node, Key, Value, T) when is_record(Node, node) ->
    false = is_full(Node, T),
    case Node#node.leaf of
	true ->
	    Keys = Node#node.keys,
	    {_Index, NewKeys} = insert_sorted(Keys, Key),
	    %% TODO insert value
	    Node#node{keys=NewKeys};
	false ->
	    Index = child_insert_index(Node#node.keys, Key),
	    InsertChild = nth(Index, Node#node.childs),
	    {NewNode, NewInsertChild} = 
		case is_full(InsertChild, T) of
		    true ->
			NewNode2 = split_child(Node, InsertChild, T),
			{NewNode2, nth(child_insert_index(NewNode2#node.keys, Key), NewNode2#node.childs)};
		    false ->
			{Node, InsertChild}
		end,
	    UpdatedInsertChild = insert_nonfull(NewInsertChild, Key, Value, T),
	    Index2 = child_insert_index(NewNode#node.keys, Key),
	    {Before, [_ReplaceChild|After]} = split(Index2-1, NewNode#node.childs),
	    NewChilds = append(Before, [UpdatedInsertChild | After]),
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
    GreaterThanP = fun(Element) -> V > Element end,
    {LowerL, UpperL} = lists:splitwith(GreaterThanP, L),
    Index = length(LowerL),
    {Index, lists:append(LowerL, [V | UpperL])}.

%% Split child C and move one key up into P
%% PNode = split_child(PNode, CNode, T)
split_child(P, C, T) ->
    true = is_full(C, T),
    M = median_index(C#node.keys),
    {_LowerKeys, [MoveUpKey | _UpperKeys]} = split(M-1, C#node.keys),
    {NewPKeyIndex, UpdatedPKeys} = insert_sorted(P#node.keys, MoveUpKey),
    C2 = update_childs(C, M, P#node.childs, NewPKeyIndex),
    P#node{keys=UpdatedPKeys, 
	   childs=C2,
	   leaf=false}.

update_childs(C, M, Pchilds, MovedUpKeyIndex) ->
    {LowerKeys, [_MoveUpKey | UpperKeys]} = split(M-1, C#node.keys),
    case split_childs(Pchilds, MovedUpKeyIndex) of
	{[]=LowerParentChilds, []=UpperParentChilds} ->
	    ok;
	{LowerParentChilds, [_Skip | UpperParentChilds]} -> 
	    ok
    end,
    {LowerChilds, UpperChilds} = split_childs(C#node.childs, M),
    UpperC = C#node{keys=UpperKeys, childs=UpperChilds},
    LowerC = C#node{keys=LowerKeys, childs=LowerChilds},    
    append([LowerParentChilds,  [LowerC, UpperC],  UpperParentChilds]).

%% Split the childs list (like the one of an internal node), or do
%% nothing when leaf
split_childs([], _SplitIndex) ->
    {[], []};
split_childs(Childs, SplitIndex) when is_list(Childs), is_integer(SplitIndex) ->
    split(SplitIndex, Childs).

%% Test cases

%% Fixtures

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

%% Fixture tree with this structure:
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
    #tree{root=Node}.

%% Test cases

%% An internal node gets split, at median index of their keys
%% array. The childs get split as well and that depends on this same
%% median index.
test_split_childs() ->
    P0 = make_leaf([5, 9, 13]),
    P = P0#node{leaf=false, childs=[C1 = make_leaf([3]),
				    C2 = make_leaf([7]),
				    C3 = make_leaf([11]),
				    C4 = make_leaf([15])]},
    {[C1], [C2, C3, C4]} = split_childs(P#node.childs, 1), %% unused case in btree
    {[C1, C2], [C3, C4]} = split_childs(P#node.childs, 2), %% median index
    {[C1, C2, C3], [C4]} = split_childs(P#node.childs, 3), %% unsed case in btree
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
    UpdatedNode = insert_nonfull(NonFullNode, 9, _Value = 0, T),
    #node{keys=[7,8,9]} = UpdatedNode,
    %% insert provoking error
    FullNode = make_leaf([4, 5, 6]),
    {'EXIT', {{badmatch,true}, _}} = (catch insert_nonfull(FullNode, 9, _Value = 0, T)),
    ok.

test_insert_nonfull_recursive() ->
    T = 2,
    A = #node{keys=[4],
	      childs=[#node{keys=[3]},
		      #node{keys=[7]}],
	      leaf=false},
    _B = #node{keys=[4],
	       childs=[#node{keys=[3]},
		       #node{keys=[6,7]}],
	       leaf=false},
    #node{keys=[4], childs=_, leaf=false} = insert_nonfull(A, 6, _Value = 0, T), 
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
    P1 = split_child(P, C, T),
    #node{keys=[e], childs=[LowerChild, UpperChild], leaf=false} = P1, 
    #node{keys=[d]} = LowerChild,
    #node{keys=[f]} = UpperChild,
    ok.

test_split_under_nonempty_parent() ->
    T = 2,
    P = #node{keys=[3, 8], childs=[C1=make_leaf(1), C2=make_leaf([4, 5, 7]), C3=make_leaf(9)], leaf=false},
    #node{keys=[1, 2]} =    C1,
    #node{keys=[4, 5, 7]} = C2,
    #node{keys=[9, 10]} =   C3,
    P1 = split_child(P, C2, T),
    #node{keys=[3, 5, 8], childs=[C1, SplitLow, SplitUpper, C3], leaf=false} = P1, 
    %% C1 [1, 2]
    #node{keys=[4], childs=[]} = SplitLow,
    #node{keys=[7], childs=[]} = SplitUpper,
    %% C3 [9, 10]
    ok.

test_split_first_child_under_nonempty_parent() ->
    T = 2,
    C1 = #node{keys=[2, 4, 6]},
    C2 = #node{keys=[9, 10]},
    P = #node{keys=[7], childs=[C1, C2], leaf=false},
    P1 = split_child(P, C1, T),
    #node{keys=[4, 7], childs=[LowerChild, UpperChild, C2], leaf=false} = P1, 
    #node{keys=[2]} = LowerChild,
    #node{keys=[6]} = UpperChild,
    %% C2 [9, 10]
    ok.

test_split_node() ->
    T = 2,
    Grandchilds = [G1=make_leaf(1), G2=make_leaf(4), G3=make_leaf(7), G4=make_leaf(10)],
    C = #node{keys=[3, 6, 9], leaf=false, childs=Grandchilds},
    P = #node{keys=[]},
    P1 = split_child(P, C, T),
    %% the parent node has gained one key (the median) and with two childs
    #node{keys=[6], childs=[LowerChild, UpperChild], leaf=false} = P1,
    %% the left child has the lower keys
    #node{keys=[3], childs=[G1, G2], leaf=false} = LowerChild,
    %% the right child has the upper keys
    #node{keys=[9], childs=[G3, G4], leaf=false} = UpperChild,
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
    #tree{t=2} = create(),
    #tree{t=3} = create(3),
    ok = test_split(),
    ok = test_insert(),
    ok = test_is_full(),
    ok = test_median_index(),
    ok = test_child_insert_index(),
    ok = test_insert_into_node_nonfull(),
    ok = test_map_with_index(),
    ok.
