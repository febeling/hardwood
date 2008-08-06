-module(hardwood).

-compile([export_all]).

-include("hardwood.hrl").

%% Tree operations

%% Tree = create(T)
%% Tree = insert(Tree, Key, Value)
%% Value = lookup(Tree, Key)
%% Tree = delete(Tree, Key)

create() ->
    create(2).
create(T) ->
    #btree{t=T}.

%% Node operations

%% PNode = split(PNode, CNode, T)
%% Node = insert_nonfull(Node, Key)
%% bool() = is_full(Node, T)

is_full(Node, T) ->
    #node{keys = Keys} = Node,
    length(Keys) == 2 * T - 1.

median_index(List) ->
    Length = length(List),
    round(Length / 2) - 1.

%% {Index, UpdatedL} = 
insert_sorted(L, V) ->
    Predicate = fun(Element) -> V > Element end,
    {LowerL, UpperL} = lists:splitwith(Predicate, L),
    Index = length(LowerL),
    {Index, lists:append([LowerL, [V], UpperL])}.

split(P, C, T) ->
    true = is_full(C, T),
    Keys = C#node.keys,
    M = median_index(Keys),
    {LowerKeys, [MoveUpKey|UpperKeys]} = lists:split(M, Keys),
    UpperC = C#node{keys=UpperKeys},
    LowerC = C#node{keys=LowerKeys},
    {NewPKeyIndex, UpdatedPKeys} = insert_sorted(P#node.keys, MoveUpKey),
    LowerChilds = lists:sublist(P#node.childs, NewPKeyIndex),
    UpperChilds = lists:sublist(P#node.childs, NewPKeyIndex + 1, NewPKeyIndex + length(P#node.childs)),
    UpdatedP = P#node{keys=UpdatedPKeys, childs=lists:append([LowerChilds, [LowerC, UpperC], UpperChilds]), 
		     leaf=false},
    {UpdatedP, LowerC, UpperC}.

%% TODO childs splitten

%% Test cases

test_is_full() ->
    F=#node{keys=[a,b,c]},
    true = is_full(F, 2),
    false = is_full(F, 3),
    E=#node{},
    false = is_full(E, 2),
    ok.

test_split() ->
    T = 2,
    C = #node{keys=[d, e, f], leaf=true}, % Case: New root through split
    P = #node{keys=[], leaf=true},
    {P1, _, _} = split(P, C, T),
    {node, [e], [C1, C2], false} = P1, % 1 key moved up into new node
    {node, [d], [], true} = C1,
    {node, [f], [], true} = C2,
    ok.

test_median_index() ->
    1 = median_index([1,   2 ,3,4]),
    1 = median_index([1,   2 ,3]),
    2 = median_index([1,2, 3 ,4,5]),
    2 = median_index([1,2, 3 ,4,5,6]),
    ok.

test() ->
    {btree, _, 2} = create(),
    {btree, _, 3} = create(3),
    ok = test_split(),
    ok = test_is_full(),
    ok = test_median_index(),
    ok.
