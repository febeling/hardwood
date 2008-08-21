-module(hardwood_render).

-compile([export_all]).

-include("hardwood.hrl").

-import(utils, [map_with_index/2]).
-import(lists, [seq/2, append/2, map/2, mapfoldl/3]).

%%{Nodes, Edges} = render_digraph(Node, T)
render_digraph(Tree) ->
    Node = Tree#btree.root,
    render_digraph("root_node", Node, 0, [], []).

%% render_edges(Childs) TODO

render_digraph(Name, Node, SeqNum, NodesAcc, EdgesAcc) ->
    NodesAcc1    = [render_node(Name, Node)|NodesAcc],
%%    ChildSeqNums = seq(SeqNum + 1, SeqNum + length(Node#node.childs)),
    EdgesAcc1    = [ "edge_todo"
		    %%render_edges(Node#node.childs)
		    |EdgesAcc],

    %% mapfoldl(F, Acc, L) -> {L1, Acc1}
    %% Fun = fun(A, AccIn) -> {B, AccOut}

    F = fun(Child, {SeqNum0, NodesAcc0, EdgesAcc0}) ->
		{SubNodesList, SubEdgesList, SeqNum1} = render_digraph(node_name(Child, SeqNum0), Child, SeqNum0, NodesAcc0, EdgesAcc0),
		{ok, {SeqNum1 + 1, SubNodesList, SubEdgesList}}
    end,

    {_ListOfOks, {SeqNumNext, SubNodesList, SubEdgesList}} = mapfoldl(F, {SeqNum, NodesAcc1, EdgesAcc1}, Node#node.childs),
    {SubNodesList, SubEdgesList, SeqNumNext}.
    
%% Join strings in a list together with a given other string
%% join([string()], string()) -> iolist()
join(Strings, Separator) ->
    join(Strings, Separator, []).
join([H | T], Sep, []) ->
    join(T, Sep, [H]);
join([H | T], Sep, Acc) ->
    join(T, Sep, [H, Sep | Acc]);
join([], _Sep, Acc) ->
    lists:reverse(Acc).

render_node(NodeName, Node) ->
    io_lib:format("~s ~s", [NodeName, render_keys(Node#node.keys)]).

%% node_name(Node, Counter)
node_name(_Node, SeqNum) ->
    io_lib:format("struct~B", [SeqNum]).

%% render_keys(Keys) -> string()
render_keys(Keys) ->
    Fn = fun(Key, Index) -> io_lib:format("<f~p> ~p", [Index, Key]) end,
    Label = join(map_with_index(Fn, Keys), "|"),
    io_lib:format("[label=\"~s\"]", [Label]).

