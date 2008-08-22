-module(hardwood_render).

-compile([export_all]).

-include("hardwood.hrl").

-import(utils, [map_with_index/2]).
-import(lists, [seq/2, append/2, map/2, mapfoldl/3, zip/2]).
-import(string, [join/2]).

%%{Nodes, Edges} = render_digraph(Node, T)
render_digraph(Tree) when is_record(Tree, btree) ->
    Node = Tree#btree.root,
    render_digraph("root_node", Node, 0, [], []).

corner(1) ->
    "sw";
corner(N) when is_integer(N) ->
    "se".

field(0) ->
    "f0";
field(N) when is_integer(N) ->
    io_lib:format("f~p", [N-1]).

render_edges(ParentNum, ChildNums) ->
    F = fun(ChildNum, SibNum) ->
		{io_lib:format("struct~p:~s:~s -> struct~p:n;", 
			       [ParentNum, field(SibNum), corner(SibNum), ChildNum]),
		 SibNum + 1}
	end,
    {Edges, _SibNum} = mapfoldl(F, 0, ChildNums),
    Edges.

render_digraph(Name, Node, SeqNum, NodesAcc, EdgesAcc) when is_list(Name), is_record(Node, node) ->
    NodesAcc1    = [render_node(Name, Node)|NodesAcc],
    F = fun(Child, {SeqNum0, NodesAcc0, EdgesAcc0}) ->
		{SubNodesList, SubEdgesList, SeqNum1} = render_digraph(node_name(SeqNum0), Child, SeqNum0, NodesAcc0, EdgesAcc0),
		{SeqNum0, {SeqNum1, SubNodesList, SubEdgesList}}
	end,
    {ListOfNums, {SeqNumNext, SubNodesList, SubEdgesList}} = mapfoldl(F, {SeqNum + 1, NodesAcc1, EdgesAcc}, Node#node.childs),
    EdgesAcc1    = render_edges(SeqNum, ListOfNums),
    {SubNodesList, append(EdgesAcc1,SubEdgesList), SeqNumNext}.

render_node(NodeName, Node) ->
    io_lib:format("~s ~s", [NodeName, render_keys(Node#node.keys)]).

node_name(SeqNum) ->
    io_lib:format("struct~B", [SeqNum]).

render_keys(Keys) ->
    Fn = fun(Key, Index) -> io_lib:format("<f~p> ~p", [Index, Key]) end,
    Label = join(map_with_index(Fn, Keys), "|"),
    io_lib:format("[label=\"~s\"]", [Label]).
