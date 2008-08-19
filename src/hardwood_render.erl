-module(hardwood_render).

-compile([export_all]).

-include("hardwood.hrl").

-import(utils, [map_with_index/2]).

%%{Nodes, Edges} = render_digraph(Node, T)
render_digraph(Tree) ->
    Node = Tree#btree.root,
    render_digraph("root_node", Node, 0, [], []).

%%   % Name       = node_name(Node, SeqNum),

%% render_digraph(Node, NodeName, SeqNum, NodesStringsAcc, EdgesStringsAcc) ->
%%     % NodeString     = render_node(NodeName, Node),
%%     % SeqNums        = seq(SeqNum + 1, SeqNum + length(Node#node.childs)),
%%     % ChildNodeNames = map(fun node_name/2, Node#node.childs, SeqNums),
%%     % {LastSeqNum, NodesStringsAccM, EdgesStringsAccM} = render_digraph(),
render_digraph(Name, Node, N, NodesAcc, EdgesAcc) ->
    NodesAcc1    = [render_node(Name, Node)|NodesAcc],
    ChildSeqNums = seq(SeqNum + 1, SeqNum + length(Node#node.childs)),
    EdgesAcc1    = [render_edges(Node#node.childs)|EdgesAcc]

%render node from self
%render edges from above

    
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

