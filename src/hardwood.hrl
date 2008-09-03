-record(node, {
	  keys = [],
	  childs = [],
	  leaf = true
	 }
       ).
-record(tree, {
	  root = #node{}, 
	  t = 2 % max 2t childs, max 2t-1 keys
	 }).
