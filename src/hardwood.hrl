
-record(node, {
	  keys =   [],
	  values = [],
	  childs = [],
	  leaf =   true
	 }
       ).

 % max 2t childs, max 2t-1 keys
-record(tree, {
	  root = #node{}, 
	  t =    2
	 }).
