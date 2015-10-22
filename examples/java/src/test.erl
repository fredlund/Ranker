-module(test).

-compile(export_all).

test() ->
  Dirs = 
    filelib:wildcard("imp*"),
  Implementations =
    lists:map
      (fun (Dir) ->
	   {Dir,Dir++"/classes"}
       end, Dirs),
  Recipe = 
    java_reuse_recipe:make_recipe
      (node_spec(),
       ["SortArr.SortArr"], %% list of classes to be "reused"
       [], %% classpath
       100,
       10,
       false),
  DistPlan = [{local,25,info}],
  ranker:classify
    (corr,
     java_reuse_recipe,
     [Recipe,DistPlan],
     Implementations,
     []).
     
node_spec() ->
  [{log_level,warning},
   {java_verbose,"SEVERE"},
%%   {java_verbose,"WARNING"},
   {java_exception_as_value,true},
%%   {java_timeout_as_value,true},
   {call_timeout,20000}].
