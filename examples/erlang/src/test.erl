-module(test).

-include("implementation.hrl").
-compile(export_all).

test() ->
  Dirs =
    filelib:wildcard("imp*"),
  Implementations =
    lists:map
    (fun (Dir) ->
	 Index = string:substr(Dir,4),
	 {Dir,list_to_atom("reverse"++Index)}
     end, Dirs),
  ranker:classify
    (corr,
     erlang_recipe,
     [],
     Implementations,
     [{timeout,500}]).
