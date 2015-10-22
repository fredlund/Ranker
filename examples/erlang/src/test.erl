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
	 #implementation{id=Dir,private=list_to_atom("reverse"++Index)}
     end, Dirs),
  ranker:classify
    (corr,
     erlang_recipe,
     [],
     Implementations).
