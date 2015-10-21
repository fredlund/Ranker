-module(ranker_render).
-compile(export_all).

render(StatemModule) ->
  fun (Commands) ->
      Result =
	StatemModule:render_commands
	(lists:foldl
	   (fun (Command,Acc) -> StatemModule:render(Command,Acc) end,
	    StatemModule:render_init(), Commands)),
      RendResult =
	case Result of
	  _ when is_list(Result) -> Result;
	  Other ->
	    io:format
	      ("*** Warning: render of~n~p~ndoes not return a list but~n~p~n",
	       [Commands,Other]),
	    throw(badarg)
	end,
      io_lib:write(lists:reverse(RendResult))
  end.
   
do_render(StatemModule,Filename) ->
  {ok,Binary} = file:read_file(Filename),
  {classified,Implementations,Classes} = binary_to_term(Binary),
  ranker_classify:graph(make_results_name(""),render(StatemModule),Implementations,Classes,[]).

render_classes(Exercise) ->
  Files = filelib:wildcard(Exercise++"*classes*.bin"),
  StatemModule = list_to_atom(Exercise),
  lists:foreach
    (fun (Filename) ->
	 N = filename:rootname(Filename),
	 io:format("reading ~s~n",[Filename]),
	 {ok,Binary} = file:read_file(Filename),
	 {classified,Implementations,Classes} = binary_to_term(Binary),
	 ranker_classify:graph(N,render(StatemModule),Implementations,Classes,[{symbolic_edges,true}]),
	 io:format("before dot...~n"),
	 os:cmd(io_lib:format("dot -Tpng < ~p.dot > ~p.png",[N,N]))
     end, Files).

make_results_name(Option) ->
  io_lib:format("results:~s:~s",[Option,os:getpid()]).
