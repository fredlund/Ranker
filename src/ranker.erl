-module(ranker). 

-compile(export_all).
-include_lib("eqc/include/eqc.hrl").

-include("implementation.hrl").

-record(implementation_rec,{id,pid,implementation,timeouts=0}).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-define(DEBUGVAL(),true).
-else.
-define(LOG(X,Y), ok).
-define(DEBUGVAL(),false).
-endif.

report_error(Cmds,H,DS,Res,ImplementationRec) ->
  io:format
    ("~n~n~p: commands~n~p~n failed in state~n"++
     "~p~n due to ~p~n",[ImplementationRec#implementation_rec.id,Cmds,DS,Res]),
  io:format("Failure reason is ~p~n",[Res]),
  HLen = length(H),
  FailCommands = lists:sublist(Cmds,HLen),
  Combined = lists:zip(FailCommands,H),
  io:format("~nFailing commands of length ~p~n",[HLen]),
  lists:foreach
    (fun ({Cmd,{_,Result}}) ->
	 io:format("~p => ~p~n",[Cmd,Result])
     end, Combined),
  io:format("Failing command: ~p~n",[lists:sublist(Cmds,HLen+1,1)]).

%% erl -pa ../ebin -run rank render_classes lab1b_corr -run erlang halt
%% erl -pa ../ebin -run rank render_classes lab2b_corr -run erlang halt
%% erl -pa ../ebin -run rank render_classes lab3_corr -run erlang halt
%% erl -pa ../ebin -run rank render_classes lab4_corr -run erlang halt
%% erl -pa ../ebin -run rank render_classes lab3_corr -run erlang halt
%% erl -pa ../ebin -run rank render_classes lab3_corr -run erlang halt
%% erl -pa ../ebin -run rank render_classes lab_old_corr -run erlang halt
%%

classify(Module,ImplementationRecipe) ->
  Implementations = ImplementationRecipe(),
  io:format
    ("Dir: ~p Elapsed seconds: ~p~n",
     [Dir,Time/1000000]).
  ets:new(eqc_result,[public,named_table]),
  ets:insert(eqc_result,{result,0,0,[]}),
  ets:insert(eqc_result,{current_test_case,0}),
  FinalClasses =
    classify:classify
      (atom_to_list(Module),
       100,
       lists:map 
	 (fun ({_,Implementation}) -> 
	      Implementation#implementation_rec.id end, 
	  ImplementationStructs),
       eqc_statem:commands(Module),
       fun(Commands) -> run_for_each(Module,Commands) end),
  file:write_file
    (Filename,
     term_to_binary
       ({classified,lists:map(fun ({Name,_,_,_}) -> Name end, Implementations),
	 FinalClasses})).

setup_implementations(Implementations) ->
  lists:map
    (fun ({ImplementationName,_Group,ImplementationDir,_Time}) ->
	 {ImplementationName,
	  #implementation_rec
	  {id=ImplementationName,
	   implementation=
	     node_reuse:make_implementation
	       (ImplementationName,ImplementationDir)}}
     end,
     Implementations).

implementations(Implementations) ->
  lists:map(fun ({Implementationname,_}) ->
		Implementationname 
	    end, Implementations).

run_for_each(Module,Cmds) ->
  [{implementations,Implementations}] = ets:lookup(eqc_result,implementations),
  Self = self(),
  lists:map
    (fun (Implementation) ->
	 spawn_link
	   (fun () ->
		check(Module,Cmds,Implementation,Self)
	    end)
     end, Implementations),
  {Fails,Timeouts} = collect_fails(Implementations),
  if
    Fails=/=[] ->
      io:format("Failing implementations:~n~p~n",[lists:usort(Fails)]);
    true ->
      ok
  end,
  if
    Timeouts=/=[] ->
      io:format("Timeouts: ~p~n",[lists:usort(Timeouts)]);
     true ->
      ok
  end,
  lists:foreach(fun handle_timeout/1, Timeouts),
  Fails++Timeouts.

handle_timeout(Implementation) ->
  [{_,ImplementationRec}] = ets:lookup(eqc_result,{implementation,Implementation}),
  NumTimeouts = ImplementationRec#implementation_rec.timeouts+1,
  io:format
    ("*** Timeout number ~p for implementation ~p~n",
     [NumTimeouts,ImplementationRec#implementation_rec.id]),
  ets:insert
    (eqc_result,{{implementation,Implementation},ImplementationRec#implementation_rec{timeouts=NumTimeouts}}).

check(Module,Cmds,Implementation,Parent) ->
  Counter = 
    case ets:lookup(eqc_result,node_counter) of
      [{_,Cnt}] -> Cnt;
      _ -> 0
    end,
  true =
    ets:insert(eqc_result,{node_counter,Counter+1}),
  Implementation = ImplementationRec#implementation_rec.id,
  Id = (ImplementationRec#implementation_rec.implementation)#implementation.implementation_id,
  ets:insert
    (eqc_result,
     {{implementation,Implementation},
      ImplementationRec#implementation_rec{pid=self()}}),
  {Time,{_H1,_DS1,Res1}} =
    timer:tc
    (fun () -> 
	 put(node,Node),
	 put(name,Implementation),
	 eqc_statem:run_commands(Module,Cmds,[{node,Node},{classes,Classes}])
     end),
  Seconds = Time/1000000,
  if
    Seconds>5.0 ->
      io:format("Warning: time=~p for implementation ~p~n",[Seconds,Id]);
    true ->
      ok
  end,
  if 
    Res1=/=ok ->
      io:format("Implementation ~p failed with exit code ~p~n",[Implementation,Res1]);
    true -> 
      ok
  end,
  Result = 
    case Res1 of
      ok -> false;
      java_timeout -> timeout;
      {exception,java_timeout} -> timeout;
      %% Not sure how this can happen??
      {postcondition,java_timeout} -> timeout; 
      _Other -> true
    end,
  Parent!{implementation,Implementation,Result}.

collect_fails(Implementations) ->
  collect_fails(Implementations,[],[]).
collect_fails([],Fails,Timeouts) ->
  {Fails,Timeouts};
collect_fails([_|Rest],Fails,Timeouts) ->
  receive 
    {implementation,Implementation,FailResult} ->
      case FailResult of
	true -> collect_fails(Rest,[Implementation|Fails],Timeouts);
	false -> collect_fails(Rest,Fails,Timeouts);
	timeout -> collect_fails(Rest,Fails,[Implementation|Timeouts])
      end
    end.

render(Module) ->
  fun (Commands) ->
      Result =
	Module:render_commands
	(lists:foldl
	   (fun (Command,Acc) -> Module:render(Command,Acc) end,
	    Module:render_init(), Commands)),
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
   
set_option(Key) ->
  io:format("Option ~p set~n",[Key]),
  ets:insert(eqc_result,{Key,true}).

do_render(Module,Filename) ->
  {ok,Binary} = file:read_file(Filename),
  {classified,Implementations,Classes} = binary_to_term(Binary),
  classify:graph(make_results_name(""),render(Module),Implementations,Classes,[]).

render_classes([Exercise]) ->
  Files = filelib:wildcard(Exercise++"*classes*.bin"),
  Module = list_to_atom(Exercise),
  lists:foreach
    (fun (Filename) ->
	 N = filename:rootname(Filename),
	 {ok,Binary} = file:read_file(Filename),
	 {classified,Implementations,Classes} = binary_to_term(Binary),
	 classify:graph(N,render(Module),Implementations,Classes,[{symbolic_edges,true}]),
	 os:cmd(io_lib:format("dot -Tpng < ~p.dot > ~p.png",[N,N]))
     end, Files).

make_results_name(Option) ->
  io_lib:format("results:~s:~s",[Option,os:getpid()]).

