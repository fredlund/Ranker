-module(ranker). 

-compile(export_all).
-include_lib("eqc/include/eqc.hrl").

%%-define(debug,true).

-define(EtsTableName,ranker_ets).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-define(DEBUGVAL(),true).
-else.
-define(LOG(X,Y), ok).
-define(DEBUGVAL(),false).
-endif.

classify(StatemModule,Recipe,Implementations) ->
  ets:new(?EtsTableName,[public,named_table]),
  ImplementationIds =
    lists:map (fun (Imp) -> proplists:get_value(id,Imp) end, Implementations),
  set_implementations(ImplementationIds),
  RecipeState =
    Recipe:initial_state(Implementations),
  set_recipe_struct(RecipeState),
  lists:foreach
      (fun ({Id,Implementation}) ->
	   ImpState = Recipe:init_implementation(Implementation,RecipeState),
	   set_imp_struct(Id,ImpState)
       end, lists:zip(ImplementationIds,Implementations)),
  _FinalClasses =
    classify:classify
      (atom_to_list(StatemModule),
       100,
       ImplementationIds,
       eqc_statem:commands(StatemModule),
       fun(Commands) -> run_for_each(StatemModule,Commands) end).

run_for_each(StatemModule,Cmds) ->
  Self = self(),
  RecipeState = recipe_struct(),
  Recipe = proplists:get_value(recipe,RecipeState),
  Implementations = implementations(),
  RunRS =
    lists:foldl
      (fun (ImpId,RS) ->
	   ImpState = imp_struct(ImpId),
	   {NewRS,NewImpState} = 
	     Recipe:prepare_implementation(ImpState,RS),
	   spawn_link
	     (fun () ->
		  check(StatemModule,Cmds,ImpId,NewImpState,Self)
	      end),
	   NewRS
       end, RecipeState, Implementations),
  {Fails,Timeouts} = collect_fails(Implementations),
  if
    Fails=/=[] ->
      io:format
	("Failing implementations:~n~p~n",
	 [lists:usort(Fails)]);
    true ->
      ok
  end,
  if
    Timeouts=/=[] ->
      io:format
	("Timeouts: ~p~n",
	 [lists:usort(Timeouts)]);
     true ->
      ok
  end,
  FinalRS = Recipe:post_run(RunRS,Fails,Timeouts),
  set_recipe_struct(FinalRS),
  Fails++Timeouts.

check(StatemModule,Cmds,ImpId,ImpState,Parent) ->
  {Time,{_H1,_DS1,Res1}} =
    timer:tc
    (fun () -> 
	 eqc_statem:run_commands(StatemModule,Cmds,[{id,ImpId},{imp,ImpState}])
     end),
  Seconds = Time/1000000,
  if
    Seconds>5.0 ->
      io:format
	("Warning: time=~p for implementation ~p~n",
	 [Seconds,ImpId]);
    true ->
      ok
  end,
  if 
    Res1=/=ok ->
      io:format
	("Implementation ~p failed with exit code ~p~n",
	 [ImpId,Res1]);
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
  Parent!{implementation,ImpId,Result}.

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

set_recipe_struct(RS) ->
  ets:insert(?EtsTableName,{recipe_struct,RS}).

recipe_struct() ->
  [{_,RS}] = ets:lookup(?EtsTableName,recipe_struct),
  RS.

set_imp_struct(ImpId,ImpState) ->
  ets:insert(?EtsTableName,{{imp,ImpId},ImpState}).

imp_struct(ImpId) ->
  [{_,ImpState}] = ets:lookup(?EtsTableName,{imp,ImpId}),
  ImpState.

set_implementations(Implementations) ->
  ets:insert(?EtsTableName,{implementations,Implementations}).

implementations() ->
  [{_,Implementations}] = ets:lookup(?EtsTableName,implementations),
  Implementations.


  
