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

classify(StatemModule,RecipeModule,Recipe,ImplementationList) ->
  {ImplementationIds,Implementations} = lists:unzip(ImplementationList),
  ets:new(?EtsTableName,[public,named_table]),
  ets:insert(?EtsTableName,{implementations,ImplementationIds}),
  ok = RecipeModule:start(Implementations,Recipe),
  _FinalClasses =
    ranker_classify:classify
      (atom_to_list(StatemModule),
       100,
       ImplementationIds,
       eqc_statem:commands(StatemModule),
       fun(Commands) -> run_for_each(StatemModule,RecipeModule,Commands) end).

run_for_each(StatemModule,RecipeModule,Cmds) ->
  Self = self(),
  [{_,Implementations}] = ets:lookup(?EtsTableName,implementations),
  lists:foreach
    (fun (ImpId) ->
	 spawn_link
	   (fun () ->
		check(StatemModule,RecipeModule,Cmds,ImpId,Self)
	    end)
     end, Implementations),
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
  Fails++Timeouts.

check(StatemModule,RecipeModule,Cmds,ImpId,Parent) ->
  ImpData = RecipeModule:start_implementation(ImpId),
  {Time,{_H1,_DS1,Res1}} =
    timer:tc
    (fun () -> 
	 eqc_statem:run_commands(StatemModule,Cmds,[{id,ImpId},{imp,ImpData}])
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
  RecipeModule:stop_implementation(ImpId,Result),
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


  
