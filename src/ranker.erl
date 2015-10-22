-module(ranker). 

-compile(export_all).
-include_lib("eqc/include/eqc.hrl").

-include("implementation.hrl").

%%-define(debug,true).

-define(EtsTableName,ranker_ets).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-define(DEBUGVAL(),true).
-else.
-define(LOG(X,Y), ok).
-define(DEBUGVAL(),false).
-endif.

classify(RankerModule,RecipeModule,Recipe,Implementations) ->
  ImplementationIds =
    lists:map
      (fun (Imp) -> Imp#implementation.id end, 
       Implementations),
  ets:new(?EtsTableName,[public,named_table]),
  ets:insert(?EtsTableName,{implementations,ImplementationIds}),
  ok = RecipeModule:start(Implementations,Recipe),
  _FinalClasses =
    ranker_classify:classify
      (atom_to_list(RankerModule),
       100,
       ImplementationIds,
       RankerModule:generator(),
       fun(Data) -> run_for_each(RankerModule,RecipeModule,Data) end).

run_for_each(RankerModule,RecipeModule,Data) ->
  Self = self(),
  [{_,Implementations}] = ets:lookup(?EtsTableName,implementations),
  lists:foreach
    (fun (ImpId) ->
	 spawn_link
	   (fun () ->
		check(RankerModule,RecipeModule,Data,ImpId,Self)
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

check(RankerModule,RecipeModule,Data,ImpId,Parent) ->
  ImpData = RecipeModule:start_implementation(ImpId),
  {Time,Result} =
    timer:tc
    (fun () -> 
	 RankerModule:prop(Data,ImpId,ImpData)
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


  
