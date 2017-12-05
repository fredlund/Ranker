-module(ranker). 

-export([classify/5,version/0]).

-include_lib("eqc/include/eqc.hrl").
-include("implementation.hrl").
-include("result.hrl").

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-define(DEBUGVAL(),true).
-else.
-define(LOG(X,Y), ok).
-define(DEBUGVAL(),false).
-endif.

-type option() :: {timeout,integer()}.

-spec classify(atom(),atom(),any(),[{string(),any()}],[option()]) -> 
		  [#result{}].
classify(RankerModule,RecipeModule,Recipe,PreImplementations,PreOptions) ->
  Options = PreOptions++[{timeout,5000}],
  {ImplementationIds,_} =
    lists:unzip(PreImplementations),
  ?LOG
    ("ids=~p~n",
     [ImplementationIds]),
  Implementations = 
    lists:map
      (fun ({Id,Private}) ->
	   #implementation{id=Id,private=Private}
       end, PreImplementations),
  ok = RecipeModule:start(PreImplementations,Recipe),
  FinalClasses =
    ranker_classify:classify
      (atom_to_list(RankerModule),
       100,
       ImplementationIds,
       RankerModule:generator(),
       fun(Data) ->
	   run_for_each(RankerModule,RecipeModule,Data,Implementations,Options)
       end),
  RecipeModule:finish(),
  FinalClasses.

run_for_each(RankerModule,RecipeModule,Data,Implementations,Options) ->
  Self = self(),
  process_flag(trap_exit,true),
  NewImplementations =
    lists:map
      (fun (Implementation) ->
	   Pid = 
	     spawn_link
	       (fun () ->
		    check
		      (RankerModule,RecipeModule,Data,
		       Implementation,Options,Self)
		end),
	   Implementation#implementation{pid=Pid,status=computing}
       end, Implementations),
  {Fails,Timeouts} =
    collect_fails(NewImplementations,length(NewImplementations),RecipeModule,Options),
  if
    Fails=/=[] ->
      ?LOG
	("Failing implementations:~n~p~n",
	 [lists:usort(Fails)]);
    true ->
      ok
  end,
  if
    Timeouts=/=[] ->
      ?LOG
	("Timeouts: ~p~n",
	 [lists:usort(Timeouts)]),
      timer:sleep(100);
     true ->
      ok
  end,
  lists:map
    (fun (Implementation) ->
	 Implementation#implementation.id
     end,
     Fails++Timeouts).

check(RankerModule,RecipeModule,Data,Implementation,_Options,Parent) ->
  Id =
    Implementation#implementation.id,
  ImpData =
    RecipeModule:start_implementation
      (Id,Implementation#implementation.private),
  {Time,Result} =
    timer:tc
    (fun () -> 
	 RankerModule:prop(Data,Id,ImpData)
     end),
  Seconds = Time/1000000,
  if
    Seconds>5.0 ->
      io:format
	("~n*** Warning: time=~p for implementation ~p~n",
	 [Seconds,Id]);
    true ->
      ok
  end,
  Parent!{implementation,Id,Result}.

collect_fails(Implementations,N,RecipeModule,Options) ->
  collect_fails(Implementations,[],[],N,RecipeModule,Options).
collect_fails(_Implementations,Fails,Timeouts,0,_RecipeModule,_Options) ->
  {Fails,Timeouts};
collect_fails(Implementations,Fails,Timeouts,N,RecipeModule,Options) when N>0 ->
  Timeout =
    case proplists:get_value(timeout,Options) of
      TimeoutValue when is_integer(TimeoutValue), TimeoutValue>0 ->
	TimeoutValue;
      _ ->
	infinity
    end,
  receive 
    {implementation,Id,Result} ->
      Implementation =
	lists:keyfind(Id,#implementation.id,Implementations),
      RecipeModule:stop_implementation
	(Id,Implementation#implementation.private,Result),
      NewImplementations =
	lists:keyreplace(Id,#implementation.id,Implementations,
			 Implementation#implementation{status=finished}),
      case Result of
	true ->
	  collect_fails
	    (NewImplementations,Fails,Timeouts,N-1,RecipeModule,Options);
	false ->
	  collect_fails
	    (NewImplementations,[Implementation|Fails],Timeouts,N-1,RecipeModule,Options);
	timeout ->
	  collect_fails
	    (NewImplementations,Fails,[Implementation|Timeouts],N-1,RecipeModule,Options)
      end;

    {'EXIT',Pid,Reason} ->
      if
	Reason=/=normal ->
	  io:format
	    ("~n*** Warning: pid ~p exited due to ~p~n",
	     [Pid,Reason]),
	  case lists:keyfind(Pid,#implementation.pid,Implementations) of
	    false -> 
	      collect_fails(Implementations,Fails,Timeouts,N,RecipeModule,Options);
	    Implementation ->
	      io:format
		("~n*** handling implementation timeout:~n~p~n",
		 [Implementation]),
	      Id = Implementation#implementation.id,
	      RecipeModule:stop_implementation
		(Id,Implementation#implementation.private,timeout),
	      NewImplementations =
		lists:keyreplace(Id,#implementation.id,Implementations,
				 Implementation#implementation{status=finished}),
	      collect_fails
		(NewImplementations,Fails,[Implementation|Timeouts],N-1,RecipeModule,Options)
	  end;
	true ->
	  collect_fails(Implementations,Fails,Timeouts,N,RecipeModule,Options)
      end;

    Msg ->
      io:format
	("~n*** Warning: unknown message ~p received~n",
	 [Msg]),
      collect_fails(Implementations,Fails,Timeouts,N,RecipeModule,Options)

  after Timeout ->
      Remaining =
	lists:filter
	  (fun (Imp) -> Imp#implementation.status == computing end,
	   Implementations),
      io:format
	("~n*** Warning: timeout in collect_fails: remaining implementations: ~p~n",
	 [lists:map(fun (Imp) -> Imp#implementation.id end, Remaining)]),
      lists:foreach
	(fun (Imp) ->
	     RecipeModule:stop_implementation
	       (Imp#implementation.id,
		Imp#implementation.private,
		timeout)
	 end, Remaining),
      Result = terminate_timeouts(Remaining,Fails,Timeouts,Options),
      Result
  end.

terminate_timeouts([],Fails,Timeouts,_Options) ->
  {Fails,Timeouts};
terminate_timeouts([Implementation|Rest],Fails,Timeouts,Options) ->
  ?LOG("will kill ~p:~p~n",[Implementation#implementation.id,Implementation#implementation.pid]),
  erlang:exit(Implementation#implementation.pid,kill),
  terminate_timeouts(Rest,Fails,[Implementation|Timeouts],Options).

-spec version() -> string().
version() ->
  case [ Vsn || {ranker, _, Vsn} <- application:loaded_applications() ] of
    [] ->
      "unknown";
    [Vsn] ->
      Vsn
  end.

  
