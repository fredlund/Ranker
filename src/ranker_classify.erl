%% Orginal version by John Hughes et.al.
%% Modified by Lars-Ake Fredlund.
%% 

-module(ranker_classify).
-include_lib("eqc/include/eqc.hrl").

-export([classify/5,graph/5]).

-define(ETS_TABLE,ranker_classify_ets).

-define(debug,true).
-include("result.hrl").

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-define(DEBUGVAL(),true).
-else.
-define(LOG(X,Y), ok).
-define(DEBUGVAL(),false).
-endif.

-spec classify(string(),
	       integer(),
	       [{string(),any()}],
	       any(),
	       fun((any()) -> boolean())) -> [#result{}].
classify(Name,N,Implementations,Gen,Test) -> 
  ranker_create_ets_table:ensure_open(?ETS_TABLE),
  ?LOG("Implementations are~n~p~n",[Implementations]),
  classes(Name,1,Implementations,N,[],Gen,Test).

classes(Name,I,Implementations,N,Classes,Gen,Test) ->
  AllPairs =
    lists:usort
      (lists:flatmap
	 (fun (Class) ->
	      pairs(Class#result.failures,Class#result.successes)
	  end, Classes)),
  AllIncomparables =
    [ {P,Q} ||
      {P,Q} <- AllPairs,
      P < Q,
      lists:member({Q,P},AllPairs) ],
  AllEqual =
    [ {P,Q} ||
      P <- Implementations, Q <- Implementations,
      P < Q,
      not(lists:member({P,Q},AllPairs)), 
      not(lists:member({Q,P},AllPairs)) ],
  io:format
    ("~n~n~s: --- iteration ~p; ~p tests, ~p pairs ~p unordered ~p equal ---~n~n",
     [print_time(),I,length(Classes),
      length(AllPairs),
      length(AllIncomparables) div 2,
      length(AllEqual)]),
  
  ets:insert(?ETS_TABLE,{fail,void}),
  case counterexample(prop_complete(Classes,Gen,Implementations,Test)) of
    true ->
      file:write_file
	(Name++"_classes_"++integer_to_list(I)++".bin",
	 term_to_binary({classified,Implementations,Classes})),
      Classes;
    [TestCase] ->
      timer:sleep(3000),
      ?LOG
	("Failing test case:~n~p~n~n------ Retesting --------~n~n",
	 [TestCase]),
      Ps = lists:usort(Test(TestCase)),
      ResultP = #result{failures=Ps,successes=Implementations--Ps,testcase=TestCase},
      if
	Ps==[] ->
	  io:format("*** Warning: ps is nil for ~p~n",[TestCase]);
	true ->
	  ok
      end,
      IsCovered =
	case covered(Classes,ResultP) of
	  true ->
	    io:format
	      ("*** Warning: failing implementations~n~s~nARE COVERED BY~n tests~n~s~n",
	       [print_class(ResultP),print_classes(Classes)]),
	    true;
	  {false,_} ->
	    false
	end,
      %% As an (expensive) sanity check, check that computed failing
      %% implementations is stable...
      Qs = lists:usort(Test(TestCase)),
      ValuesDiffer =
	if
	  Ps=/=Qs ->
	    io:format
	      ("*** Warning: values differ??? ps:~n~p~n qs:~n~p~n",
	       [Ps,Qs]),
	    true;
	  true -> false
	end,
      if
	not(IsCovered), not(ValuesDiffer) ->
	  NewClasses = independent(Classes++[ResultP]),
	  ?LOG
	    ("Tests were ~n~s~n; new ones are ~n~s~n",
	     [print_classes(Classes),print_classes(NewClasses)]),
	  ?LOG
	    ("Testcase~n~p~nwith failures~n~p~n",[TestCase,Ps]),
	  file:write_file
	    (Name++"_classes_"++integer_to_list(I)++".bin",
	     term_to_binary({classified,Implementations,NewClasses})),
	  UC = lists:usort(Classes), NC = lists:usort(NewClasses),
	  if
	    UC=/=NC ->
	      ?LOG
		("NewTests=~n~p~nOldTests=~n~p~n",
		 [UC,NC]);
	    true -> ok
	  end,
	  classes(Name,I+1,Implementations,N,NewClasses,Gen,Test);
	true ->
	  io:format("Warning: repeating iteration ~p~n",[I]),
	  classes(Name,I,Implementations,N,Classes,Gen,Test)
      end
  end.

print_classes(Classes) ->
  lists:foldl
    (fun (Class,Acc) ->
	 "{" ++  print_class(Class) ++ "}\n"++Acc
     end,
     "",
     Classes).

print_class(Class) ->
  lists:foldr(fun (Implementation,Acc) -> Implementation++", "++Acc end, "", Class#result.failures).

prop_complete(Classes,Gen,Implementations,Test) ->
  ?FORALL(TestCase,
	  Gen,
	  pcovered(Classes,Test,Implementations,TestCase)).

independent(Classes) ->
  [Class || Class <- Classes, not to_bool(covered(Classes--[Class],Class))].

pcovered(Classes,Test,Implementations,TestCase) ->
  Ps = lists:usort(Test(TestCase)),
  Result = #result{failures=Ps,successes=Implementations--Ps,testcase=TestCase},
  CovResult = covered(Classes,Result),
  Covered = to_bool(CovResult),
  if
    not(Covered) ->
      ?LOG
	("~s~nIS NOT COVERED BY~n~s~n",
	 [print_class(Result),
	  print_classes(Classes)]);
    true ->
      ok
  end,
  Covering =
    if
      not(Covered) ->
	[{fail,Fail}] = ets:lookup(?ETS_TABLE,fail),
	case shrink_measure(Fail,Result,CovResult) of
	  {true,NewMeasure} ->
	    ?LOG
	      ("shrinking succeeded: was ~p is ~p~n",
	       [Fail,NewMeasure]),
	    ets:insert(?ETS_TABLE,{fail,NewMeasure}),
	    false;
	  {false,_NewMeasure} ->
	    ?LOG
	      ("shrinking failed: was ~p suggested new ~p~n",
	       [Fail,_NewMeasure]),
	    true
	end;
      true -> true
    end,
  Covering.

shrink_measure(PreOldMeasure,Result,CovResult) ->
  shrink_measure_higher(PreOldMeasure,Result,CovResult).
  %%shrink_measure_normal(PreOldMeasure,Result,CovResult).
  %%shrink_measure_lower(PreOldMeasure,Result,CovResult).

shrink_measure_higher(PreOldMeasure,Result,{false,CovMeasure}) ->
  NumFailures =
    length(Result#result.failures),
  NumSuccesses =
    length(Result#result.successes),
  _ = %%SizeOfNewPairs = 
    NumFailures*NumSuccesses,
  NewMeasure =
    %%SizeOfNewPairs
    CovMeasure,
  OldMeasure =
    if
      PreOldMeasure==void ->
	0;
      true ->
	PreOldMeasure
    end,
  if
    NewMeasure >= OldMeasure ->
      {true,NewMeasure};
    true ->
      {false,NewMeasure}
  end.

%% shrink_measure_normal(_PreOldMeasure,_Result,{false,CovMeasure}) ->
%%   NewMeasure =
%%     CovMeasure,
%%   {true,NewMeasure}.
%% 
%% shrink_measure_never(PreOldMeasure,_Result,{false,CovMeasure}) ->
%%   NewMeasure =
%%     %%SizeOfNewPairs
%%     CovMeasure,
%%   if
%%     PreOldMeasure==void ->
%%       {true,NewMeasure};
%%     true ->
%%       {false,NewMeasure}
%%   end.
%% 
%% shrink_measure_lower(PreOldMeasure,Result,{false,CovMeasure}) ->
%%   NumFailures =
%%     length(Result#result.failures),
%%   NumSuccesses =
%%     length(Result#result.successes),
%%   _SizeOfNewPairs = 
%%     NumFailures*NumSuccesses,
%%   NewMeasure =
%%     CovMeasure,
%%   OldMeasure =
%%     if
%%       PreOldMeasure==void ->
%% 	99999999;
%%       true ->
%% 	PreOldMeasure
%%     end,
%%   if
%%     NewMeasure =< OldMeasure ->
%%       {true,NewMeasure};
%%     true ->
%%       {false,NewMeasure}
%%   end.

to_bool({Value,_}) ->
  to_bool(Value);
to_bool(Value) when is_boolean(Value) ->
  Value.

covered(Classes,Result) ->
  Pairs = pairs(Result#result.failures,Result#result.successes),
  NewPairs =
    lists:filter
      (fun ({Failure,Success}) ->
	   not(lists:any
		 (fun (Class) ->
		      lists:member(Failure,Class#result.failures)
			andalso lists:member(Success,Class#result.successes)
		  end, Classes))
       end, Pairs),
  case length(NewPairs) of
    0 -> true;
    N -> {false,N}
  end.

pairs(L1,L2) ->
  lists:flatmap(fun (E1) -> lists:map(fun (E2) -> {E1,E2} end, L2) end, L1).

calculate_graph(Implementations,PreClasses) ->
  Classes =
    lists:map (fun (Class) -> {Class#result.testcase,
			       Class#result.failures} end, PreClasses),
  Successes = [{P,[T || {T,Ps0} <- Classes,
			not lists:member(P,Ps0)]}
	       || P <- Implementations],
  Equivalences = lists:usort([ {[ Q || {Q,Tq} <- Successes, Tq==T],T}
			       || {_P,T} <- Successes]),
  ?LOG("Successes = ~p\n",[Successes]),
  ?LOG("Equivalences = ~p\n",[Equivalences]),
  Edges = [{P,Q,TsP--TsQ}
	   || {P,TsP} <- Equivalences,
	      {Q,TsQ} <- Equivalences,
	      P/=Q,
	      TsQ--TsP == [],
	      [] ==
		[R || {R,TsR} <- Equivalences,
		      R/=P,
		      R/=Q,
		      TsR--TsP == [],
		      TsQ--TsR == []]],
  ?LOG("Edges = ~p\n",[Edges]),
  States = [P || {P,_} <- Equivalences],
  ?LOG("States=~n~p~nEdges=~n~p~n",[States,Edges]),
  ?LOG("num states=~p num edges=~p~n",[length(States),length(Edges)]),
  {States,Edges}.

graph(Name,Render,Implementations,Classes,Options) ->
  {States,Edges} = calculate_graph(Implementations,Classes),
  ?LOG("Saving result in ~s~n",[Name++".dot"]),
  StateMap = render_states(States),
  ?LOG("StateMap is~n~p~n",[StateMap]),
  {NewEdges,_,EdgeMap} =
    case proplists:get_value(symbolic_edges,Options) of
      true -> symbolic_edges(Edges);
      false -> {Edges,void,[]}
    end,
  RenderedStates =
    [dot_p(state(PState,StateMap))
     || PState <- States],
  RenderedEdges =
    [dot_e(Render,state(P,StateMap),state(Q,StateMap),Ts)
     || {P,Q,Ts} <- NewEdges],
  file:write_file
    (Name++".dot",
     list_to_binary(
       ["digraph{\n",
	RenderedStates,
	RenderedEdges,
	"}\n"])),
  output_interpretation(Name++".interpretation",StateMap,EdgeMap),
  statistics(Name,States,Edges).

output_interpretation(Name,StateMap,EdgeMap) ->
  if
    (StateMap=/=[]) orelse (EdgeMap=/=[]) ->
      {ok,File} = file:open(Name,[write]),
      if
	StateMap=/=[] ->
	  io:format(File,"States:~n",[]),
	  lists:foreach
	    (fun ({State,StateMapped}) ->
		 case length(State)>3 of
		   true ->
		     io:format
		       (File,"~s ->~n  ~p~n",
			[StateMapped,State]);
		   _ -> ok
		 end
	     end, lists:reverse(StateMap));
	true -> ok
      end,
      if
	EdgeMap=/=[] ->
	  io:format(File,"~nEdges:~n",[]),
	  lists:foreach
	    (fun ({Edge,EdgeMapped}) ->
		 io:format(File,"~p -> ~n  ~p~n",[EdgeMapped,Edge])
	     end, lists:reverse(EdgeMap));
	true -> ok
      end,
      file:close(File);
    true -> ok
  end.

state(P,StateMap) ->
  {_,State} = lists:keyfind(P,1,StateMap),
  State.

symbolic_edges(Edges) ->
  ?LOG("Edges is ~p~n",[Edges]),
  Result=
    lists:foldl
      (fun (_Edge={P,Q,Ts},{NewEdges,Counter,Map}) ->
	   {NewTs,NewCounter,NewMap} =
	     lists:foldl
	       (fun (T,{NewTs1,NewCounter1,NewMap1}) ->
		    case lists:keyfind(T,1,NewMap1) of
		      {_,Value} -> {[Value|NewTs1],NewCounter1,NewMap1};
		      false -> 
			ST=NewCounter1,
			{[ST|NewTs1],NewCounter1+1,[{T,ST}|NewMap1]}
		    end
		end,
		{[],Counter,Map}, Ts),
	   {[{P,Q,{symbolic,NewTs}}|NewEdges],NewCounter,NewMap}
       end, {[],0,[]}, Edges),
  ?LOG("NewEdges is ~p~n",[element(1,Result)]),
  Result.

calculate_paths(States,Edges) ->
  Starters =
    lists:filter
      (fun (State) -> 
	   not(lists:any(fun ({_,Q,_}) -> Q==State end, Edges))
       end,States),
  NonStarters = 
    States--Starters,
  StarterPaths =
    lists:map
      (fun (State) -> {State,[]} end, Starters),
  Paths =
    lists:map
      (fun (State) -> {State,find_path_to(State,StarterPaths,Edges)} end,
       Starters++NonStarters),
  lists:sort
    (fun ({_,Path1},{_,Path2}) ->
	 path_length(Path1) =< path_length(Path2)
     end, Paths).

calculate_all_paths(States,Edges) ->
  Starters =
    lists:filter
      (fun (State) -> 
	   not(lists:any(fun ({_,Q,_}) -> Q==State end, Edges))
       end,States),
  _NonStarters = States--Starters,
  ?LOG("Starters are~n~p~nNonstarters:~n~p~n",[Starters,_NonStarters]),
  StarterPaths =
    lists:map
      (fun (State) -> {State,[],[]} end, Starters),
  ?LOG("Starterpaths=~n~p~n",[StarterPaths]),
  lists:map
    (fun (State) ->
	 {State,all_paths_to(State,Starters,StarterPaths,Edges)}
     end, States).

statistics(Name,States,Edges) ->
  {ok,File} = file:open(Name++".results",[write]),
  io:format(File,"Path Results:~n~n",[]),
  lists:foreach
    (fun ({State,Path}) ->
	 io:format(File,"~p: ~p~n",[State,path_length(Path)])
     end,
     calculate_paths(States,Edges)),
  io:format(File,"All Path Results:~n~n",[]),
  lists:foreach
    (fun ({State,Paths}) ->
	 io:format(File,"~p:~n~p~n",[State,Paths])
     end,
     calculate_all_paths(States,Edges)),
  file:close(File).

path_length([]) -> 0;
path_length([First|Rest]) ->
  length(First)+path_length(Rest).

find_path_to(State,CurrentPaths,Edges) ->
  %%?LOG("CurrentPaths are~n~p~n",[CurrentPaths]),
  case lists:keyfind(State, 1, CurrentPaths) of
    {_,Path} -> Path;
    false ->
      NextPaths = 
	lists:foldl
	  (fun (_Path={PState,PPath},NewPaths1) ->
	       lists:foldl
		 (fun (_Edge={From,To,Label},NewPaths2) ->
		      if
			PState==From ->
			  case lists:keyfind(To, 1, NewPaths2) of
			    {_,_} ->
			      NewPaths2;
			    false ->
			      [{To,[Label|PPath]}|NewPaths2]
			  end;
			true -> 
			  NewPaths2
		      end
		  end, NewPaths1, Edges)
	   end, CurrentPaths, CurrentPaths),
      case length(NextPaths)>length(CurrentPaths) of
	true -> find_path_to(State,NextPaths,Edges);
	false -> throw(badarg)
      end
  end.

all_paths_to(State,Starters,StarterPaths,Edges) ->
  case lists:member(State,Starters) of
    true -> 
      {_,FPs} = split_paths(State,StarterPaths),
      FPs;
    false ->
      all_paths_to1(State,StarterPaths,Edges,[])
  end.

all_paths_to1(State,AlivePaths,Edges,FPs) ->
  ?LOG
    ("all_paths_to: state=~p alivepaths=~n~p~nfps=~n~p~n",
     [State,AlivePaths,FPs]),
  {AllAlivePaths,AllFinishedPaths} =
    lists:foldl
      (fun (_Path={PState,PLabels,PStates},{FAlivePaths,FinishedPaths}) ->
	   NextPaths =
	     lists:foldl
	       (fun (_Edge={From,To,Label},AlivePaths2) ->
		    if
		      PState == From ->
			NewPath = {To,[Label|PLabels],[From|PStates]},
			[NewPath|AlivePaths2];
		      true ->
			AlivePaths2
		    end
		end, [], Edges),
	   {NextAlivePaths,NextFinishedPaths} = split_paths(State,NextPaths),
	   {NextAlivePaths++FAlivePaths,NextFinishedPaths++FinishedPaths}
       end, {[],FPs}, AlivePaths),
  if
    AllAlivePaths=/=[] ->
      all_paths_to1(State,AllAlivePaths,Edges,AllFinishedPaths);
    true ->
      AllFinishedPaths
  end.

split_paths(State,Paths) ->
  lists:foldl
    (fun (Path={PState,_PLabels,_PStates},{APs,FPs}) ->
	 if
	   PState==State ->
	     {APs,[Path|FPs]};
	   true ->
	     {[Path|APs],FPs}
	 end
     end, {[],[]}, Paths).

render_states(States) ->
  element
    (2,
     lists:foldr
       (fun (State,{Counter,Map}) ->
	    {NewCounter,RenderedState} = dot_name(State,Counter),
	    {NewCounter,[{State,RenderedState}|Map]}
	end, {0,[]}, States)).

dot_p(RenderedState) ->
  ?LOG("RenderedState=~p~n",[RenderedState]),
  io_lib:format("~s [];\n",[RenderedState]).

dot_name(LStrs,Counter) ->
  ?LOG("dot_name(~p)~n",[LStrs]),
  case length(LStrs) of
    N when N<4 ->
      {Counter,
       "\""++
	 lists:foldr
	   (fun (S,Str) -> 
		Name = shorten(S),
		if Str=="" -> Name;
		   true -> Name++","++Str 
		end
	    end, "",LStrs)++"\""};
    N ->
      {Counter+1,
       "\""++
	 integer_to_list(Counter)++
	 " (#"++integer_to_list(N)++")\""}
  end.


shorten(Name) ->
  case string:str(Name,"_") of
    0 -> Name;
    N -> string:sub_string(Name,1,N-1)
  end.

dot_e(Render,P,Q,Ts) ->
  EdgeString = 
    case Ts of
      {symbolic,Numbers} -> 
	SortedNumbers = lists:usort(Numbers),
	CompressedNumbers = compress(SortedNumbers),
	render_numbers(CompressedNumbers);
      _ ->
	render_edge(Render,Ts)
    end,
  io_lib:format
    ("~s -> ~s [label=\"~s\"];\n",
     [P,Q,EdgeString]).

render_numbers([]) ->
  "";
render_numbers([{N1,N2}|Rest]) ->
  Continuation = 
    if
      Rest==[] -> "";
      true -> ","++render_numbers(Rest)
    end,
  io_lib:format("~p-~p~s",[N1,N2,Continuation]);
render_numbers([N|Rest]) ->
  Continuation = 
    if
      Rest==[] -> "";
      true -> ","++render_numbers(Rest)
    end,
  io_lib:format("~p~s",[N,Continuation]).

compress([]) ->
  [];
compress([N]) ->
  [N];
compress([N|Rest]) ->
  {EndPoint,Remains} = compress(N,Rest),
  if
    EndPoint=/=N ->
      [{N,EndPoint}|compress(Remains)];
    true ->
      [N|compress(Rest)]
  end.

compress(N,[]) ->
  {N,[]};
compress(N,L=[M|Rest]) ->
  if
    M=/=N+1 -> {N,L};
    true -> compress(M,Rest)
  end.

render_edge(Renderer,Ts) ->
  lists:map
    (fun (T) ->
	 ?LOG("T is ~p~n",[T]),
	 case T of
	   [{symbolic,E}] -> atom_to_list(E);
	   _ -> Renderer(T)
	 end
     end, Ts).

print_time() ->
  {Year,Month,Day} = date(),
  {Hour,Minute,Second} = time(),
  io_lib:format("~p-~p-~p@~p:~p:~p",[Year,Month,Day,Hour,Minute,Second]).
