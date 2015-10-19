%%% File    : classify.erl
%%% Author  :  <>
%%% Description : 
%%% Created : 25 May 2010 by  <>

-module(ranker_classify).
-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-record(result,{testcase,failures,successes}).

classify(Name,N,Implementations,Gen,Test) ->
  io:format("Implementations are~n~p~n",[Implementations]),
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

  %% io:format
  %%("~nall loaded size:~p~n~p~n",[length(code:all_loaded()),code:all_loaded()]),
    ets:insert(eqc_result,{fail,void}),
    case counterexample(prop_complete(N,Classes,Gen,Implementations,Test)) of
	true -> Classes;
      [TestCase] ->
	timer:sleep(3000),
	io:format
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
	    io:format
	      ("Tests were ~n~s~n; new ones are ~n~s~n",
	       [print_classes(Classes),print_classes(NewClasses)]),
	    io:format
	      ("Testcase~n~p~nwith failures~n~p~n",[TestCase,Ps]),
	    file:write_file
	      (Name++"_classes_"++integer_to_list(I)++".bin",
	       term_to_binary({classified,Implementations,NewClasses})),
	    UC = lists:usort(Classes), NC = lists:usort(NewClasses),
	    if
	      UC=/=NC ->
		io:format
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

prop_complete(N,Classes,Gen,Implementations,Test) ->
%%    numtests(
  %%    N,
    %%  ?ALWAYS(3,
	      ?FORALL(TestCase,
		      Gen,
		      %%noshrink(Gen),
		      pcovered(Classes,Test,Implementations,TestCase))
.
%%)).

%% Don't we forget old information???
%% Another approach would be to systematically 
%% What is the datastructure? We should describe it.
%% What about the sum of classes? 
%% How long can the analysis take? (what is the bound?)
independent(Classes) ->
    [Class || Class <- Classes, not to_bool(covered(Classes--[Class],Class))].

pcovered(Classes,Test,Implementations,TestCase) ->
  Ps = lists:usort(Test(TestCase)),
  Result = #result{failures=Ps,successes=Implementations--Ps,testcase=TestCase},
  CovResult = covered(Classes,Result),
  Covered = to_bool(CovResult),
  if
    not(Covered) ->
      io:format
	("~s~nIS NOT COVERED BY~n~s~n",
	 [print_class(Result),
	  print_classes(Classes)]);
    true ->
      ok
  end,
  Covering =
    if
      not(Covered) ->
	[{fail,Fail}] = ets:lookup(eqc_result,fail),
	case shrink_measure(Fail,Result,CovResult) of
	  {true,NewMeasure} ->
	    io:format
	      ("shrinking succeeded: was ~p is ~p~n",
	       [Fail,NewMeasure]),
	    ets:insert(eqc_result,{fail,NewMeasure}),
	    false;
	  {false,NewMeasure} ->
	    io:format
	      ("shrinking failed: was ~p suggested new ~p~n",
	       [Fail,NewMeasure]),
	    true
	end;
      true -> true
    end,
  Covering.

shrink_measure(PreOldMeasure,Result,CovResult) ->
  %%shrink_measure_higher(PreOldMeasure,Result,CovResult).
  %%shrink_measure_normal(PreOldMeasure,Result,CovResult).
  shrink_measure_lower(PreOldMeasure,Result,CovResult).

shrink_measure_higher(PreOldMeasure,Result,{false,CovMeasure}) ->
  NumFailures =
    length(Result#result.failures),
  NumSuccesses =
    length(Result#result.successes),
  SizeOfNewPairs = 
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

shrink_measure_normal(PreOldMeasure,Result,{false,CovMeasure}) ->
  NewMeasure =
    %%SizeOfNewPairs
    CovMeasure,
  {true,NewMeasure}.

shrink_measure_never(PreOldMeasure,Result,{false,CovMeasure}) ->
  NewMeasure =
    %%SizeOfNewPairs
    CovMeasure,
  if
    PreOldMeasure==void ->
      {true,NewMeasure};
    true ->
      {false,NewMeasure}
  end.

shrink_measure_lower(PreOldMeasure,Result,{false,CovMeasure}) ->
  NumFailures =
    length(Result#result.failures),
  NumSuccesses =
    length(Result#result.successes),
  SizeOfNewPairs = 
    NumFailures*NumSuccesses,
  NewMeasure =
    %%SizeOfNewPairs
    CovMeasure,
  OldMeasure =
    if
      PreOldMeasure==void ->
	99999999;
      true ->
	PreOldMeasure
    end,
  if
    NewMeasure =< OldMeasure ->
      {true,NewMeasure};
    true ->
      {false,NewMeasure}
  end.

intersection([],_) ->
  [];
intersection(_,[]) ->
  [];
intersection(L1=[H1|T1],L2=[H2|T2]) ->
  if
    H1>H2 ->
      intersection(L1,T2);
    H2>H1 ->
      intersection(T1,L2);
    true ->
      [H1|intersection(T1,T2)]
  end.

subset([],_) ->
  true;
subset(_,[]) ->
  false;
subset([H1|T1],[H1|T2]) ->
  subset(T1,T2);
subset(L1=[H1|_],[H2|T2]) ->
  if
    H2 < H1 ->
      subset(L1,T2);
    true ->
      false
  end.

%%covered(Classes,Result) ->
%%  Ps = Result#result.failures,
%%  Ps ==
%%    lists:usort([P || Ps0 <-
%%			lists:map(fun (Class) -> Class#result.failures end,
%%				  Classes),
%%		      Ps0 -- Ps == [],
%%		      P <- Ps0]).

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
		    || {P,T} <- Successes]),
    io:format("Successes = ~p\n",[Successes]),
    io:format("Equivalences = ~p\n",[Equivalences]),
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
    io:format("Edges = ~p\n",[Edges]),
    States = [P || {P,_} <- Equivalences],
  io:format("States=~n~p~nEdges=~n~p~n",[States,Edges]),
  io:format("num states=~p num edges=~p~n",[length(States),length(Edges)]),
  {States,Edges}.

graph(Name,Render,Implementations,Classes,Options) ->
  {States,Edges} = calculate_graph(Implementations,Classes),
  io:format("Saving result in ~s~n",[Name++".dot"]),
  StateMap = render_states(States),
  io:format("StateMap is~n~p~n",[StateMap]),
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
  io:format("Edges is ~p~n",[Edges]),
  Result=
  lists:foldl
    (fun (Edge={P,Q,Ts},{NewEdges,Counter,Map}) ->
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
  io:format("NewEdges is ~p~n",[element(1,Result)]),
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
  NonStarters = 
    States--Starters,
  StarterPaths =
    lists:map
      (fun (State) -> {State,[],[]} end, Starters),
  lists:map
    (fun (State) ->
	 {State,all_paths_to(State,StarterPaths,Edges)}
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
  %%io:format("CurrentPaths are~n~p~n",[CurrentPaths]),
  case lists:keyfind(State, 1, CurrentPaths) of
    {_,Path} -> Path;
    false ->
      NextPaths = 
	lists:foldl
	  (fun (Path={PState,PPath},NewPaths1) ->
	       lists:foldl
		 (fun (Edge={From,To,Label},NewPaths2) ->
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
		      
all_paths_to(State,StarterPaths,Edges) ->
  all_paths_to(State,StarterPaths,Edges,[]).

all_paths_to(State,AlivePaths,Edges,FPs) ->
  {AllAlivePaths,AllFinishedPaths} =
    lists:foldl
      (fun (Path={PState,PLabels,PStates},{AlivePaths,FinishedPaths}) ->
	   NextPaths =
	     lists:foldl
	       (fun (Edge={From,To,Label},AlivePaths2) ->
		    if
		      PState == From ->
			NewPath = {To,[Label|PLabels],[From|PStates]},
			[NewPath|AlivePaths2];
		      true ->
			AlivePaths2
		    end
		end, [], Edges),
	   {NextAlivePaths,NextFinishedPaths} = split_paths(State,NextPaths),
	   {NextAlivePaths++AlivePaths,NextFinishedPaths++FinishedPaths}
       end, {[],FPs}, AlivePaths),
  if
    AllAlivePaths=/=[] ->
      all_paths_to(State,AllAlivePaths,Edges,AllFinishedPaths);
    true ->
      AllFinishedPaths
  end.

split_paths(State,Paths) ->
  lists:foldl
    (fun (Path={PState,PLabels,PStates},{APs,FPs}) ->
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
  io:format("RenderedState=~p~n",[RenderedState]),
  io_lib:format("~s [];\n",[RenderedState]).

dot_name(LStrs,Counter) ->
  io:format("dot_name(~p)~n",[LStrs]),
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
	 io:format("T is ~p~n",[T]),
	 case T of
	   [{symbolic,E}] -> atom_to_list(E);
	   _ -> Renderer(T)
	 end
     end, Ts).

view_dot(Mod,ImageType) ->
    Name = atom_to_list(Mod),
    Type = atom_to_list(ImageType),
    oscmd("EQC_DOT","dot","-T"++Type++" "++Name++".dot -o"++Name++"."++Type),
    spawn(fun()->oscmd("EQC_VIEWER","",Name++"."++Type) end).

oscmd(EnvVar,Default,Args) ->
    CmdName = case os:getenv(EnvVar) of
		  false ->
		      Default;
		  S ->
		      S
	      end,
    Cmd = CmdName ++ " " ++ Args,
    io:format("Running ~s (set ~s to change)~n",[Cmd,EnvVar]),
    io:format(os:cmd(Cmd)).

print_time() ->
  {Year,Month,Day} = date(),
  {Hour,Minute,Second} = time(),
  io_lib:format("~p-~p-~p@~p:~p:~p",[Year,Month,Day,Hour,Minute,Second]).

  

