-module(eqctest).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").


%% Should return a list of <group,matricula,time> entries identifying
%% the last successful entrega
find_entregas(LFile,Target) ->
  WildCard = Target++"/*",
  io:format
    ("find_entregas: wildcard=~s~n",
     [WildCard]),
  lists:foldl
    (fun (Dir,Files) ->
	 Group = filename:basename(Dir),
	 %%io:format
	   %%("group is ~p for ~p: recursing over ~p~n",[Group,Dir,Target]),
	 find_group_entregas(LFile,Group,Dir)++Files
     end,[],filelib:wildcard(WildCard)).

find_group_entregas(LFile,Group,GroupTarget) ->
  lists:foldl
    (fun (Dir,Files) ->
	 User = filename:basename(Dir),
	 case has_valid_entrega(LFile,Group,User,Dir) of
	   {ok,Entrega} ->
	     [Entrega|Files];
	   _ ->
	     io:format
	       ("~n*** Warning: no valid entrega for user ~p in group ~p~n",
		[User,Group]),
	     Files
	 end
     end,[],filelib:wildcard(GroupTarget++"/*")).

has_valid_entrega(LFile,Group,User,Dir) ->
  case lists:foldl
    (fun (EntregaDir,Best) ->
	 Time = to_dtime(filename:basename(EntregaDir)),
	 case filelib:wildcard(EntregaDir++"/"++LFile) of
	   [] -> Best;
	   _ -> update_best(Time,filename:absname(EntregaDir),User,Group,Best)
	 end
     end, void, filelib:wildcard(Dir++"/*")) of
    void -> false;
    Result -> {ok,Result}
  end.

update_best(Time,Dir,User,Group,void) ->
  {User,Group,Dir,Time};
update_best(Time,Dir,User,Group,{User2,Group2,Dir2,Time2}) ->
  case cmp_times(Time,Time2) of
    greater -> {User,Group,Dir,Time};
    _ -> {User2,Group2,Dir2,Time2}
  end.
      
cmp_times({Y1,M1,D1,H1,Min1,S1},{Y2,M2,D2,H2,Min2,S2}) ->
  if
    Y1>Y2 -> greater;
    Y2>Y1 -> lesser;
    M1>M2 -> greater;
    M2>M1 -> lesser;
    D1>D2 -> greater;
    D2>D1 -> lesser;
    H1>H2 -> greater;
    H2>H1 -> lesser;
    Min1>Min2 -> greater;
    Min2>Min1 -> lesser;
    S1>S2 -> greater;
    S2>S1 -> lesser;
    true -> equal
  end.

to_dtime([Y1,Y2,Y3,Y4,M1,M2,D1,D2,_,H1,H2,Min1,Min2,S1,S2]) ->
  {Y,_}=string:to_integer([Y1,Y2,Y3,Y4]),
  {M,_}=string:to_integer([M1,M2]),
  {D,_}=string:to_integer([D1,D2]),
  {H,_}=string:to_integer([H1,H2]),
  {Min,_}=string:to_integer([Min1,Min2]),
  {S,_}=string:to_integer([S1,S2]),
  {Y,M,D,H,Min,S}.

find_late_entregas(Dir,Description) ->
  lists:foreach
    (fun ({LabNo,File,CutOffTime}) ->
	 LabDir = io_lib:format("~s/Lab~p",[Dir,LabNo]),
	 io:format("Checking Lab ~p in directory ~s~n",[LabNo,LabDir]),
	 Entregas = find_entregas(File,LabDir),
	 io:format("Number of entregas is ~p~n",[length(Entregas)]),
	 LateEntregas =
	   lists:filter
	     (fun ({User,Group,Dir,Time}) ->
		  case cmp_times(Time,CutOffTime) of
		    lesser -> false;
		    Other -> true
		  end
	      end, Entregas),
	 io:format("Number of late entregas is ~p~n",[length(LateEntregas)]),
	 case length(LateEntregas) > 0 of
	   true ->
	     io:format("Cutoff time: ~p~n",[CutOffTime]),
	     lists:foreach
	       (fun ({User,Group,Dir,Time}) ->
		    io:format
		      ("*** User ~p from Group ~p has late entrega "++
			 "of lab ~p~n        at time ~p~n",
		       [User,Group,LabNo,Time])
		end, LateEntregas);
	   false -> ok
	 end
     end,
     Description).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lines(LFile,Target) ->
  lists:map
    (fun (Entrega={User,Group,Dir,Time}) ->
	 ObjectCommand =
	   "~/Downloads/javancss-32.53/bin/javancss "++
	   "-object "++
	   Dir++"/"++LFile,
	 ObjectResult = os:cmd(ObjectCommand),
	 NCCSValue =
	   case re:run(ObjectResult,".*Program NCSS:[[:space:]]*(.*)",
		       [{capture,all_but_first,list}]) of
	     {match,[Value]} ->
	       list_to_float(Value);
	     nomatch ->
	       io:format
		 ("Running ~n~s~nfailed to match, result:~n~s~n",
		  [ObjectCommand,ObjectResult]),
	       void
	   end,
	 FunctionCommand =
	   "~/Downloads/javancss-32.53/bin/javancss "++
	   "-function "++
	   Dir++"/"++LFile,
	 FunctionResult = os:cmd(FunctionCommand),
	 Pattern = ".*Average Function CCN:[[:space:]]*([[:digit:]]*\\.[[:digit:]]*)",
	 CCNValue =
	   case re:run(FunctionResult,
		       Pattern,
		       [{capture,all_but_first,list},dotall]) of
	     {match,[FuncValue]} ->
	       list_to_float(FuncValue);
	     nomatch ->
	       io:format
		 ("Running ~n~s~nfailed to match pattern ~s, result:~n~s~n",
		  [FunctionCommand,Pattern,FunctionResult]),
	       void
	   end,

	 FuncPattern =
	   ".*JVDC\sFunction[[:space:]]*(.*)Average\sFunction\sNCSS",
	 IndividualCCNS = 
	   case re:run(FunctionResult,
		       FuncPattern,
		       [{capture,all_but_first,list},dotall]) of
	     {match,[FuncValues]} ->
	       %%io:format
	       %%("FuncValues:~n~s~n",
	       %%[FuncValues]),
	       get_func_ccns(FuncValues,[],0);
	     nomatch ->
	       io:format
		 ("Running ~n~s~nfailed to match pattern ~s, result:~n~s~n",
		  [FunctionCommand,FuncPattern,FunctionResult]),
	       {void,void}
	   end,
	 {Entrega,NCCSValue,CCNValue,IndividualCCNS}
     end, find_entregas(LFile,Target)).

get_func_ccns(String,L,Max) ->
  FuncPattern = "[[:space:]]*[[:graph:]]*[[:space:]]*[[:graph:]]*[[:space:]]*([[:graph:]]*)[[:space:]]*[[:graph:]]*[[:space:]]*[[:graph:]]*(.*)",
  case re:run(String,FuncPattern,[{capture,all_but_first,list},dotall]) of
    {match,[Num,Other]} ->
      if
	Num=/="" ->
	  Int = list_to_integer(Num),
	  %%io:format("Num=~s Other=~s~n",[Num,Other]),
	  NewMax = if Int>Max -> Int; true -> Max end,
	  get_func_ccns(Other,[Int|L],NewMax);
	true ->
	  {lists:reverse(L),Max}
      end;
    nomatch -> {lists:reverse(L),Max}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile(LFile,Target) ->
  lists:foreach
    (fun ({User,Group,Dir,Time}) ->
	 Command =
	   "cd "++Dir++"; ~/Downloads/jdk/jdk1.6.0_38/bin/javac "++
	   "-d classes "++
	   "-cp /home/fred/gits/src/aed_labs_2014/positionList.jar "++
	   "*.java",
	 ClassDir = Dir++"/"++"classes/",
	 %%io:format("will ensure ~s~n",[ClassDir]),
	 case filelib:ensure_dir(ClassDir) of
	   ok ->
	     ok;
	   {error,Reason} -> 
	     io:format("Error: could not create ~s due to ~p~n",[Dir,Reason]),
	     throw(bad)
	 end,
	 Result =
	   os:cmd(Command),
	 io:format
	   ("Compiled for ~p in ~p~nCommand:~n~p~nResult:~n~p~n",
	    [User,Group,Command,Result])
     end, find_entregas(LFile,Target)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

runtests(Module,LFile,Target,CP) ->
  lists:foreach
    (fun ({User,Group,Dir,Time}) ->
	 io:format
	   ("~n===============================================================~n"),
	 io:format
	   ("~nWill test ~s for user ~s in group ~s~n",
	    [Target,User,Group]),
	 CmdLine =
	   io_lib:format
	      ("erl -noshell -sname tester -pa ebin -run ~p test ~p '~s' "++
		 print_cp([Dir|CP])++
		 " -run erlang halt~n",
	       [?MODULE,Module,
		io_lib:format("User ~s in group ~s",[User,Group])]),
	 io:format("Cmd is ~s~n",[CmdLine]),
	 Result = os:cmd(CmdLine),
	 io:format("~nOutput:~n~n~s~n",[Result])
     end, find_entregas(LFile,Target)).

print_cp([]) ->
  "";
print_cp([Obj|Rest]) ->
  io_lib:format("~s ",[Obj])++print_cp(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(Args) ->
  case Args of
    [ModuleString,Id|CP] ->
      test(list_to_atom(ModuleString),CP,Id);
    _ ->
      io:format("*** Error: strange arguments ~p~n",[Args])
  end,
  halt().

test(Module,CP,Id) ->
  case ets:info(eqc_data) of
    undefined ->
      ets:new(eqc_data,[public,named_table]);
    _ ->
      ok
  end,
  V=eqc:quickcheck
    (?FORALL(Cmds,
             eqc_statem:more_commands(1,eqc_statem:commands(Module)),
             begin
	       Node = create_node(CP,Id),
	       %%io:format("will run~n~p~n",[Cmds]),
               {H,DS,Res} = eqc_statem:run_commands(Module,Cmds,[{node,Node}]),
	       java:terminate(Node),
               case Res of
                 ok -> true;
                 _ -> 
                   io:format("~nFAILING...~nRes=~p~n",[Res]),
		   %%io:format("~nnode=~p~n",[get(node)]),
		   %%io:format("~nCmds=~p~n",[Cmds]),
		   %%io:format("~nH=~p~n",[H]),
		   %%io:format("~nDS=~p~n",[DS]),
                   false
               end
             end)),
  eqc:stop(),
  ets:delete(eqc_data,node),
  if
    not(V) ->
      io:format
	("~n~n*** ~s FAILED; counterexample is~n~n~p~n",
	 [Id,eqc:counterexample()]);
    true ->
      io:format
	("~n~n~s PASSED~n",[Id])
  end.

create_node(CP,Name) ->
  %%java:set_loglevel(all),
  %%io:format("Classpath is ~p~n",[CP]),
  {ok,Node} =
    java:start_node
      ([{add_to_java_classpath,CP},
%%	{log_level,info},
%%	{java_verbose,"INFO"},
	{java_exception_as_value,true}]),
  ets:insert(eqc_data,{node,Node}),
  put(node,Node),
  put(name,Name),
  Node.

node_id() ->
  get(node).

name() ->
  get(name).

%% eqctest:test(lab1_corr,["/home/fred/aed_backup_20150215/aed/Laboratorio1/G-3S1M/000999/20140918-200018/"],"000999").

%% eqctest:runtests(lab1_corr, "CheckConsecutiveElements.java", "/home/fred/aed_backup_20150215/aed/Laboratorio1/", []).

%% erl -pa ebin -name tst@tabitha.ls.fi.upm.es
%% rank:do_classify(lab1_corr,"CheckConsecutiveElements.java","/home/fred/aed_backup_20150215/aed/Laboratorio1/","class.bin",[{local,6,info},{"138.100.13.47",6,info}]).
%%
%% erl -pa ../ebin -run rank render_classes lab1_corr -run erlang halt
%% eqctest:runtests(lab2_corr, "ExtendedNodePositionList.java", "/home/fred/entregas2", ["net-datastructures-5-0.jar"]).

%% eqctest:test(lab2_corr,["/home/fred/aed_backup_20150215/aed/Laboratorio2/G-3S1M/000999/20140925-185003/","classes/positionList.jar"],"000999").

%% eqctest:runtests(lab2_corr, "RemovePositionList.java", "/home/fred/aed_backup_20150215/aed/Laboratorio2/", ["classes/positionList.jar"]).

%% erl -pa ebin -name tst@tabitha.ls.fi.upm.es
%% rank:do_classify(lab2_corr,"RemovePositionList.java","/home/fred/aed_backup_20150215/aed/Laboratorio2/","class.bin",[{local,6,info},{"138.100.13.47",6,info}]).



%% eqctest:runtests(lab3_corr, "MoreExtendedNodePositionList.java", "/home/fred/entregas3", ["net-datastructures-5-0.jar"]).

%% eqctest:runtests(lab5_corr, "MyElementIterator.java", "/home/fred/entregas5", ["net-datastructures-5-0.jar"]).

%% eqctest:runtests(lab6_corr, "CompareAlumnos.java", "/home/fred/entregas6", ["/home/fred/gits/src/aed_labs/classes"]).

%% eqctest:test(lab1_corr, "/tmp/entregas/Lab1/G-3S2M/110142/20120928-125449

%% eqctest:test(lab1_corr, "/tmp/entregas/Lab1/G-3S2M/110142/20120928-125449

%% eqctest:test(lab5_corr, ["net-datastructures-5-0.jar","/home/fred/entregas5/Lab5/G-3S1M/000999/20121025-131858"],"000999").

%% eqctest:test(lab6_corr, ["/home/fred/gits/src/aed_labs/classes","/home/fred/entregas6/Lab6/G-3S2M/000999/20121122-103655"],"000999").

%% eqctest:test(lab1_corr, "tmps").

%% eqctest:compile("CheckConsecutiveElements.java", "/home/fred/aed_backup_20150215/aed/Laboratorio1").

%% eqctest:compile("RemovePositionList.java", "/home/fred/aed_backup_20150215/aed/Laboratorio2").

%% eqctest:compile("SortList.java", "/home/fred/aed_backup_20150215/aed/Laboratorio4").

%% eqctest:compile("MergeLists.java", "/home/fred/aed_backup_20150215/aed/Laboratorio3").

%% eqctest:compile("FairMerge.java", "/home/fred/aed_backup_20150215/aed/Laboratorio5").

%% eqctest:compile("IteratorMerge.java", "/home/fred/aed_backup_20150215/aed/Laboratorio6").

%% eqctest:lines("MergeLists.java", "/home/fred/aed_backup_20150215/aed/Laboratorio3").

%% eqctest:lines("CheckConsecutiveElements.java", "/home/fred/aed_backup_20150215/aed/Laboratorio1").

find_late_entregas() ->
  find_late_entregas
    ("/home/fred/entregas_sep_2011",
     [{1,"IntegerSet.java",{2012,10,6,0,0,0}},
      {2,"ExtendedNodePositionList.java",{2012,10,15,0,0,0}},
      {3,"MoreExtendedNodePositionList.java",{2012,10,20,0,0,0}},
      {4,"IterateNodePositionList.java",{2012,10,28,0,0,0}},
      {5,"MyElementIterator.java",{2012,11,8,0,0,0}},
      {6,"CompareAlumnos.java",{2012,12,1,0,0,0}},
      {7,"BinTree.java",{2012,12,10,0,0,0}},
      {8,"BinTreeWithDelete.java",{2012,12,15,0,0,0}},
      {9,"Heap.java",{2012,12,25,0,0,0}}
     ]).

normalize_matricula(A) when is_atom(A) ->
  normalize_matricula(atom_to_list(A));
normalize_matricula([First|Rest]) 
  when First>=$1, First=<$9 ->
  [First|normalize_matricula_rest(Rest)];
normalize_matricula([First|Rest]) ->
  normalize_matricula(Rest).

normalize_matricula_rest([]) ->
  [];
normalize_matricula_rest([32|Rest]) ->
  normalize_matricula_rest(Rest);
normalize_matricula_rest([First|Rest]) when First>=$a, First=<$z ->
  [First-($a-$A)|normalize_matricula_rest(Rest)];
normalize_matricula_rest([First|Rest]) ->
  [First|normalize_matricula_rest(Rest)].

