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
      %%io:format("Args are ~p~n",[Args]),
      %%io:format("CP is ~p~n",[CP]),
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
	       create_node(CP),
	       %%io:format("will run~n~p~n",[Cmds]),
               {H,DS,Res} = eqc_statem:run_commands(Module,Cmds),
	       java:terminate(node_id()),
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

create_node(CP) ->
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
  put(name,"unknown"),
  Node.

node_id() ->
  get(node).

name() ->
  get(name).

%% eqctest:test(lab1_corr,["/home/fred/aed_backup_20150215/aed/Laboratorio1/G-3S1M/000999/20140918-200018/"],"000999").

%% eqctest:runtests(lab1_corr, "IntegerSet.java", "/home/fred/entregas1", []).

%% eqctest:runtests(lab2_corr, "ExtendedNodePositionList.java", "/home/fred/entregas2", ["net-datastructures-5-0.jar"]).

%% eqctest:runtests(lab3_corr, "MoreExtendedNodePositionList.java", "/home/fred/entregas3", ["net-datastructures-5-0.jar"]).

%% eqctest:runtests(lab5_corr, "MyElementIterator.java", "/home/fred/entregas5", ["net-datastructures-5-0.jar"]).

%% eqctest:runtests(lab6_corr, "CompareAlumnos.java", "/home/fred/entregas6", ["/home/fred/gits/src/aed_labs/classes"]).

%% eqctest:test(lab5_corr, ["net-datastructures-5-0.jar","/home/fred/entregas5/Lab5/G-3S1M/000999/20121025-131858"],"000999").

%% eqctest:test(lab6_corr, ["/home/fred/gits/src/aed_labs/classes","/home/fred/entregas6/Lab6/G-3S2M/000999/20121122-103655"],"000999").

%% eqctest:test(lab1_corr, "tmps").

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



  



  


