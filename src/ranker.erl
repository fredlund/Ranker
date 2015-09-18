-module(ranker). 

-compile(export_all).
-include_lib("eqc/include/eqc.hrl").

-record(student,{name,node,pid,nodespec,timeouts=0}).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-define(DEBUGVAL(),true).
-else.
-define(LOG(X,Y), ok).
-define(DEBUGVAL(),false).
-endif.

%%
%% Add error reason as well, or rather a classification of that
%% 


%% Bugs:
%%
%% Improvements:
%%


report_error(Cmds,H,DS,Res,StudentRec) ->
  io:format
    ("~n~n~p: commands~n~p~n failed in state~n"++
     "~p~n due to ~p~n",[StudentRec#student.name,Cmds,DS,Res]),
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


%% rank:do_classify(lab3_corr,"MoreExtendedNodePositionList.java","/home/fred/../fred_old/entregas3","class.bin").
%% rank:do_classify(lab3_corr,"MoreExtendedNodePositionList.java","testd3","class.bin").
%% rank:do_classify(lab2_corr,"ExtendedNodePositionList.java","testd","class.bin",[{"sadrach.ls.fi.upm.es",2,info},{"esther.ls.fi.upm.es",2,info}]).
%% rank:do_classify(lab2_corr,"ExtendedNodePositionList.java","testd","class.bin",[{"babel.ls.fi.upm.es",6,info},{"esther.ls.fi.upm.es",4,info},{"sadrach.ls.fi.upm.es",2,info}]).
%% rank:render_classes(["lab2_corr"]).
%% rank:do_classify(lab1_corr,"IntegerSet.java","testd2","class.bin").
%% rank:do_render("class.bin").
%%
%% rank:do_classify(lab1_corr,"IntegerSet.java","/home/fred/entregas1","class.bin",[{"babel.ls.fi.upm.es",6,debug},{"esther.ls.fi.upm.es",6,debug},{"sadrach.ls.fi.upm.es",2,debug}]).
%%
%% rank:do_classify(lab2_corr,"ExtendedNodePositionList.java","/home/fred/entregas2","class.bin",[{"babel.ls.fi.upm.es",6,debug},{"esther.ls.fi.upm.es",6,debug},{"sadrach.ls.fi.upm.es",2,debug}]).
%%
%% rank:do_classify(lab3_corr,"MoreExtendedNodePositionList.java","/home/fred/entregas3","class.bin",[{"babel.ls.fi.upm.es",6,debug},{"esther.ls.fi.upm.es",6,debug},{"sadrach.ls.fi.upm.es",2,debug}]).
%%
%% rank:do_classify(lab4_corr,"IterateNodePositionList.java","/home/fred/entregas4","class.bin",[{"babel.ls.fi.upm.es",6,debug},{"esther.ls.fi.upm.es",6,debug},{"sadrach.ls.fi.upm.es",2,debug}]).
%%
%% rank:do_classify(lab5_corr,"MyElementIterator.java","/home/fred/entregas5","class.bin",[{"babel.ls.fi.upm.es",6,debug},{"esther.ls.fi.upm.es",6,debug},{"sadrach.ls.fi.upm.es",2,debug}]).
%%
%% rank:do_classify(lab6_corr,"CompareAlumnos.java","/home/fred/entregas6","class.bin",[{"babel.ls.fi.upm.es",6,info},{"esther.ls.fi.upm.es",6,info},{"sadrach.ls.fi.upm.es",2,info}]).
%%
%% rank:do_classify(lab6_corr,"CompareAlumnos.java","/home/fred/old_entregas6/Lab6","class.bin",[{local,6,info}]).
%%
%% rank:do_classify(lab6_corr,"CompareAlumnos.java","/home/fred_old/entregas6/Lab6","class.bin",[{local,6,info}]).
%%
%% rank:do_classify(lab6_corr,"CompareAlumnos.java","/home/fred/old_entregas6/Lab6","class.bin",[{local,6,info}]).
%%
%% rank:do_classify(lab6_corr,"CompareAlumnos.java","/home/fred/old_entregas6/Lab6","class.bin",[{local,6,info},{"138.100.13.47",6,info}]).
%%
%%
%% erl -pa ../ebin -run rank render_classes lab2_corr -run erlang halt
%% erl -pa ../ebin -run rank render_classes lab6_corr -run erlang halt

do_classify(Module,LFile,Dir,Filename) ->
  classify_par(Module,eqctest:find_entregas(LFile,Dir),Filename,void).

do_classify(Module,LFile,Dir,Filename,Plan) ->
  NodePlan = dist:start_erlang_nodes(Plan),
  classify_par(Module,eqctest:find_entregas(LFile,Dir),Filename,NodePlan).

%% Students == [{User,Group,Dir,Time}]

classify_par(Module,Students,Filename,RunPlan) ->
  io:format("classify_par called~n"),
  ets:new(eqc_result,[public,named_table]),
  ets:insert(eqc_result,{result,0,0,[]}),
  ets:insert(eqc_result,{current_test_case,0}),
  io:format("run_plan is ~p~n",[RunPlan]),
  if
    RunPlan=/=void ->
      ets:insert(eqc_result,{run_plan,RunPlan});
    true ->
      ok
  end,
  set_option(restart),
  %%set_option(noDelete),
  %%set_option(noAddNode),
  %%set_option(noReplace),
  StudentNodes = setup_students(Students),
  lists:foreach
    (fun ({Student,Record}) ->
	ets:insert(eqc_result,{{student,Student},Record})
    end, StudentNodes),
  ets:insert(eqc_result,{students,students(StudentNodes)}),
  NumStudents = length(Students),
  Classes =
    classify:classiphy
      (atom_to_list(Module),
       100,
       lists:map (fun ({_,Student}) -> Student#student.name end, StudentNodes),
       NumStudents,
       eqc_statem:commands(Module),
       fun(Commands) -> run_for_each(Module,Commands) end),
  file:write_file
    (Filename,
     term_to_binary
       ({classified,lists:map(fun ({Name,_,_,_}) -> Name end, Students),
	 Classes})).

do_render(Module,Filename) ->
  {ok,Binary} = file:read_file(Filename),
  {classified,Students,Classes} = binary_to_term(Binary),
  classify:graph(make_results_name(""),render(Module),Students,Classes,[]).

render_classes([Exercise]) ->
  Files = filelib:wildcard(Exercise++"*classes*.bin"),
  Module = list_to_atom(Exercise),
  lists:foreach
    (fun (Filename) ->
	 N = filename:rootname(Filename),
	 {ok,Binary} = file:read_file(Filename),
	 {classified,Students,Classes} = binary_to_term(Binary),
	 classify:graph(N,render(Module),Students,Classes,[{symbolic_edges,true}]),
	 os:cmd(io_lib:format("dot -Tpng < ~p.dot > ~p.png",[N,N]))
     end, Files).

make_results_name(Option) ->
  io_lib:format("results:~s:~s",[Option,os:getpid()]).

setup_students(Students) ->
  StudentNodes =
    lists:map
      (fun ({StudentName,_Group,StudentDir,_Time}) ->
	   AddJavaClassPath =
	     ["/home/fred/lib/net-datastructures-5-0.jar",
	      "/home/fred/gits/src/aed_labs/classes",
	      StudentDir],
	   NodeSpec =
	     node_spec(StudentName,AddJavaClassPath,StudentDir),
	   {StudentName,
	    #student{name=StudentName,nodespec=NodeSpec}}
       end,
       Students),
  StudentNodes.

students(StudentNodes) ->
  lists:map(fun ({Student,_}) -> Student end, StudentNodes).

run_for_each(Module,Cmds) ->
  [{students,Students}] = ets:lookup(eqc_result,students),
  io:format("Commands are~n~p~n",[Cmds]),
  Self = self(),
  lists:map
    (fun (Student) ->
	 [{_,StudentNode}] = ets:lookup(eqc_result,{student,Student}),
	 spawn
	   (fun () ->
		check(Module,Cmds,StudentNode,Self)
		%%io:format("terminating checker ~p~n",[self()])
	    end)
	 %%io:format("spawned checker ~p~n",[Pid])
     end, Students),
  {Fails,Timeouts} = collect_fails(Students),
  if
    Fails=/=[] ->
      io:format("Failing students:~n~p~n",[lists:usort(Fails)]);
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

handle_timeout(Student) ->
  [{_,StudentRec}] = ets:lookup(eqc_result,{student,Student}),
  NumTimeouts = StudentRec#student.timeouts+1,
  io:format
    ("*** Timeout number ~p for student ~p~n",
     [NumTimeouts,StudentRec#student.name]),
  ets:insert
    (eqc_result,{{student,Student},StudentRec#student{timeouts=NumTimeouts}}).

check(Module,Cmds,StudentRec,Parent) ->
  Counter = 
    case ets:lookup(eqc_result,node_counter) of
      [{_,Cnt}] -> Cnt;
      _ -> 0
    end,
  true =
    ets:insert(eqc_result,{node_counter,Counter+1}),
  NodeSpec =
    case proplists:get_value(symbolic_name,StudentRec#student.nodespec) of
      Name when is_list(Name) ->
	[{symbolic_name,Name ++ "_" ++ integer_to_list(Counter)}|
	 StudentRec#student.nodespec];
      _ ->
	StudentRec#student.nodespec
    end,
  {ok,Node} =
    case ets:lookup(eqc_result,run_plan) of
      [{_,Plan}] ->
	dist:start_node(Plan,NodeSpec);
      _ ->
	io:format("no plan~n",[]),
	java:start_node(NodeSpec)
    end,
  Student = StudentRec#student.name,
  ets:insert
    (eqc_result,
     {{student,Student},
      StudentRec#student{node=Node,pid=self()}}),
  put(node,Node),
  put(name,Student),
  {_H1,_DS1,Res1} = eqc_statem:run_commands(Module,Cmds),
  if 
    Res1=/=ok ->
      io:format("Student ~p failed with exit code ~p~n",[Student,Res1]);
    true -> 
      ok
  end,
  Result = 
    case Res1 of
      ok -> false;
      {exception,java_timeout} -> timeout;
      _Other -> true
    end,
  if
    Result==timeout ->
      java:brutally_terminate(Node);
    true ->
      java:terminate(Node)
  end,
  Parent!{student,Student,Result}.

collect_fails(Students) ->
  collect_fails(Students,[],[]).
collect_fails([],Fails,Timeouts) ->
  {Fails,Timeouts};
collect_fails([_|Rest],Fails,Timeouts) ->
  receive 
    {student,Student,FailResult} ->
      %%io:format("got reply ~p;N=~p~n",[{Student,FailResult},N]),
      case FailResult of
	true -> collect_fails(Rest,[Student|Fails],Timeouts);
	false -> collect_fails(Rest,Fails,Timeouts);
	timeout -> collect_fails(Rest,Fails,[Student|Timeouts])
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
   
shrink({var,N}) -> N;
shrink(Other) -> Other.

findNode({call,_,_,[findNode,[_,Size,_]]}) -> {f,list_to_atom(Size)};
findNode({var,N}) -> N;
findNode(_) -> node.

set_option(Key) ->
  io:format("Option ~p set~n",[Key]),
  ets:insert(eqc_result,{Key,true}).

node_spec(StudentName,AddClassPath,_StudentDir) ->
  io:format("ClassPath for ~p is~n~p~n",[StudentName,AddClassPath]),
  [{symbolic_name,StudentName},
   {log_level,warning},
%%   {java_verbose,true},
   {java_exception_as_value,true},
   {call_timeout,5000},
   {add_to_java_classpath,AddClassPath}].

