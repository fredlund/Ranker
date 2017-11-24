-module(java_reuse_recipe).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([make_recipe/6]).
-export([start/2,start_implementation/2,stop_implementation/3,finish/0]).

-behaviour(gen_server).
-behaviour(ranker_recipe).

-record(state,{nodes,recipe,dist_model}).
-record(node,{node_id,timeout,java_node_id,reuses_left,active_implementations,implementations,is_busy,is_alive,pending_starts,location,counter,status=idle}).

-define(TIMEOUT,60*1000).
-define(GEN_SERVER_NAME,ranker_java_reuse).
-define(ETS_TABLE,ranker_java_reuse_ets).

-include("java_reuse_recipe.hrl").
-include("java_reuse_implementation.hrl").

-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-define(DEBUGVAL(),true).
-else.
-define(LOG(X,Y), ok).
-define(DEBUGVAL(),false).
-endif.


init([Recipe,Implementations]) ->
  init([Recipe,Implementations,[{local,1}]]);
init([Recipe,Implementations,DistPlan]) ->
  ranker_create_ets_table:ensure_open(?ETS_TABLE),
  Nodes = map_implementations_to_nodes(Implementations,Recipe),
  ?LOG("nodes are~n~p~n",[Nodes]),
  NodeDistPlan = start_erlang_nodes(DistPlan),
  State = #state{nodes=Nodes,recipe=Recipe,dist_model=NodeDistPlan},
  start_select(NodeDistPlan),
  {ok,State}.

handle_call(_Msg={start_implementation,ImplementationId},From,State) ->
  ?LOG("handle_call(~p)~n",[_Msg]),
  PreImplementation = read_implementation(ImplementationId),
  Implementation = PreImplementation#java_reuse_implementation{status=starting},
  write_implementation(Implementation),
  NodeId = Implementation#java_reuse_implementation.node_id,
  Node = read_node(NodeId),
  case lists:member(ImplementationId,Node#node.active_implementations) of
    true ->
      io:format
	("Fatal error: implementation ~p is active at ~p:~p on start_implementation???~n",
	 [ImplementationId,NodeId,Node]),
      throw(bad);
    false ->
      ok
  end,
  NewNode = Node#node{pending_starts=Node#node.pending_starts++[{ImplementationId,From}]},
  write_node(NewNode),
  do_something(NewNode,State),
  {noreply, State, ?TIMEOUT};

handle_call(_Msg={stop_implementation,ImplementationId,Result},From,State) ->
  ?LOG("handle_call(~p)~n",[_Msg]),
  PreImplementation = read_implementation(ImplementationId),
  Implementation = PreImplementation#java_reuse_implementation{status=idle},
  write_implementation(Implementation),
  NodeId = Implementation#java_reuse_implementation.node_id,
  Node = read_node(NodeId),
  Actives = Node#node.active_implementations,
  case not(lists:member(ImplementationId,Actives)) of
    true ->
      io:format
	("*** WARNING: implementation ~p:~n~p~nis not active at~n~p:~p on stop_implementation???~n",
	 [ImplementationId,PreImplementation,NodeId,Node]),
      ok;
    false ->
      ok
  end,
  Timeout = (Node#node.timeout) orelse (Result==timeout),
  %%io:format("~p: got ~p timeout is ~p~n",[NodeId,Result,Timeout]),
  if
    Result==timeout ->
      io:format("~p: got timeout for implementation ~p~n",[NodeId,ImplementationId]);
    true ->
      ok
  end,
  NewNode = Node#node{active_implementations=Actives--[ImplementationId],timeout=Timeout},
  write_node(NewNode),
  gen_server:reply(From,ok),
  do_something(NewNode,State),
  {noreply, State, ?TIMEOUT};

handle_call(_Msg=terminate,From,State) ->
  ?LOG("handle_call(~p)~n",[_Msg]),
  {stop,ordered,From,State}.

handle_cast(_Msg={node_created,{NodeId,JavaNodeId}},State) ->
  ?LOG("handle_cast(~p)~n",[_Msg]),
  Node = read_node(NodeId),
  NewNode =
    Node#node
    {is_busy=false,
     is_alive=true,
     reuses_left=(State#state.recipe)#java_reuse_recipe.reuse_limit,
     timeout=false,
     counter=0,
     active_implementations=[],
     pending_starts=Node#node.pending_starts,
     java_node_id=JavaNodeId},
  write_node(NewNode),
  do_something(NewNode,State),
  {noreply, State, ?TIMEOUT};

handle_cast(_Msg={implementation_started,{ImplementationId,Reply,NodeId,JavaNodeId,Classes}},State) ->
  ?LOG("handle_cast(~p)~n",[_Msg]),
  Node = read_node(NodeId),
  Implementation = read_implementation(ImplementationId),
  NewImplementation = Implementation#java_reuse_implementation{status=started},
  write_implementation(NewImplementation),
  NewNode =
    Node#node
    {is_busy=false,
     counter=Node#node.counter+1,
     active_implementations=[ImplementationId|Node#node.active_implementations]},
  {_Pid,_Ref} = Reply,
  ?LOG
    ("~p: replying to ~p is_alive? ~p~n",
     [ImplementationId,_Pid,is_process_alive(_Pid)]),
  gen_server:reply(Reply,{JavaNodeId,Classes}),
  write_node(NewNode),
  do_something(NewNode,State),
  {noreply, State, ?TIMEOUT}.

handle_info(timeout,State) ->
  io:format("~nTimeout~n"),
  lists:foreach
    (fun (PreNode) ->
	 Node = read_node(PreNode#node.node_id),
	 io:format
	   ("Node ~p has status ~p busy ~p~n",
	    [Node#node.node_id,Node#node.status,Node#node.is_busy]),
	 io:format
	   ("Active:~n~p~n",
	    [Node#node.active_implementations]),
	 io:format
	   ("java_node=~p reuses_left=~p is_alive=~p~n"++
	      "pending_starts=~p location=~p counter=~p~n",
	    [Node#node.java_node_id, Node#node.reuses_left,
	     Node#node.is_alive,Node#node.pending_starts,Node#node.location,
	     Node#node.counter]),
	 lists:foreach
	   (fun (NodeImplementation) ->
		Implementation =
		  read_implementation
		    (NodeImplementation#java_reuse_implementation.id),
		io:format("~p,",[Implementation])
	    end, Node#node.implementations),
	 io:format("~n~n")
     end, State#state.nodes),
  {noreply,State,?TIMEOUT}.

code_change(_,_,_) ->
  {error,nyi}.

terminate(ordered,_State) ->
  ets:delete(?ETS_TABLE),
  ok.

do_something(Node,#state{recipe=Recipe} = State) ->
  #node{active_implementations=Actives, pending_starts=Pending, is_busy=IsBusy,
	timeout=Timeout, reuses_left=ReusesLeft, is_alive=IsAlive} = Node,
  if
    IsBusy ->
      ok;

    (not Recipe#java_reuse_recipe.permit_concurrent_implementations) andalso (Actives=/=[]) ->
      ok;

    not(IsAlive), Pending=/=[] ->
      start_node(Node,State);

    ReusesLeft =< 0, Actives==[], Pending=/=[] ->
      start_node(Node,State);
    
    Actives==[], Timeout, Pending=/=[] ->
      start_node(Node,State);
    
    ReusesLeft>0, Pending=/=[] ->
      node_start_implementation(Node,State);
    
    true ->
      ok
  end.

start_node(Node = #node{node_id=NodeId,pending_starts=Pending,implementations=Implementations},State) ->
  ?LOG("Will start node ~p~n",[NodeId]),
  [{ImplementationId,_}|_] = Pending,
  Implementation = read_implementation(ImplementationId),
  #java_reuse_recipe{args=PreArgs,classpath=PreClassPath} = State#state.recipe,
  ClassPath = [Implementation#java_reuse_implementation.path|PreClassPath],
  Args = [{add_to_java_classpath,ClassPath}|PreArgs],
  {ErlangNode,_,_} = select_node(),
  CompleteSpec = 
    case ErlangNode of
      local -> Args;
      _ -> [{erlang_remote,ErlangNode}|Args]
    end,
  ?LOG
    ("starting node ~p at ~p; principal implementation ~p~n",
     [NodeId,ErlangNode,ImplementationId]),
  ServerId = self(),
  write_node(Node#node{is_busy=true}),
  spawn_link
    (fun () ->
	 OldJavaNodeId = Node#node.java_node_id,
	 if
	   OldJavaNodeId=/=void, Node#node.timeout ->
	     java:brutally_terminate(OldJavaNodeId);
	   OldJavaNodeId=/=void ->
	     java:terminate(OldJavaNodeId);
	   true ->
	     ok
	 end,
	 {ok,JavaNodeId} = 
	   java:start_node(CompleteSpec),
	 DefaultClassPool =
	   java:call_static(JavaNodeId,'javassist.ClassPool',getDefault,[]),
	 lists:foreach
	   (fun (NodeImplementation) ->
		ClassPool =
		  if
		    NodeImplementation#java_reuse_implementation.id
		    == ImplementationId ->
		      DefaultClassPool;
		    true ->
		      void
		  end,
		NewNodeImplementation =
		  NodeImplementation#java_reuse_implementation
		  {classpool=ClassPool},
		write_implementation(NewNodeImplementation)
	    end, Implementations),
	 gen_server:cast(ServerId,{node_created,{NodeId,JavaNodeId}})
     end).

node_start_implementation(#node{pending_starts=[{ImplementationId,Reply}|Pendings]} = Node,State) ->
  ?LOG
    ("start_implementation(~p) at node ~p~n",
     [ImplementationId,Node#node.node_id]),
  ServerId = self(),
  write_node
    (Node#node
     {is_busy=true,
      pending_starts=Pendings,
      reuses_left=Node#node.reuses_left-1}),
  spawn_link(fun () -> create_implementation(ImplementationId,Reply,Node,State,ServerId) end).

create_implementation(ImplementationId,
	    Reply,
	    #node{node_id=NodeId,
		  counter=Counter,
		  java_node_id=JavaNodeId} = Node,
	    State,
	    ServerId) ->
  Implementation
    = #java_reuse_implementation{classpool=ClassPool,path=Path}
    = read_implementation(ImplementationId),
  #java_reuse_recipe{classes=Classes} =
    State#state.recipe,
  if
    %% We have to create a new classpool for the implementation
    ClassPool == void ->
      ?LOG("before creating classpool~n",[]),
      NewClassPool = java:new(JavaNodeId,'javassist.ClassPool',[]),
      ?LOG("before insert in classpath~n",[]),
      java:call
	(NewClassPool,
	 insertClassPath,
	 [java:list_to_string(JavaNodeId,Path)]),
      NewImplementation =
	Implementation#java_reuse_implementation{classpool=NewClassPool},
      write_implementation(NewImplementation),
      create_implementation(ImplementationId,Reply,Node,State,ServerId);
    
    %% The old classpool can be reused
    true ->
      NewClasses = 
	lists:map
	(fun (ClassName) ->
	     NewClassName = ClassName++"_"++integer_to_list(Counter),
	     ?LOG
		("Implementation ~p copying to class ~p at node ~p~n",
		 [ImplementationId,NewClassName,NodeId]),
	     ?LOG("ClassPool=~p~n",[ClassPool]),
	     CtClass =
	       java:call
		 (ClassPool,
		  getAndRename,
		  [java:list_to_string(JavaNodeId,ClassName),
		   java:list_to_string(JavaNodeId,NewClassName)]),
	     Class = java:call(CtClass,toClass,[]),
	     %% The next line registers the new class under the right
	     %% name for the JavaErlang library
	     ?LOG
		("acquiring class ~p (ctclass=~p) at node_id ~p~n",
		 [Class,CtClass,java:node_id(Class)]),
	     java:acquire_class(java:node_id(Class),Class,list_to_atom(ClassName)),
	     list_to_atom(NewClassName)
	 end, Classes),
      gen_server:cast
	(ServerId,
	 {implementation_started,{ImplementationId,Reply,NodeId,JavaNodeId,NewClasses}})
  end.

map_implementations_to_nodes(Implementations,#java_reuse_recipe{reuse_limit=ReuseLimit,num_node_shares=NumShares}) ->
  {Cnt,Curr,Accum} =
    lists:foldr
      (fun (Implementation,{Count,Current,Acc}) ->
	   if
	     Count < NumShares ->
	       {Count+1,[Implementation|Current],Acc};
	     true ->
	       {1,[Implementation],[Current|Acc]}
	   end
       end, {0,[],[]}, Implementations),
  ImplementationsAtNodes =
    if
      Cnt==0 -> Accum;
      true -> [Curr|Accum]
    end,
  {_,Nodes} = 
    lists:foldr
      (fun (ImplementationsPerNode,{I,Acc}) ->
	   ImplementationsPN =
	     lists:map
	       (fun ({Id,Path}) ->
		    #java_reuse_implementation{id=Id,node_id=I,path=Path}
		end, ImplementationsPerNode),
	   {I+1,
	    [#node
	     {node_id=I,
	      timeout=false,
	      java_node_id=void,
	      is_alive=false,
	      reuses_left=ReuseLimit,
	      active_implementations=[],
	      is_busy=false,
	      pending_starts=[],
	      implementations=ImplementationsPN,
	      counter=0}|Acc]}
       end, {0,[]}, ImplementationsAtNodes),
  lists:foreach
    (fun (Node) ->
	 write_node(Node),
	 lists:foreach
	   (fun (Implementation) ->
		write_implementation(Implementation)
	    end, Node#node.implementations)
     end, Nodes),
  Nodes.
  
make_recipe(Args,Classes,ClassPath,ReuseLimit,NumShares,ConcurrencyPermitted) 
  when is_list(Args), is_list(Classes), is_list(ClassPath) ->
  #java_reuse_recipe{args=Args,
	  classes=Classes,
	  classpath=ClassPath,
	  reuse_limit=ReuseLimit*NumShares,
	  permit_concurrent_implementations=ConcurrencyPermitted,
	  num_node_shares=NumShares}.

start_select(Plan) ->
  Mod =
    lists:foldl(fun ({_,Factor,_},Acc) -> Factor+Acc end, 0, Plan),
  FloatPlan =
    lists:map(fun ({N,W,O}) -> {W/Mod,{N,W,O}} end, Plan),
  SortedFloatPlan =
    lists:sort
      (fun ({TF1,_},{TF2,_}) -> TF1>TF2 end, FloatPlan),
  Distances = 
    lists:map(fun ({TF,Item}) -> {TF,0,0,TF,Item} end, SortedFloatPlan),
  ets:insert(?ETS_TABLE,{dist_state,{0,Distances}}).

select_node() ->
  case ets:lookup(?ETS_TABLE,dist_state) of
    [{_,{Total,[{_,_,N,TF,Item}|Rest]}}] ->
      ets:insert
	(?ETS_TABLE,
	 {dist_state,
	  {Total+1,
	   recompute_distances(Total+1,[{void,void,N+1,TF,Item}|Rest])}}),
      Item
  end.

recompute_distances(Total,Items) ->
  lists:sort
    (fun ({Dist1,_,_,_,_},{Dist2,_,_,_,_}) -> Dist1>Dist2 end,
     lists:map
       (fun ({_,_,N,Fraction,Item}) ->
	    RealFraction = N/Total,
	    Distance = Fraction-RealFraction,
	    {Distance,RealFraction,N,Fraction,Item}
	end, Items)).

start_erlang_nodes(Plan) ->
  start_erlang_nodes(Plan,[]).
start_erlang_nodes([],_Started) -> [];
start_erlang_nodes([{local,Factor,LogLevel}|Rest],Started) ->
  ?LOG("skipping starting local host~n",[]),
  [{node(),Factor,LogLevel}|start_erlang_nodes(Rest,[node()|Started])];
start_erlang_nodes([{Host,Factor,LogLevel}|Rest],Started) ->
  case string:str(net_adm:localhost(),Host) of
    1 ->
      ?LOG("skipping starting local host~n",[]),
      [{node(),Factor,LogLevel}|start_erlang_nodes(Rest,[node()|Started])];
    _ -> 
      ?LOG("will start a node at ~p~n",[list_to_atom(Host)]),
      case slave:start_link(list_to_atom(Host)) of
        {ok,Node} ->
	  spawn(Node,java,set_loglevel,[LogLevel]),
	  timer:sleep(10),
          [{Node,Factor,LogLevel}|start_erlang_nodes(Rest,[Node|Started])];
        {error,_Reason} ->
          ?LOG
            ("Cannot start a node at ~s due to ~p~n",
             [Host,_Reason]),
          lists:foreach
            (fun (Node) ->
                 if Node=/=node() -> slave:stop(Node); true -> ok end
             end,
             Started),
          throw(badarg)
      end
  end.

write_implementation(Implementation) ->
  Id = Implementation#java_reuse_implementation.id,
  ets:insert(?ETS_TABLE,{{implementation,Id},Implementation}).

read_implementation(Id) ->
  [{_,Implementation}] = ets:lookup(?ETS_TABLE,{implementation,Id}),
  Implementation.

write_node(Node) ->
  ets:insert(?ETS_TABLE,{{node,Node#node.node_id},Node}).

read_node(NodeId) ->
  [{_,Node}] = ets:lookup(?ETS_TABLE,{node,NodeId}),
  Node.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Implementations,[Recipe,DistPlan]) ->
  {ok,_Server} = 
    gen_server:start_link
      ({local,?GEN_SERVER_NAME},?MODULE,
       [Recipe,Implementations,DistPlan],[]),
  ok.

start_implementation(Id,_Private) ->
  gen_server:call(?GEN_SERVER_NAME,{start_implementation,Id},infinity).

stop_implementation(Id,_Private,Result) ->
  gen_server:call(?GEN_SERVER_NAME,{stop_implementation,Id,Result},infinity).
  
finish() ->
  gen_server:call(?GEN_SERVER_NAME,terminate,infinity).


