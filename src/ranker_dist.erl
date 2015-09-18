-module(ranker_dist).

-compile(export_all).

-ifdef(debug).
-define(LOG(X,Y), io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-define(DEBUGVAL(),true).
-else.
-define(LOG(X,Y), ok).
-define(DEBUGVAL(),false).
-endif.

start_node(N,Plan,NodeSpec) ->
  Node = select_node(N,Plan),
  CompleteSpec = [{erlang_remote,Node}|NodeSpec],
  java:start_node(CompleteSpec).

select_node(N,Plan) ->
  Mod =
    lists:foldl(fun ({_,Factor},Acc) -> Factor+Acc end,0,Plan),
  Value =
    N rem Mod,
  select(Value,Plan).

start_node(Plan,NodeSpec) ->
  Node = select_node(Plan),
  CompleteSpec = 
    case Node of
      local -> NodeSpec;
      _ -> [{erlang_remote,Node}|NodeSpec]
    end,
  java:start_node(CompleteSpec).

select_node(Plan) ->
  Mod =
    lists:foldl(fun ({_,Factor},Acc) -> Factor+Acc end,0,Plan),
  try ets:update_counter(eqc_result,dist_counter,1) of
      N when is_integer(N) -> select(N rem Mod,Plan)
  catch _:badarg ->
      try ets:insert(eqc_result,{dist_counter,0}), select_node(Plan)
      catch _:badarg -> select_node(Plan) end
  end.

select(Value,Plan) ->
  select(Value,Plan,0).
select(Value,[{Node,Factor}|Rest],Acc) ->
  Limit = Acc+Factor,
  if
    Value<Limit -> Node;
    true-> select(Value,Rest,Limit)
  end.
    
start_erlang_nodes(Plan) ->
  start_erlang_nodes(Plan,[]).
start_erlang_nodes([],_Started) -> [];
start_erlang_nodes([{local,Factor,_LogLevel}|Rest],Started) ->
  io:format("skipping starting local host~n",[]),
  [{node(),Factor}|start_erlang_nodes(Rest,[node()|Started])];
start_erlang_nodes([{Host,Factor,LogLevel}|Rest],Started) ->
  case string:str(net_adm:localhost(),Host) of
    1 ->
      io:format("skipping starting local host~n",[]),
      [{node(),Factor}|start_erlang_nodes(Rest,[node()|Started])];
    _ -> 
      io:format("will start a node at ~p~n",[list_to_atom(Host)]),
      case slave:start_link(list_to_atom(Host)) of
        {ok,Node} ->
	  spawn(Node,java,set_loglevel,[LogLevel]),
	  timer:sleep(10),
          [{Node,Factor}|start_erlang_nodes(Rest,[Node|Started])];
        {error,Reason} ->
          io:format
            ("Cannot start a node at ~s due to ~p~n",
             [Host,Reason]),
          lists:foreach
            (fun (Node) ->
                 if Node=/=node() -> slave:stop(Node); true -> ok end
             end,
             Started),
          throw(badarg)
      end
  end.

test() ->    
  HostPlan =
    [
     {"sadrach.ls.fi.upm.es",1},
     {"ruth.ls.fi.upm.es",1},
     {"babel.ls.fi.upm.es",2},
     {"moises.ls.fi.upm.es",1}
    ],
  Assignments =
    lists:foldl
      (fun (N,Count) -> 
	   Node = start_node(N,HostPlan,[]),
	   case lists:keyfind(Node,1,Count) of
	     {_,V} -> lists:keyreplace(Node,1,Count,{Node,V+1});
	     _ -> lists:keystore(Node,1,Count,{Node,1})
	   end
       end, [], lists:seq(1,100)),
  io:format("~p~n",[Assignments]).
  
	
