-module(corr).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-record(state,{started,object,args,id}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initial_state() ->
  #state{started=false,args=void}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_pre(State) ->
  not(State#state.started).

start_args(_State) ->
  [{var,id},{var,imp}].

start_post(State,Args,Result) ->
  not_exception(State#state.id,{?MODULE,start,Args},Result).

start_next(State,Var,[Id,_]) ->
  State#state{started=true,object=Var,id=Id}.

start(_Id,{Node,[Class]}) ->
  java:new(Node,Class,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_pre(State) ->
  State#state.started andalso (State#state.args==void).

list_args(State) ->
  [State#state.object,list_or_null(small_nat_or_null())].

list_post(_State,_Args,_Result) ->
  true.

list_next(State,Var,_Args) ->
  State#state{args=Var}.

list(Object,List) ->
  ListArg = 
    if
      List==null -> null;
      true -> list_to_arrayList(java:node_id(Object),List)
    end,
  {List,ListArg}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort_pre(State) ->
  (State#state.args=/=void).

sort_args(State) ->
  [State#state.object, State#state.args].

sort_post(State,Args=[_,{ModelList,List}],Result) ->
  expect_eq
    (State#state.id,
    {?MODULE,sort,Args},
     model_sort(ModelList),
     extract_ints(arrayList_to_list(Result)))
    andalso
    expect_eq
    (State#state.id,
     {?MODULE,sort,Args},
     ModelList,
     extract_ints(arrayList_to_list(List))).

sort_next(State,_Var,_Args) ->
  State#state{args=void}.

sort(Object,{_ModelList,List}) ->
  catch java:call(Object,insertionSort,[List]).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

small_nat_or_null() ->
  eqc_gen:frequency([{1,null},{3,choose(0,3)}]).

list_or_null(Generator) ->
  frequency([{50,list(Generator)},{1,null}]).

model_sort(null) ->
  null;
model_sort(L) ->
  lists:sort(fun (E1,E2) -> leq(E1,E2) end, L).

leq(null,_) ->      
  true;
leq(_,null) -> 
  false;
leq(N,M) -> 
  N=<M.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_to_arrayList(Node,L) ->
  List = java:new(Node,'java.util.ArrayList',[]),
  list_to_arrayList(Node,L,List).

list_to_arrayList(_Node,[],List) ->
  List;
list_to_arrayList(Node,[First|Rest],List) ->
  FirstInteger = 
    if 
      First == null -> null;
      true -> java:new(Node,'java.lang.Integer',[First])
    end,
  java:call(List,add,[FirstInteger]),
  list_to_arrayList(Node,Rest,List).

arrayList_to_list(null) ->
  null;
arrayList_to_list(Exc={java_exception,_}) ->
  Exc;
arrayList_to_list(java_timeout) ->
  java_timeout;
arrayList_to_list(List) ->
  FirstPos = 0,
  Size = java:call(List,size,[]),
  list_elements(List,FirstPos,Size).

list_elements(_,Pos,Size) when Pos>=Size ->
  [];
list_elements(_List,_,java_timeout) ->
  java_timeout;
list_elements(List,Pos,Size) ->
  Elements = list_elements(List,Pos+1,Size),
  case Elements of
    java_timeout -> java_timeout;
    L when is_list(L) ->
      case java:call(List,get,[Pos]) of
	java_timeout -> java_timeout;
	Element -> [Element|Elements]
      end
  end.

extract_ints(L) when is_list(L) ->
  lists:map(fun (Object) ->
		if
		  Object==null -> null;
		  true -> java:call(Object,intValue,[]) 
		end
	    end, L);
extract_ints(Other) ->
  Other.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
not_exception(Id,Call,java_timeout) ->
  io:format("***~p: call ~p timed out~n",[Id,Call]),
  io:format("~n"),
  false;
not_exception(Id,Call,{java_exception,Exc}) ->
  io:format("***~p: call ~p returns a java exception~n",[Id,Call]),
  java:print_stacktrace(Exc),
  io:format("~n"),
  false;
not_exception(_,_,Return) ->
  case Return of
    _ when is_integer(Return) -> ok;
    {object,_,_,_,_} -> ok;
    _ -> io:format("Return is ~p~n",[Return])
  end,
  true.

expect_eq(Id,Call,Value,Result) ->
  case {Value,Result} of
    {_,java_timeout} ->
      io:format
	("~n***~p: ~p: expected postcondition value ~s; code timed out~n",
	 [Id,Call,print_value(Value)]),
      false;
    {{exception,ClassName},{java_exception,Obj}} ->
      case java:instanceof(Obj,ClassName) of
        true -> true;
        false -> 
          io:format
            ("~n***~p: ~p: expected exception ~p=/=~p~n",
             [Id,Call,ClassName,java:getSimpleClassName(Obj)]),
          false;
        {java_exception,Exception} ->
          io:format
            ("*** ~p: exception ~p raised for instanceof(~p,~p)~n",
             [Id,java:getSimpleClassName(Exception),Obj,ClassName]),
          java:print_stacktrace(Exception),
          throw(expect_eq)
      end;
    _ ->
      if Value==Result -> true;
         true ->
          case 
            is_integer(Value) andalso
            java:is_object_ref(Result) andalso
            java:instanceof(Result,'java.lang.Integer') of
            true ->
              expect_eq(Id,Call,Value,java:call(Result,intValue,[]));
            false ->
              io:format
                ("~n***~p: ~p: expected postcondition value ~s=/=~s~n",
                 [Id,Call,print_value(Value),print_value(Result)]),
              false
          end
      end
  end.

print_value(Object={object,_,_Obj}) ->
  io_lib:format("~p : ~p",[Object,java:getSimpleClassName(Object)]);
print_value({java_exception,Obj}) ->
  io_lib:format("exception ~p",[java:getSimpleClassName(Obj)]);
print_value(Object) ->
  io_lib:format("~p",[Object]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generator() ->
  eqc_statem:commands(?MODULE).

prop(Cmds,ImpId,ImpData) ->
  case eqc_statem:run_commands(?MODULE,Cmds,[{id,ImpId},{imp,ImpData}]) of
    {_,_,Res} ->
      Res==ok;
    _ ->
      false
  end.


  
  

