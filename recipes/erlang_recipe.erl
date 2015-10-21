-module(erlang_recipe).

-define(ETS_TABLE,ranker_erlang_ets).

-export([start/2,start_implementation/1,stop_implementation/2,finish/0]).

start(Implementations,_) ->
  ranker_create_ets_table:ensure_open(?ETS_TABLE),
  ImplementationIds =
    lists:map
      (fun (Implementation) ->
	   {Implementation#implementation.implementation_id,Implementation}
       end, Implementations), 
  ets:insert(?ETS_TABLE,{implementations,ImplementationIds}).
  
start_implementation(Id) ->
  [{_,ImplementationIds}] = ets:lookup(?ETS_TABLE,implementations),
  {_,Implementation} = lists:keyfind(Id,1,ImplementationIds),
  ok.

stop_implementation(_,_) ->
  ok.

finish() ->
  ok.

  
