-module(erlang_recipe).

-include("implementation.hrl").

-define(ETS_TABLE,ranker_erlang_ets).

-export([start/2,start_implementation/1,stop_implementation/2,finish/0]).

start(Implementations,_) ->
  ranker_create_ets_table:ensure_open(?ETS_TABLE),
  ImplementationIds =
    lists:map
      (fun (Implementation) ->
	   {Implementation#implementation.id,Implementation}
       end, Implementations), 
  ets:insert(?ETS_TABLE,{implementations,ImplementationIds}).
  
start_implementation(_) ->
  ok.

stop_implementation(_,_) ->
  ok.

finish() ->
  ok.

  
