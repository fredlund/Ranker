-module(erlang_recipe).

-export([start/2,start_implementation/1,stop_implementation/2,finish/0]).

-include("implementation.hrl").

-define(ETS_TABLE,ranker_erlang_ets).

start(Implementations,_) ->
  ranker_create_ets_table:ensure_open(?ETS_TABLE),
  ets:insert
    (?ETS_TABLE,
     {implementations,
      lists:map(fun (Imp) ->
		    {Imp#implementation.id,Imp#implementation.private}
		end, Implementations)}),
  ok.

start_implementation(Id) ->
  [{_,Implementations}] = ets:lookup(?ETS_TABLE,implementations),
  {_,Private} = lists:keyfind(Id,1,Implementations),
  Private.

stop_implementation(_,_) ->
  ok.

finish() ->
  ok.

  
