-module(erlang_recipe).

-export([start/2,start_implementation/1,stop_implementation/2,finish/0]).

start(_Implementations,_) ->
  ok.

start_implementation(_) ->
  ok.

stop_implementation(_,_) ->
  ok.

finish() ->
  ok.

  
