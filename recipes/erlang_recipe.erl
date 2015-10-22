-module(erlang_recipe).

-export([start/2,start_implementation/2,stop_implementation/3,finish/0]).

-include("implementation.hrl").

start(_Implementations,_) ->
  ok.

start_implementation(_Id,Private) ->
  Private.

stop_implementation(_,_,_) ->
  ok.

finish() ->
  ok.

  
