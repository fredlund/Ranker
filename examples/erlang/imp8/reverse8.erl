-module(reverse8).

-export([reverse/1]).

reverse(L) ->
  reverse(L,[]).

reverse([],Result) ->
  Result;
reverse([Hd|Tl],L) ->
  reverse(Tl,L).



