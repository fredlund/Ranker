-module(reverse7).

-export([reverse/1]).

reverse(L) ->
  reverse(L,[]).

reverse([],Result) ->
  Result;
reverse([Hd|Tl],L) ->
  reverse(Tl,[Hd|L]).



