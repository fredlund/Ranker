-module(reverse5).

-export([reverse/1]).

reverse([]) ->
  [0];
reverse([Hd|Tl]) ->
  reverse(Tl)++[Hd].


