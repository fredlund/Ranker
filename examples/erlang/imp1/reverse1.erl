-module(reverse1).

-export([reverse/1]).

reverse([]) ->
  [];
reverse([Hd|Tl]) ->
  reverse(Tl)++[Hd].


