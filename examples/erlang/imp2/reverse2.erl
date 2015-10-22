-module(reverse2).

-export([reverse/1]).

reverse([Hd|Tl]) ->
  reverse(Tl)++[Hd].


