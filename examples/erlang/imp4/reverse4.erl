-module(reverse4).

-export([reverse/1]).

reverse([]) ->
  [];
reverse([Hd|Tl]) ->
  [Hd|reverse(Tl)].


