-module(reverse10).

-export([reverse/1]).

reverse([]) ->
  [];
reverse([Hd|Tl]) ->
  reverse([Hd|Tl]).





