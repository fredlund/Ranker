-module(corr).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

generator() ->
  list(nat()).

prop(L,_,Imp) ->
  (catch Imp:reverse(L))==lists:reverse(L).
