-module(java_reuse_recipe).
-compile(export_all).

%% Initial recipe state
inital_state(_Implementations) ->
  ok.

%% Initial implementation state
init_implementation(_Implementation,_RecipeState) ->
  ok.

%% 
prepare_implementation(_ImpState,_RecipeState) ->
  ok.

post_run(_RecipeState,_Fails,_Timeouts) ->
  ok.

