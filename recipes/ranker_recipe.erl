-module(ranker_recipe).

-callback start([{any(),any()}],any()) -> ok.
-callback start_implementation(any(),any()) -> any().
-callback stop_implementation(any(),any(),any()) -> ok.
-callback finish() -> ok.
  
   
  

  
