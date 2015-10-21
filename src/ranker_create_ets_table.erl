-module(ranker_create_ets_table).

-compile(export_all).

new(Name,Options) ->
  SelfPid = self(),
  case ets:info(Name) of
    undefined ->
      spawn(fun () ->
		try 
		  ets:new(Name,[named_table,{keypos,1},public|Options]),
		  wait_until_stable(Name),
		  SelfPid!{initialized,true},
		  wait_forever()
		catch _:_ ->
		    wait_until_stable(Name),
		    SelfPid!{initialized,false}
		end
	    end),
      receive
	{initialized,Value} -> Value
      end;
    _ ->
      wait_until_stable(Name),
      false
  end.

ensure_open(Name) ->
  case ets:info(Name) of
    undefined ->
      new(Name,[]);
    _ ->
      ok
  end.

wait_until_stable(Name) ->
  case ets:info(Name) of
    Info when is_list(Info) ->
      ok;
    _ ->
      timer:sleep(10),
      wait_until_stable(Name)
  end.

wait_forever() ->
  receive _ -> wait_forever() end.

      
