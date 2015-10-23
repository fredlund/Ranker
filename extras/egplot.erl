%% How to compare graphs? Maybe an option compare([Windows])
%% to use the same coordinates and size. And an option reset([Windows])
%% to choose freely.
%% 

-module(egplot).

-compile(export_all).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include("egplot.hrl").

-define(TERMINAL,"x11").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
  gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  {ok, #egstate{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({new_window,Name,Users,Options},
	    _From,
	    State=#egstate{windows=Windows}) ->
  NewWindow = create_new_window(Name,Users,Options),
  Port = NewWindow#window.port,
  NewState = State#egstate{windows=[{Port,NewWindow}|Windows]},
  Reply = ok,
  {reply, Reply, NewState};
handle_call({move,ToWindowName},_From,State) ->
  case find_window(ToWindowName,State) of
    {ok,ToWindow} ->
      case State#egstate.clipboard of
	{Port,Users} when Users=/=[] ->
	  {_,FromWindow} =
	    lists:keyfind(Port,1,State#egstate.windows),
	  if
	    Port =/= ToWindow#window.port ->
	      NewFromWindow = 
		FromWindow#window
		{users=
		   lists:filter
		     (fun (User) -> not(lists:member(User,Users)) end,
		      FromWindow#window.users)},
	      NewToWindow =
		ToWindow#window
		{users=Users++ToWindow#window.users},
	      redraw(NewFromWindow),
	      redraw(NewToWindow),
	      NewWindows =
		lists:keyreplace
		  (ToWindow#window.port,
		   1,
		   lists:keyreplace
		     (FromWindow#window.port,
		      1,
		      State#egstate.windows,
		      {FromWindow#window.port,NewFromWindow}),
		   {ToWindow#window.port,NewToWindow}),
	      {reply, ok, State#egstate{windows=NewWindows,clipboard=void}};
	    true -> {reply, nok0, State}
	  end;
	_ ->
	  io:format("clipboard is ~p~n",[State#egstate.clipboard]),
	  {reply, nok1, State}
      end;
    _ -> {reply, nok2, State}
  end;
handle_call({save,FileName},_From,State) ->
  if
    is_list(FileName) ->
      case file:write_file(FileName,term_to_binary(State)) of
	ok -> {reply, ok, State};
	_ -> {reply, nok, State}
      end;
    true -> {reply, nok, State}
  end;
handle_call({restore,FileName},_From,State) ->
  if
    is_list(FileName) ->
      case file:read_file(FileName) of
	{ok,Binary} ->
	  NewState = binary_to_term(Binary),
	  if
	    is_record(NewState,egstate) ->
	      lists:foreach
		(fun ({_,Window}) -> close(Window) end,
		 State#egstate.windows),
	      NewWindows =
		lists:map
		  (fun ({_,Window}) ->
		       NewWindow = create_new_window(Window),
		       {NewWindow#window.port,NewWindow}
		   end,
		   NewState#egstate.windows),
	      {reply, ok, NewState#egstate{windows=NewWindows}};
	    true -> {reply, nok, State}
	  end;
	_ -> {reply, nok, State}
      end;
    true -> {reply, nok, State}
  end;
handle_call({grade,WindowName,N},_From,State) ->
  if
    is_list(WindowName) ->
      case find_window(WindowName,State) of
	{ok,Window} ->
	  NewState =
	    State#egstate
	    {windows=
	       lists:keyreplace
		 (Window#window.port,
		  1,
		  State#egstate.windows,
		  {Window#window.port,
		   Window#window{grade=N}})},
	  {reply,ok,NewState};
	_ -> {reply,nok,State}
      end;
    true -> {reply,nok,State}
  end;
handle_call({info,WindowName},_From,State) ->
  if
    is_list(WindowName) ->
      case find_window(WindowName,State) of
	{ok,Window} ->
	  io:format
	    ("Window ~p has grade ~p and users~n~p~n",
	     [WindowName,Window#window.grade,
	      lists:map
		(fun (User) -> User#user.name end,
		 Window#window.users)]),
	  {reply,ok,State};
	_ -> {reply,nok,State}
      end;
    true -> {reply,nok,State}
  end.

find_window(Name,State) ->
  find_window1(Name,State#egstate.windows).
find_window1(_Name,[]) ->
  false;
find_window1(Name,[{_,Window}|Rest]) ->
  if
    Name==Window#window.name ->
      {ok,Window};
    true ->
      find_window1(Name,Rest)
  end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({Port,{data,{eol,Msg}}}, State) ->
  NewState = interpret_port_msg(Port,Msg,State),
  {noreply, NewState};
handle_info(Info, State) ->
  io:format("got unknown message ~p~n",[Info]),
  {noreply, State}.

interpret_port_msg(Port,Msg,State) ->
  io:format("Port=~p WinPort1=~p~n",[Port,element(1,hd(State#egstate.windows))]),
  {_,Window} = lists:keyfind(Port,1,State#egstate.windows),
  {ok,[Cmd],Rest} = io_lib:fread("~s",Msg),
  {NewWindow,NewState} =
    case Cmd of
      "coord:" ->
	{X,Rest1} = get_coord(Rest),
	{ok,_,Rest2} = io_lib:fread(",",Rest1),
	{Y,_} = get_coord(Rest2),
	io:format
	  ("got coordinates ~p,~p on window ~p; state is ~p~n",
	   [X,Y,Window#window.name,Window#window.state]),
	case Window#window.state of
	  inert ->
	    {Window#window{state={coord,[X,Y]}},State};
	  {coord,[X1,Y1]} ->
	    Users = find_matching_curves(X1,Y1,X,Y,Window),
	    lists:foreach
	      (fun (User) ->
		   io:format
		     ("User ~p was selected in window ~p~n",
		      [User#user.name,Window#window.name])
	       end, Users),
	    {Window#window{state=inert},State#egstate{clipboard={Port,Users}}}
	end;
      "cancel" ->
	io:format
	  ("got cancel on window ~p~n",
	   [Window#window.name]),
	{Window#window{state=inert},State};
      "toggle_title" ->
	io:format
	  ("got toggle_title on window ~p~n",
	   [Window#window.name]),
	Options =
	  Window#window.options,
	TitleValue =
	  proplists:get_value(title,Options,true),
	Window1 = 
	  Window#window{options=[{title,not(TitleValue)}|Options]},
	redraw(Window1),
	{Window1,State};
      "toggle_complexity" ->
	io:format
	  ("got toggle_complexity on window ~p~n",
	   [Window#window.name]),
	Options =
	  Window#window.options,
	ComplexityValue =
	  proplists:get_value(complexity,Options,false),
	Window1 = 
	  Window#window{options=[{complexity,not(ComplexityValue)}|Options]},
	redraw(Window1),
	{Window1,State};
      "dump" ->
	io:format
	  ("got dump on window ~p~n",
	   [Window#window.name]),
	dump(Window),
	{Window,State}
    end,
  NewState#egstate
    {windows=lists:keyreplace(Port,1,State#egstate.windows,{Port,NewWindow})}.

find_matching_curves(X1,Y1,X2,Y2,Window) ->
  {MinX,MaxX} = if X1 > X2 -> {X2,X1}; true -> {X1,X2} end,
  {MinY,MaxY} = if Y1 > Y2 -> {Y2,Y1}; true -> {Y1,Y2} end,
  lists:filter
    (fun (User) -> 
	 Coords = User#user.coords,
	 lists:any(fun ([Xu,Yu]) ->
		       (Xu>=MinX)
			 andalso (Xu=<MaxX)
			 andalso (Yu>=MinY)
			 andalso (Yu=<MaxY)
		   end, Coords)
     end, Window#window.users).

get_coord(String) ->
  case io_lib:fread("~f",String) of
    {ok,[Coord],Rest} ->
      {Coord,Rest};
    {error,_} ->
      case io_lib:fread("~d",String) of
	{ok,[Coord],Rest} ->
	  {Coord,Rest};
	{error,_} ->
	  io:format("cannot read ~p~n",[String]),
	  throw(bad)
      end
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% API
%%%===================================================================

new_window(Name) ->
  new_window(Name,[]).

new_window(Name,Users) ->
  new_window(Name,Users,[]).

new_window(Name,Users,Options) ->
  gen_server:call(?SERVER,{new_window,Name,Users,Options}).

move(ToWindow) ->
  gen_server:call(?SERVER,{move,ToWindow}).

grade(Window,N) ->
  gen_server:call(?SERVER,{grade,Window,N}).

info(Window) ->
  gen_server:call(?SERVER,{info,Window}).

save(FileName) ->
  gen_server:call(?SERVER,{save,FileName}).

restore(FileName) ->
  gen_server:call(?SERVER,{restore,FileName}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_new_window(Window) when is_record(Window,window) ->
  Port = open_port({spawn,"gnuplot"},[{line,10000}]),
  NewWindow = Window#window{port=Port},
  start_gnuplot(Window),
  NewWindow.
create_new_window(Name,UserStructs,Options) ->
  Port = open_port({spawn,"gnuplot"},[{line,10000}]),
  Window =
    #window
    {name=Name,users=UserStructs,port=Port,options=Options},
  start_gnuplot(Window),
  Window.

start_gnuplot(#window{users=Users,options=Options,port=Port,name=Title}) ->
  TerminalCommand =
    case proplists:get_value(screen,Options) of
      {SizeX,SizeY} when is_integer(SizeX), is_integer(SizeY) ->
	io_lib:format
	  ("set terminal "++?TERMINAL++" size ~p, ~p title '~s'\n",
	   [SizeX,SizeY,Title]);
      _ ->
	io_lib:format
	  ("set terminal "++?TERMINAL++" title '~s'\n",
	   [Title])
    end,
  Port!{self(),{command,TerminalCommand}},
  Port!{self(),{command,"set mouse\n"}},
  Port!{self(),{command,"bind all \"Button1\" 'result=sprintf(\"coord: \%g,\%g\",MOUSE_X, MOUSE_Y); print result';\n"}},
  Port!{self(),{command,"bind all \"c\" 'result=sprintf(\"cancel\"); print result';\n"}},
  Port!{self(),{command,"bind all \"t\" 'result=sprintf(\"toggle_title\"); print result';\n"}},
  Port!{self(),{command,"bind all \"o\" 'result=sprintf(\"toggle_complexity\"); print result';\n"}},
  Port!{self(),{command,"bind all \"d\" 'result=sprintf(\"dump\"); print result';\n"}},
  Port!{self(),{command,"set print \"-\"\n"}},
  plot_users(Port,Options,Users).

plot_users(Port,Options,Users) ->
  UsersSpec = format_users(Users,Options),
  case UsersSpec of
    "" -> ok;
    FormatUsers -> Port!{self(),{command,"plot "++FormatUsers++"\n"}}
  end.

redraw(Window) ->
  plot_users(Window#window.port,Window#window.options,Window#window.users).

dump(#window{users=Users,options=Options,port=Port,name=_Title}) ->
  TerminalCommand = 
    case proplists:get_value(screen,Options) of
      {SizeX,SizeY} when is_integer(SizeX), is_integer(SizeY) ->
	io_lib:format
	  ("set terminal pngcairo size ~p, ~p\n",
	   [SizeX,SizeY]);
      _ ->
	io_lib:format
	  ("set terminal png\n",
	   [])
    end,
  Port!{self(),{command,TerminalCommand}},
  Port!{self(),{command,"set output 'dumped.png'\n"}},
  plot_users(Port,Options,Users).

close(Window) ->
  (Window#window.port)!{self(),{command,"quit\n"}}.

read_user(UserName,IsWorst) ->
  read_user(UserName,IsWorst,[]).
read_user(UserName,IsWorst,Options) ->
  Class =
    if 
      IsWorst ->
	"worst";
      true ->
	"best"
    end,
  Complexity =
    case proplists:get_value(complexity,Options,false) of
      false -> [];
      true -> 
	{ok,B} = file:read_file("complexity_"++Class++"_"++UserName),
	format_complexity(string:strip(binary_to_list(B),both,$\n))
    end,
  Name = 
    case proplists:get_value(name,Options,false) of
      false -> UserName;
      OptName -> OptName
    end,
  FileName = "data_"++Class++"_"++UserName,
  {ok,File} = file:open(FileName,[read]),
  Coordinates = read_coordinates(File),
  ok = file:close(File),
  #user
    {name=Name,
     coords=Coordinates,
     filename=FileName,
     complexity=Complexity}.

format_complexity(String) ->
  strip_decimals(String).

strip_decimals([]) ->
  [];
strip_decimals(Str=[Ch|Rest]) ->
  if
    Ch >= $0, Ch =< $9 ->
      read_intpart(Str,[]);
    true ->
      [Ch|strip_decimals(Rest)]
  end.

read_intpart(Str=[Ch|Rest],Coll) ->
  if
    Ch >= $0, Ch =< $9 ->
      [Ch|read_intpart(Rest,[Ch|Coll])];
    Ch == $. ->
      NumDec =        
	if
	  Coll==[0] -> 3;
	  true -> 1
	end,
      [Ch|read_fracpart(Rest,NumDec)];
    true ->
      strip_decimals(Str)
  end.
	   
read_fracpart([],_) -> [];
read_fracpart(Str=[Ch|Rest],N) ->    
  if
    Ch >= $0, Ch =< $9 ->
      Recursive = read_fracpart(Rest,N-1),
      if
	N>0 ->
	  [Ch|Recursive];
	true ->
	  Recursive
      end;
    true ->
      strip_decimals(Str)
  end.

read_coordinates(File) ->
  lists:sort(fun ([X1,_],[X2,_]) -> X1<X2 end, read_coordinates1(File)).
read_coordinates1(File) ->
  case io:fread(File,"","~d ~d") of
    {ok,Coords} -> [Coords|read_coordinates1(File)];
    eof -> []
  end.

format_users(Users,Options) ->
  format_users1(Users,Options).
format_users1([],_) -> "";
format_users1([User],Options) ->
  format_user(User,Options);
format_users1([User|Rest],Options) ->
  format_user(User,Options)++", "++format_users1(Rest,Options).  
format_user(User,Options) ->
  TitleName =
    case proplists:get_value(complexity,Options,false) of
      true when User#user.complexity=/=[] ->
	User#user.name++":"++User#user.complexity;
      _ ->
	User#user.name
    end,
  Title =
    case proplists:get_value(title,Options,true) of
      false -> " title ''";
      true -> " title '"++TitleName++"'"
    end,
  "'"++User#user.filename++"'"++Title.

test() ->
  test(true).

test(IsWorst) ->
  {ok,_} = start(),
  Class =
    if 
      IsWorst ->
	"worst";
      true ->
	"best"
    end,
  AllFiles = filelib:wildcard("data_"++Class++"_*"),
  AllUsers =
    lists:map
      (fun (FileName) ->
	   string:substr(FileName,length("data_"++Class++"_")+1)
       end, AllFiles),
  new_window
    ("data",
     lists:map
       (fun (User) ->
	    read_user
	      (User,
	       IsWorst,
	       [{complexity,true},
		{name,User}])
	end, AllUsers),
     [{screen,{2000,1000}}]),
  new_window("strange"),
  new_window("bad1"),
  new_window("bad2"),
  new_window("bad3"),
  new_window("bad4"),
  grade("data",0),
  grade("strange",-1).
  
  
