-module(erlyvideo).
-author('Max Lapshin <max@maxidoors.ru>').


-export([start/2, stop/1]).
-export([start/0,stop/0,restart/0,rebuild/0,reload/0]).
-export([load_config/0, reconfigure/0]).
-export([start_modules/0, stop_modules/0]).
-export([call_modules/2]).


start(normal, []) ->
  ems_vhosts:start(),
  {ok, Supervisor} = ems_sup:start_link(),
	{ok, Supervisor}.

stop(_) ->
  %stop().
  ok.

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc Starts Erlyvideo
%% @end 
%%--------------------------------------------------------------------
  
start() -> 
	error_logger:info_report("Starting Erlyvideo ..."),
	application:start(log4erl),
	application:start(crypto),
	application:start(rtmp),
	application:start(rtsp),
	
	application:load(erlyvideo),
	load_config(),
  ems_log:start(),
  media_provider:init_names(),
  
	application:start(erlyvideo),

  start_http(),
  start_rtmp(),
  start_rtsp(),
	start_modules(),
  media_provider:start_static_streams(),
	error_logger:info_report("Started Erlyvideo"),
  
	ok.
  
start_http() ->
  case ems:get_var(http_port, 8082) of
    undefined -> 
      ok;
    HTTP when is_integer(HTTP) -> 
      ems_sup:start_http_server(HTTP)
  end.


start_rtmp() ->
  case ems:get_var(rtmp_port, 1935) of
    undefined -> 
      ok;
    RTMP when is_integer(RTMP) -> 
      rtmp_socket:start_server(RTMP, rtmp_listener1, rtmp_session)
  end.


start_rtsp() ->
  case ems:get_var(rtsp_port, undefined) of
    undefined -> 
      ok;
    RTSP when is_integer(RTSP) -> 
      rtsp:start_server(RTSP, rtsp_listener1, ems_rtsp)
  end.



%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc Stops Erlyvideo
%% @end 
%%--------------------------------------------------------------------
stop() ->
	io:format("Stopping Erlyvideo ...~n"),
  ems_vhosts:stop(),
	stop_modules(),
	ems_script:stop(),
  ems_log:stop(),
	application:stop(erlyvideo),
	application:unload(erlyvideo),
	application:stop(rtsp),
	application:unload(rtsp),
	application:stop(rtmp),
	application:unload(rtmp),
	ok.

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc Stops, Compiles , Reloads and starts Erlyvideo
%% @end 
%%--------------------------------------------------------------------
restart() ->
	stop(),
	rebuild(),
	reload(),
	start().


reconfigure() ->
  RTMP = ems:get_var(rtmp_port, undefined),
  RTSP = ems:get_var(rtsp_port, undefined),
  HTTP = ems:get_var(http_port, undefined),
  ems_vhosts:stop(),
  load_config(),
  ems_vhosts:start(),
  ems_log:stop(),
  ems_log:start(),
  % ems_http:stop(),
  case {RTMP, ems:get_var(rtmp_port, undefined)} of
    {undefined, undefined} -> ok;
    {RTMP, RTMP} -> ok;
    {undefined, _} -> 
      {ok, _} = start_rtmp();
    _ -> 
      supervisor:terminate_child(rtmp_sup, rtmp_listener1),
      supervisor:delete_child(rtmp_sup, rtmp_listener1),
      {ok, _} = start_rtmp()
  end,
  case {HTTP, ems:get_var(http_port, undefined)} of
    {undefined, _} -> ok;
    {HTTP, HTTP} -> ok;
    _ -> 
      ems_http:stop()
  end,
  case {RTSP, ems:get_var(rtsp_port, undefined)} of
    {undefined, undefined} -> ok;
    {RTSP, RTSP} -> ok;
    {undefined, _} -> {ok, _} = start_rtsp();
    _ -> 
      supervisor:terminate_child(rtsp_sup, rtsp_listener1),
      supervisor:delete_child(rtsp_sup, rtsp_listener1),
      {ok, _} = start_rtsp()
  end,
  ok.

load_config() ->
  % [application:unset_env(erlyvideo, Key) || {Key, _} <- application:get_all_env(erlyvideo)],

  case file:path_consult(["priv", "/etc/erlyvideo"], "erlyvideo.conf") of
    {ok, Env, Path} -> 
      error_logger:info_report("Erlyvideo is loading config from file ~s~n", [Path]),
      [application:set_env(erlyvideo, Key, Value) || {Key, Value} <- Env],
      ok;
    {error, enoent} ->
      error_logger:error_msg("No erlyvideo.conf found");
    {error, Reason} ->
      error_logger:error_msg("Couldn't load erlyvideo.conf: ~p~n", [Reason]),
      ok
  end.



%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc Compiles Erlyvideo
%% @end 
%%--------------------------------------------------------------------
rebuild() ->
	io:format("Recompiling EMS Modules ...~n"),
	make:all([load]).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc Compiles and reloads Erlyvideo modules
%% @end
%%--------------------------------------------------------------------
reload() ->
	application:load(erlyvideo),
	case application:get_key(erlyvideo,modules) of
		undefined    -> 
			application:load(erlyvideo),
			reload();
		{ok,Modules} -> 
			io:format("Reloading EMS Modules ...~n"),
			reload(lists:usort(Modules))
	end.

reload(Module) when is_atom(Module) ->
	code:purge(Module),
	code:delete(Module),
	code:load_file(Module),
	true;
reload([]) -> ok;
reload([?MODULE | T]) -> reload(T);
reload([H|T]) ->
	reload(H),
	reload(H),
	reload(T).


%%--------------------------------------------------------------------
%% @spec () -> ok | {error, Reason}
%% @doc Initializes all modules
%% @end 
%%--------------------------------------------------------------------
start_modules() -> call_modules(start, []).

call_modules(Function, Args) -> call_modules(Function, Args, ems:get_var(modules, [])).

call_modules(_, _, []) -> 
  ok;
call_modules(Function, Args, [Module|Modules]) ->
  case ems:respond_to(Module, Function, length(Args)) of
    true -> 
      ok = erlang:apply(Module, Function, Args);
    _ ->
      ok
  end,
  call_modules(Function, Args, Modules).


%%--------------------------------------------------------------------
%% @spec () -> ok | {error, Reason}
%% @doc Shutdown all modules
%% @end 
%%--------------------------------------------------------------------
stop_modules() -> call_modules(stop, []).







