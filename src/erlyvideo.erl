%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        main erlyvideo module
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(erlyvideo).
-author('Max Lapshin <max@maxidoors.ru>').


-export([start/2, stop/1]).
-export([start/0,stop/0,restart/0,rebuild/0,reload/0]).
-export([load_config/0, reconfigure/0]).
-export([start_modules/0, stop_modules/0]).
-export([call_modules/2]).


-export([edoc/0, edoc/1]).


start(normal, []) ->
  ems_vhosts:start(),
  {ok, Supervisor} = ems_sup:start_link(),
	{ok, Supervisor}.

stop(_) ->
  %stop().
  ok.

edoc() ->
  edoc([{dir,"doc/html"}]).
  
edoc(Options) ->
  edoc:application(?MODULE,".",[{packages,false} | Options]).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc Starts Erlyvideo
%% @end 
%%--------------------------------------------------------------------
  
start() -> 
	error_logger:info_report("Starting Erlyvideo ..."),
  ems_log:start(),
	application:start(crypto),
	application:start(os_mon),
	application:start(snmp),
  % os_mon_mib:load(snmp_master_agent),
	snmpa:load_mibs(snmp_master_agent, ["snmp/ERLYVIDEO-MIB"]),
	application:start(rtmp),
	application:start(rtsp),
	
	application:load(erlyvideo),
	load_config(),
	[code:add_pathz(Path) || Path <- ems:get_var(paths, [])],
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
	application:stop(erlyvideo),
	application:unload(erlyvideo),
	application:stop(rtsp),
	application:unload(rtsp),
	application:stop(rtmp),
	application:unload(rtmp),
  ems_log:stop(),
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
  
  File = load_file_config(),
  % Dets = load_persistent_config(),
  % Env = deep_merge(File, Dets),
  Env = File,
  [application:set_env(erlyvideo, Key, Value) || {Key, Value} <- Env],
  ok.


load_file_config() ->
  case file:path_consult(["priv", "/etc/erlyvideo"], "erlyvideo.conf") of
    {ok, Env, Path} -> 
      error_logger:info_report("Erlyvideo is loading config from file ~s~n", [Path]),
      Env;
    {error, enoent} ->
      error_logger:error_msg("No erlyvideo.conf found"),
      [];
    {error, Reason} ->
      error_logger:error_msg("Couldn't load erlyvideo.conf: ~p~n", [Reason]),
      []
  end.


% load_persistent_config() ->
%   load_persistent_config(["priv", "/var/lib/erlyvideo"]).
%   
% load_persistent_config([]) ->
%   [];
%   
% load_persistent_config([Path|Paths]) ->
%   case file:read_file_info(Path) of
%     {ok, _FileInfo} ->
%       case dets:is_dets_file(Path) of
%         true ->
%           {ok, Config} = dets:open_file("erlyvideo_config", [{file, Path}]),
%           [Entry || [Entry] <- dets:match(Config, '$1')];
%         false ->
%           []
%       end;
%     {error, _} ->
%       load_persistent_config(Paths)
%   end.
% 
% deep_merge(List1, List2) ->
%   deep_merge(lists:ukeysort(1, List1), lists:ukeysort(1, List2), []).
% 
% % TODO: implement real deep merge for erlyvideo  
% deep_merge(List1, _, _) ->
%   List1.

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
	code:soft_purge(Module),
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







