%% @private
-module(rtsp_sup).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-behaviour(supervisor).

-export ([init/1,start_link/0]).
-export([start_rtsp_listener/3, start_rtsp_socket/1, start_rtp_server/2]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end 
%%--------------------------------------------------------------------
-spec(start_link() -> {error,_} | {ok,pid()}).
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_rtsp_socket(Callback) -> supervisor:start_child(rtsp_socket_sup, [Callback]).
start_rtp_server(Media, Stream) -> supervisor:start_child(rtp_server_sup, [Media, Stream]).


%% {ok, Pid} = rtsp_sup:start_rtsp_media("rtsp://212.90.177.134:41554/axis-media/media.amp", rtsp, [{host,default},{name,<<"camera">>}]).

start_rtsp_listener(Port, Name, Callback) ->
  Listener = {Name,
  {rtsp_listener, start_link ,[Port, Name, Callback]},
  permanent,
  10000,
  worker,
  [rtsp_listener]},
  supervisor:start_child(?MODULE, Listener).
  

init([rtp_server]) ->
  {ok,
    {{simple_one_for_one, 5, 60},
      [
        {   undefined,                               % Id       = internal id
          {rtp_server,start_link,[]},             % StartFun = {M, F, A}
          temporary,                               % Restart  = permanent | transient | temporary
          2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
          worker,                                  % Type     = worker | supervisor
          []                            % Modules  = [Module] | dynamic
        }
      ]
    }
  };

init([rtsp_socket]) ->
  {ok,
    {{simple_one_for_one, 5, 60},
      [
        {undefined,                               % Id       = internal id
          {rtsp_socket,start_link,[]},             % StartFun = {M, F, A}
          temporary,                               % Restart  = permanent | transient | temporary
          2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
          worker,                                  % Type     = worker | supervisor
          []                            % Modules  = [Module] | dynamic
        }
      ]
    }
  };


init([]) ->
  Supervisors = [
    {rtp_server_sup,
      {supervisor,start_link,[{local, rtp_server_sup}, ?MODULE, [rtp_server]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    },
    {rtsp_socket_sup,
      {supervisor,start_link,[{local, rtsp_socket_sup}, ?MODULE, [rtsp_socket]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    }
  ],
  
  {ok, {{one_for_one, 3, 10}, Supervisors}}.
