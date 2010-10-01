%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @private
%%% @doc        Supervisor module
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
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
-module(ems_sup).
-author('Max Lapshin <max@maxidoors.ru>').
-include("ems.hrl").
-behaviour(supervisor).

-export([init/1,start_link/0]).
-export([start_rtmp_session/1, start_media/3, start_shared_object/3,
          start_shoutcast_reader/1,
          start_http_server/1, start_http_worker/1, start_ticker/3, start_mjpeg_reader/2]).

-export([static_streams/0,start_static_streams/0]).

-spec(start_link() -> {error,_} | {ok,pid()}).
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec start_rtmp_session(RTMPSocket::pid()) -> {'error',_} | {'ok',pid()}.
start_rtmp_session(RTMPSocket) ->
  {ok, Pid} = supervisor:start_child(rtmp_session_sup, []),
  rtmp_session:set_socket(Pid, RTMPSocket),
  {ok, Pid}.

start_http_worker(ClientSocket) ->
  supervisor:start_child(ems_http_worker_sup, [ClientSocket]).

start_shoutcast_reader(Consumer) ->
  supervisor:start_child(shoutcast_reader_sup, [Consumer]).

start_mjpeg_reader(URL, Consumer) ->
  supervisor:start_child(mjpeg_reader_sup, [URL, Consumer]).

start_media(_Name, file,          Opts) -> supervisor:start_child(ems_media_sup, [file_media, Opts]);
start_media(_Name, mpegts,        Opts) -> supervisor:start_child(ems_media_sup, [mpegts_media, Opts]);
start_media(_Name, shoutcast,     Opts) -> supervisor:start_child(ems_media_sup, [mpegts_media, Opts]);
start_media(_Name, mpegts_passive,Opts) -> supervisor:start_child(ems_media_sup, [mpegts_media, Opts]);
start_media(_Name, mpegts_file,   Opts) -> supervisor:start_child(ems_media_sup, [mpegts_file_media, Opts]);
start_media(_Name, record,        Opts) -> supervisor:start_child(ems_media_sup, [live_media, Opts]);
start_media(_Name, live,          Opts) -> supervisor:start_child(ems_media_sup, [live_media, Opts]);
start_media(_Name, rtsp,          Opts) -> supervisor:start_child(ems_media_sup, [rtsp_media, Opts]);
start_media(_Name, rtmp,          Opts) -> supervisor:start_child(ems_media_sup, [rtmp_media, Opts]);
start_media( Name, http,          Opts) -> http_media:start_link(Name, Opts);
start_media(Module, custom,       Opts) -> supervisor:start_child(ems_media_sup, [Module, Opts]);
start_media(_Name, Module,        Opts) -> supervisor:start_child(ems_media_sup, [Module, Opts]).



start_ticker(Media, Consumer, Options) -> supervisor:start_child(media_ticker_sup, [Media, Consumer, Options]).

start_shared_object(Host, Name, Persistent) -> supervisor:start_child(shared_object_sup, [Host, Name, Persistent]).


start_http_server(Port) ->
  % EMS HTTP
  Listener = {ems_http_sup,                         % Id       = internal id
      {ems_http,start_listener,[Port]},             % StartFun = {M, F, A}
      permanent,                               % Restart  = permanent | transient | temporary
      2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
      worker,                                  % Type     = worker | supervisor
      [ems_http]                               % Modules  = [Module] | dynamic
  },
  supervisor:start_child(?MODULE, Listener).


-spec(init([]) -> any()).
init([rtmp_session]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {rtmp_session,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([ems_http_worker]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {ems_http,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([shoutcast_reader]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {shoutcast_reader,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([mjpeg_reader]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {mjpeg_reader,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [mjpeg_reader]                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([ems_media]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {ems_media,start_link,[]},             % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [ems_media]                            % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([media_ticker]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {media_ticker,start_link,[]},             % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [media_ticker]                            % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([shoutcast_media]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {shoutcast_media,start_link,[]},               % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [shoutcast_media]                              % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([shared_object]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {shared_object,start_link,[]},             % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [shared_object]                            % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([]) ->


  MediaProviders = lists:map(fun({Host, _}) ->
    {   media_provider:name(Host), % Id       = internal id
        {media_provider,start_link,[Host]},      % StartFun = {M, F, A}
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [media_provider]                         % Modules  = [Module] | dynamic
    }
  end, ems:get_var(vhosts, [])),




  Supervisors1 = [
    {ems_network_lag_monitor_sup,                         % Id       = internal id
        {ems_license_client,start_link,[]},            % StartFun = {M, F, A}
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [ems_license_client]                % Modules  = [Module] | dynamic
    },
    {ems_license_client_sup,                         % Id       = internal id
        {ems_network_lag_monitor,start_link,[[{timeout,1000},{threshold,80*60}]]},            % StartFun = {M, F, A}
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [ems_network_lag_monitor]                % Modules  = [Module] | dynamic
    },
    {   rtmp_session_sup,
        {supervisor,start_link,[{local, rtmp_session_sup}, ?MODULE, [rtmp_session]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    {   ems_users_sup,                         % Id       = internal id
        {ems_users,start_link,[]},             % StartFun = {M, F, A}
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [ems_users]                               % Modules  = [Module] | dynamic
    },
    {   ems_event_sup,                         % Id       = internal id
        {ems_event,start_link,[]},             % StartFun = {M, F, A}
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [ems_event]                               % Modules  = [Module] | dynamic
    },
    {   ems_flv_streams_sup,                         % Id       = internal id
        {ems_flv_streams,start_link,[]},             % StartFun = {M, F, A}
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [ems_flv_streams]                               % Modules  = [Module] | dynamic
    },
    {   ems_media_sup,
        {supervisor,start_link,[{local, ems_media_sup}, ?MODULE, [ems_media]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    {   media_ticker_sup,
        {supervisor,start_link,[{local, media_ticker_sup}, ?MODULE, [media_ticker]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    {   ems_http_worker_sup,
        {supervisor,start_link,[{local, ems_http_worker_sup}, ?MODULE, [ems_http_worker]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    {   shoutcast_reader_sup,
        {supervisor,start_link,[{local, shoutcast_reader_sup}, ?MODULE, [shoutcast_reader]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    {   mjpeg_reader_sup,
        {supervisor,start_link,[{local, mjpeg_reader_sup}, ?MODULE, [mjpeg_reader]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    {   shared_objects_sup,
        {shared_objects,start_link,[]},          % StartFun = {M, F, A}
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [shared_objects]                                       % Modules  = [Module] | dynamic
    },
    {   shared_object_sup,
        {supervisor,start_link,[{local, shared_object_sup}, ?MODULE, [shared_object]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    }
  ] ++ MediaProviders,

  Supervisors2 = case ems:get_var(scripting, false) of
    true -> [{   ems_script_sup,
        {ems_script,start_link,[]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    } | Supervisors1];
    false -> Supervisors1
  end,

  Supervisors = Supervisors2,
  {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME}, Supervisors}}.


%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------

start_static_streams() ->
  [supervisor:start_child(?MODULE, Sup) || Sup <- static_streams()].

static_streams() ->
  Hosts = proplists:get_keys(ems:get_var(vhosts,[])),
  Nested = lists:map(fun(Host) ->
    lists:map(fun(Stream) ->
      {   media_provider:static_stream_name(Host,Stream), % Id       = internal id
          {media_provider,start_static_stream,[Host,Stream]},      % StartFun = {M, F, A}
          permanent,                               % Restart  = permanent | transient | temporary
          2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
          worker,                                  % Type     = worker | supervisor
          [media_provider]                         % Modules  = [Module] | dynamic
      }
    end, ems:get_var(static, Host, []))
  end, Hosts),
  lists:flatten(Nested).

