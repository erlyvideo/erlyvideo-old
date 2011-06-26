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
-include("log.hrl").
-behaviour(supervisor).


-define(MAX_RESTART,      5).
-define(MAX_TIME,        10).

-define(NAMED_SERVER(Id,M,A), {Id,                               % Id       = internal id
    {M,start_link,A},                  % StartFun = {M, F, A}
    temporary,                               % Restart  = permanent | transient | temporary
    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                  % Type     = worker | supervisor
    [M]                                      % Modules  = [Module] | dynamic
}).
-define(STATIC_SERVER(Id,M,A), {Id,                               % Id       = internal id
    {M,start_link,A},                  % StartFun = {M, F, A}
    permanent,                               % Restart  = permanent | transient | temporary
    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                  % Type     = worker | supervisor
    [M]                                      % Modules  = [Module] | dynamic
}).
-define(SIMPLE_SERVER(M,A), ?NAMED_SERVER(undefined, M, A)).

-define(SUPERVISOR_LINK(Name), {Name,
    {supervisor,start_link,[{local, Name}, ?MODULE, [Name]]},
    permanent,                               % Restart  = permanent | transient | temporary
    infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
    supervisor,                              % Type     = worker | supervisor
    []                                       % Modules  = [Module] | dynamic
}).


-export([init/1,start_link/0]).
-export([start_rtmp_session/1, start_media/3, start_shared_object/3,
          start_shoutcast_reader/1,
          start_deskshare_capture/2,
          start_http_server/1, start_ticker/3, start_mjpeg_reader/2]).

-export([static_streams/0,start_static_streams/0]).

-spec(start_link() -> {error,_} | {ok,pid()}).
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec start_rtmp_session(RTMPSocket::pid()) -> {'error',_} | {'ok',pid()}.
start_rtmp_session(RTMPSocket) ->
  {ok, Pid} = supervisor:start_child(rtmp_session_sup, []),
  rtmp_session:set_socket(Pid, RTMPSocket),
  {ok, Pid}.

start_shoutcast_reader(Consumer) ->
  supervisor:start_child(shoutcast_reader_sup, [Consumer]).

start_mjpeg_reader(URL, Consumer) ->
  supervisor:start_child(mjpeg_reader_sup, [URL, Consumer]).
  
start_deskshare_capture(Name, Options) ->
  supervisor:start_child(deskshare_capture_sup, [Name, Options]).

start_media(_Name, file,          Opts) -> supervisor:start_child(ems_media_sup, [file_media, Opts]);
start_media(_Name, mpegts,        Opts) -> supervisor:start_child(ems_media_sup, [mpegts_media, Opts]);
start_media(_Name, shoutcast,     Opts) -> supervisor:start_child(ems_media_sup, [mpegts_media, Opts]);
start_media(_Name, mpegts_passive,Opts) -> supervisor:start_child(ems_media_sup, [mpegts_passive_media, Opts]);
start_media(_Name, mpegts_file,   Opts) -> supervisor:start_child(ems_media_sup, [mpegts_file_media, Opts]);
start_media(_Name, record,        Opts) -> supervisor:start_child(ems_media_sup, [live_media, Opts]);
start_media(_Name, append,        Opts) -> supervisor:start_child(ems_media_sup, [live_media, Opts]);
start_media(_Name, live,          Opts) -> supervisor:start_child(ems_media_sup, [live_media, Opts]);
start_media(_Name, proxy,         Opts) -> supervisor:start_child(ems_media_sup, [proxy_media, Opts]);
start_media(_Name, rtsp,          Opts) -> supervisor:start_child(ems_media_sup, [rtsp_media, Opts]);
start_media(_Name, rtmp,          Opts) -> supervisor:start_child(ems_media_sup, [rtmp_media, Opts]);
start_media(_Name, http_flv,      Opts) -> supervisor:start_child(ems_media_sup, [http_flv_media, Opts]);
start_media( Name, http,          Opts) -> http_media:start_link(Name, Opts);
start_media(Module, custom,       Opts) -> supervisor:start_child(ems_media_sup, [Module, Opts]);
start_media(_Name, Module,        Opts) -> supervisor:start_child(ems_media_sup, [Module, Opts]).



start_ticker(Media, Consumer, Options) -> supervisor:start_child(media_ticker_sup, [Media, Consumer, Options]).

start_shared_object(Host, Name, Persistent) -> supervisor:start_child(shared_object_sup, [Host, Name, Persistent]).


start_http_server(Port) ->
  % EMS HTTP
  Listener = {Port,                         % Id       = internal id
      {ems_http,start_listener,[Port]},             % StartFun = {M, F, A}
      permanent,                               % Restart  = permanent | transient | temporary
      2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
      worker,                                  % Type     = worker | supervisor
      [ems_http]                               % Modules  = [Module] | dynamic
  },
  supervisor:start_child(ems_http_sup, Listener).



%%%%%%%%  Lagging network clients monitor  %%%%%%%
init([ems_network_lag_monitor_sup]) ->
  {ok, {{one_for_one, 1000, 20}, [?STATIC_SERVER(ems_network_lag, ems_network_lag_monitor, [[{timeout,1000},{threshold,80*60}]])]}};


%%%%%%%%  Media streams  %%%%%%%%
init([erlyvideo_media_sup]) ->
  {ok,
    {{one_for_one, 1000, 20},[
      ?SUPERVISOR_LINK(ems_media_sup),
      ?SUPERVISOR_LINK(media_ticker_sup),
      ?SUPERVISOR_LINK(shoutcast_reader_sup),
      ?SUPERVISOR_LINK(mjpeg_reader_sup),
      ?SUPERVISOR_LINK(deskshare_sup)
    ]}
  };


init([ems_media_sup]) ->
  {ok, {{simple_one_for_one, ?MAX_RESTART, ?MAX_TIME}, [?SIMPLE_SERVER(ems_media, [])]}};


init([media_ticker_sup]) ->
  {ok, {{simple_one_for_one, ?MAX_RESTART, ?MAX_TIME}, [?SIMPLE_SERVER(media_ticker, [])]}};


init([shoutcast_reader_sup]) ->
  {ok, {{simple_one_for_one, ?MAX_RESTART, ?MAX_TIME}, [?SIMPLE_SERVER(shoutcast_reader, [])]}};

init([mjpeg_reader_sup]) ->
  {ok, {{simple_one_for_one, ?MAX_RESTART, ?MAX_TIME}, [?SIMPLE_SERVER(mjpeg_reader, [])]}};

init([deskshare_sup]) ->
  {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME}, [
    ?SUPERVISOR_LINK(deskshare_capture_sup),
    ?STATIC_SERVER(deskshare_tracker_sup, deskshare_tracker, [])
  ]}};

init([deskshare_capture_sup]) ->
  {ok, {{simple_one_for_one, ?MAX_RESTART, ?MAX_TIME}, [?SIMPLE_SERVER(deskshare, [])]}};


%%%%%%%%  User sessions  %%%%%%%%

init([ems_user_sessions_sup]) ->
  {ok, {{one_for_one, 1000, 20},[
    ?SUPERVISOR_LINK(rtmp_session_sup)
  ]}};


  
init([rtmp_session_sup]) ->
  {ok, {{simple_one_for_one, ?MAX_RESTART, ?MAX_TIME}, [?SIMPLE_SERVER(rtmp_session, [])]}};

  

%%%%%%%%  HTTP Listener  %%%%%%%%

  
init([ems_http_sup]) ->
  {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME}, []}};

    
%%%%%%%%  Shared objects  %%%%%%%%


init([ems_so_sup]) ->
  {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME}, [
    ?NAMED_SERVER(shared_objects_sup, shared_objects, []),
    ?SUPERVISOR_LINK(shared_object_sup)
  ]}};
    
    
init([shared_object_sup]) ->
  {ok, {{simple_one_for_one, ?MAX_RESTART, ?MAX_TIME}, [?SIMPLE_SERVER(shared_object, [])]}};



%%%%%%%%  Main supervisor  %%%%%%%%


init([]) ->

  Supervisors = [
    ?SUPERVISOR_LINK(ems_network_lag_monitor_sup),
    ?STATIC_SERVER(media_provider_sup, media_provider, []),
    ?SUPERVISOR_LINK(erlyvideo_media_sup),
    ?SUPERVISOR_LINK(ems_user_sessions_sup),
    ?SUPERVISOR_LINK(ems_http_sup),
    ?STATIC_SERVER(ems_event_sup, ems_event, []),
    ?SUPERVISOR_LINK(ems_so_sup)
  ],

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

