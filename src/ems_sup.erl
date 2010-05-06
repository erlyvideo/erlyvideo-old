%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Supervisor module
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(ems_sup).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/ems.hrl").
-behaviour(supervisor).

-export ([init/1,start_link/0]).
-export ([start_rtmp_session/1, start_rtsp_session/0, start_media/3, start_shared_object/3,
          start_mpegts_reader/1, start_mpegts_file_reader/2, start_shoutcast_reader/1,
          start_http_server/1,
          start_rtmp_stream/1, start_iphone_stream/1, start_mpegts_stream/1]).
      
-export([static_streams/0,start_static_streams/0]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end 
%%--------------------------------------------------------------------
-spec(start_link() -> {error,_} | {ok,pid()}).
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
%% @end 
%%--------------------------------------------------------------------
-spec start_rtmp_session(RTMPSocket::pid()) -> {'error',_} | {'ok',pid()}.
start_rtmp_session(RTMPSocket) -> 
  {ok, Pid} = supervisor:start_child(rtmp_session_sup, []),
  rtmp_session:set_socket(Pid, RTMPSocket),
  {ok, Pid}.

-spec start_rtsp_session() -> {'error',_} | {'ok',pid()}.
start_rtsp_session() -> supervisor:start_child(rtsp_session_sup, []).

start_mpegts_reader(Consumer) ->
  supervisor:start_child(mpegts_reader_sup, [Consumer]).

start_mpegts_file_reader(Path, Options) ->
  supervisor:start_child(mpegts_file_reader_sup, [Path, Options]).

start_shoutcast_reader(Consumer) ->
  supervisor:start_child(shoutcast_reader_sup, [Consumer]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for spawning new media entry
%% To be called by the media provider.
%% @end 
%%--------------------------------------------------------------------
start_media(Name, file           = Type, Opts) -> supervisor:start_child(file_media_sup, [Name, Type, Opts]);
start_media(Name, mpegts         = Type, Opts) -> supervisor:start_child(stream_media_sup, [Name, Type, Opts]);
start_media(Name, mpegts_file    = Type, Opts) -> supervisor:start_child(stream_media_sup, [Name, Type, Opts]);
start_media(Name, mpegts_passive = Type, Opts) -> supervisor:start_child(stream_media_sup, [Name, Type, Opts]);
start_media(Name, record         = Type, Opts) -> supervisor:start_child(stream_media_sup, [Name, Type, Opts]);
start_media(Name, live           = Type, Opts) -> supervisor:start_child(stream_media_sup, [Name, Type, Opts]);
start_media(Name, shoutcast      = Type, Opts) -> supervisor:start_child(stream_media_sup, [Name, Type, Opts]);
start_media(Name, rtsp           = Type, Opts) -> supervisor:start_child(stream_media_sup, [Name, Type, Opts]);
start_media(Name, rtmp           = Type, Opts) -> supervisor:start_child(stream_media_sup, [Name, Type, Opts]);
start_media(Name, http,                  Opts) -> http_media:start_link(Name, Opts).


start_rtmp_stream(Options) -> supervisor:start_child(rtmp_stream_sup, [Options]).
start_iphone_stream(Options) -> supervisor:start_child(iphone_stream_sup, [Options]).
start_mpegts_stream(Options) -> supervisor:start_child(mpegts_stream_sup, [Options]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for spawning new media entry
%% To be called by the media provider.
%% @end 
%%--------------------------------------------------------------------

start_shared_object(Host, Name, Persistent) -> supervisor:start_child(shared_object_sup, [Host, Name, Persistent]).


start_http_server(Port) ->
  % EMS HTTP
  Listener = {ems_http_sup,                         % Id       = internal id
      {ems_http,start_link,[Port]},             % StartFun = {M, F, A}
      permanent,                               % Restart  = permanent | transient | temporary
      2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
      worker,                                  % Type     = worker | supervisor
      [ems_http]                               % Modules  = [Module] | dynamic
  },
  supervisor:start_child(?MODULE, Listener).

%%--------------------------------------------------------------------
%% @spec (List::list()) -> any()
%% @doc Initialize application
%% @end 
%%--------------------------------------------------------------------
-spec init([]) -> any().
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
init([rtsp_session]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {rtsp_session,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([mpegts_reader]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {mpegts_reader,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([mpegts_file_reader]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {mpegts_file_reader,start_link,[]},                  % StartFun = {M, F, A}
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
init([file_media]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {file_media,start_link,[]},             % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [file_media]                            % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([stream_media]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {stream_media,start_link,[]},             % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [stream_media]                            % Modules  = [Module] | dynamic
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
init([rtmp_stream]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {rtmp_stream,start_link,[]},             % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [rtmp_stream]                            % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([iphone_stream]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {iphone_stream,start_link,[]},             % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [iphone_stream]                            % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([mpegts_stream]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {mpegts_stream,start_link,[]},             % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [mpegts_stream]                            % Modules  = [Module] | dynamic
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
    {   iphone_streams_sup,                         % Id       = internal id
        {iphone_streams,start_link,[[]]},             % StartFun = {M, F, A}
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [iphone_streams]                               % Modules  = [Module] | dynamic
    },
    % Media entry supervisor
    {   file_media_sup,
        {supervisor,start_link,[{local, file_media_sup}, ?MODULE, [file_media]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    {   stream_media_sup,
        {supervisor,start_link,[{local, stream_media_sup}, ?MODULE, [stream_media]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    {   mpegts_reader_sup,
        {supervisor,start_link,[{local, mpegts_reader_sup}, ?MODULE, [mpegts_reader]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    {   mpegts_file_reader_sup,
        {supervisor,start_link,[{local, mpegts_file_reader_sup}, ?MODULE, [mpegts_file_reader]]},
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
    {   rtmp_stream_sup,
        {supervisor,start_link,[{local, rtmp_stream_sup}, ?MODULE, [rtmp_stream]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    {   iphone_stream_sup,
        {supervisor,start_link,[{local, iphone_stream_sup}, ?MODULE, [iphone_stream]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    {   mpegts_stream_sup,
        {supervisor,start_link,[{local, mpegts_stream_sup}, ?MODULE, [mpegts_stream]]},
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

