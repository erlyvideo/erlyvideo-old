%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
%%% @doc        Supervisor module
%%% @reference  See <a href="http://erlyvideo.googlecode.com" target="_top">http://erlyvideo.googlecode.com</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
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
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-include("../include/ems.hrl").
-behaviour(supervisor).

-export ([init/1,start_link/0]).
-export ([start_rtmp_client/0, start_rtsp_client/0, start_media/2, 
          start_file_play/2, start_stream_play/2,
          start_ts_lander/1, start_ts_lander/2]).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end 
%%--------------------------------------------------------------------
-spec start_link() -> {'error',_} | {'ok',pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
%% @end 
%%--------------------------------------------------------------------
-spec start_rtmp_client() -> {'error',_} | {'ok',pid()}.
start_rtmp_client() -> supervisor:start_child(rtmp_client_sup, []).

-spec start_rtsp_client() -> {'error',_} | {'ok',pid()}.
start_rtsp_client() -> supervisor:start_child(rtsp_client_sup, []).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for spawning new media entry
%% To be called by the media provider.
%% @end 
%%--------------------------------------------------------------------
start_media(Name, file = Type) -> supervisor:start_child(file_media_sup, [Name, Type]);
start_media(Name, mpeg_ts = Type) -> supervisor:start_child(stream_media_sup, [Name, Type]);
start_media(Name, record = Type) -> supervisor:start_child(stream_media_sup, [Name, Type]);
start_media(Name, live = Type) -> supervisor:start_child(stream_media_sup, [Name, Type]).


start_file_play(MediaEntry, Options) -> supervisor:start_child(file_play_sup, [MediaEntry, Options]).
start_stream_play(MediaEntry, Options) -> supervisor:start_child(file_stream_sup, [MediaEntry, Options]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for spawning new media entry
%% To be called by the media provider.
%% @end 
%%--------------------------------------------------------------------
start_ts_lander(URL) -> supervisor:start_child(ts_lander_sup, [URL]).
start_ts_lander(URL, Consumer) -> supervisor:start_child(ts_lander_sup, [URL, Consumer]).



%%--------------------------------------------------------------------
%% @spec (List::list()) -> any()
%% @doc Initialize application
%% @end 
%%--------------------------------------------------------------------
-spec init([]) -> any().
init([rtmp_client]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {rtmp_client,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([rtsp_client]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {rtsp_client,start_link,[]},                  % StartFun = {M, F, A}
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
init([ts_lander]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {ts_lander,start_link,[]},               % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [ts_lander]                              % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([file_play]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {file_play,start_link,[]},             % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [file_play]                            % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([stream_play]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {stream_play,start_link,[]},             % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [stream_play]                            % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([]) ->
  ets:new(rtmp_sessions, [set, public, named_table]),
  
  Supervisors = [
    % EMS HTTP
    {   ems_http_sup,                         % Id       = internal id
        {ems_http,start_link,[ems:get_var(http_port, 8082)]},             % StartFun = {M, F, A}
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [ems_http]                               % Modules  = [Module] | dynamic
    },
    % EMS instance supervisor
    {   rtmp_client_sup,
        {supervisor,start_link,[{local, rtmp_client_sup}, ?MODULE, [rtmp_client]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    {   rtsp_client_sup,
        {supervisor,start_link,[{local, rtsp_client_sup}, ?MODULE, [rtsp_client]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    {   media_provider_sup,                      % Id       = internal id
        {media_provider,start_link,[]},          % StartFun = {M, F, A}
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [media_provider]                         % Modules  = [Module] | dynamic
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
    {   file_play_sup,
        {supervisor,start_link,[{local, file_play_sup}, ?MODULE, [file_play]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    {   stream_play_sup,
        {supervisor,start_link,[{local, stream_play_sup}, ?MODULE, [stream_play]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    % MPEG TS Lander
    {   ts_lander_sup,
        {supervisor,start_link,[{local, ts_lander_sup}, ?MODULE, [ts_lander]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    }
  ],
  
  Supervisors1 = case ems:get_var(rtmp_port) of
    undefined -> Supervisors;
    RTMPPort -> [% EMS Listener
      {   ems_sup,                                 % Id       = internal id
          {rtmp_server,start_link,[RTMPPort]},              % StartFun = {M, F, A}
          permanent,                               % Restart  = permanent | transient | temporary
          2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
          worker,                                  % Type     = worker | supervisor
          [rtmp_server]                             % Modules  = [Module] | dynamic
      }|Supervisors]
  end,

  Supervisors2 = case ems:get_var(rtsp_port) of
    undefined -> Supervisors1;
    RTSPPort -> [% EMS Listener
      {   rtsp_sup,                                 % Id       = internal id
          {rtsp_server,start_link,[RTSPPort]},              % StartFun = {M, F, A}
          permanent,                               % Restart  = permanent | transient | temporary
          2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
          worker,                                  % Type     = worker | supervisor
          [rtmp_server]                             % Modules  = [Module] | dynamic
      }|Supervisors1]
  end,
  
  
    {ok, {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME}, Supervisors2}}.


%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------



