%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon, 2009 Max Lapshin
%%% @doc        Supervisor module
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Luke Hubbard, Stuart Jackson, Roberto Saccon, 2009 Max Lapshin
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
-author('max@maxidoors.ru').
-include("../include/ems.hrl").
-behaviour(supervisor).

-export ([init/1,start_link/0]).
-export ([start_rtmp_session/0, start_rtsp_session/0, start_media/3, 
          start_file_play/2, start_stream_play/2,
          start_mpegts_media/1, start_shared_object/3]).


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
-spec start_rtmp_session() -> {'error',_} | {'ok',pid()}.
start_rtmp_session() -> supervisor:start_child(rtmp_session_sup, []).

-spec start_rtsp_session() -> {'error',_} | {'ok',pid()}.
start_rtsp_session() -> supervisor:start_child(rtsp_session_sup, []).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for spawning new media entry
%% To be called by the media provider.
%% @end 
%%--------------------------------------------------------------------
start_media(Name, file = Type, Opts) -> supervisor:start_child(file_media_sup, [Name, Type, Opts]);
start_media(Name, mpeg_ts = Type, Opts) -> supervisor:start_child(mpegts_media_sup, [Name, Type, Opts]);
start_media(Name, mpeg_ts_passive = Type, Opts) -> supervisor:start_child(mpegts_media_sup, [Name, Type, Opts]);
start_media(Name, record = Type, Opts) -> supervisor:start_child(stream_media_sup, [Name, Type, Opts]);
start_media(Name, live = Type, Opts) -> supervisor:start_child(stream_media_sup, [Name, Type, Opts]).


start_file_play(MediaEntry, Options) -> supervisor:start_child(file_play_sup, [MediaEntry, Options]).
start_stream_play(MediaEntry, Options) -> supervisor:start_child(stream_play_sup, [MediaEntry, Options]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for spawning new media entry
%% To be called by the media provider.
%% @end 
%%--------------------------------------------------------------------
start_mpegts_media(URL) -> supervisor:start_child(mpegts_media_sup, [URL]).

start_shared_object(Host, Name, Persistent) -> supervisor:start_child(shared_object_sup, [Host, Name, Persistent]).


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
init([mpegts_media]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {mpegts_media,start_link,[]},               % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [mpegts_media]                              % Modules  = [Module] | dynamic
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
    {   binary_to_atom(<<"media_provider_sup_", (atom_to_binary(Host, latin1))/binary>>, latin1), % Id       = internal id
        {media_provider,start_link,[Host]},      % StartFun = {M, F, A}
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [media_provider]                         % Modules  = [Module] | dynamic
    }
  end, ems:get_var(vhosts, [])),
  
  
  
  
  Supervisors = [
    {   rtmp_session_sup,
        {supervisor,start_link,[{local, rtmp_session_sup}, ?MODULE, [rtmp_session]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    % EMS HTTP
    {   ems_http_sup,                         % Id       = internal id
        {ems_http,start_link,[ems:get_var(http_port, 8082)]},             % StartFun = {M, F, A}
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [ems_http]                               % Modules  = [Module] | dynamic
    },
    {   ems_users_sup,                         % Id       = internal id
        {ems_users,start_link,[]},             % StartFun = {M, F, A}
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [ems_users]                               % Modules  = [Module] | dynamic
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
    {   mpegts_media_sup,
        {supervisor,start_link,[{local, mpegts_media_sup}, ?MODULE, [mpegts_media]]},
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
  | MediaProviders],
  
  
  
  
  {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME}, Supervisors}}.


%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------



