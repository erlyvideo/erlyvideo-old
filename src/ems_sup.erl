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
-export ([start_client/0, start_media/2, start_ts_lander/1]).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end 
%%--------------------------------------------------------------------
-spec start_link() -> {'error',_} | {'ok',pid()}.
start_link() ->
	ListenPort = ems:get_var(listen_port, ?RTMP_PORT),
	supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort]).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
%% @end 
%%--------------------------------------------------------------------
-spec start_client() -> {'error',_} | {'ok',pid()}.
start_client() -> supervisor:start_child(rtmp_client_sup, []).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for spawning new media entry
%% To be called by the media provider.
%% @end 
%%--------------------------------------------------------------------
start_media(Name, Type) -> supervisor:start_child(media_entry_sup, [Name, Type]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for spawning new media entry
%% To be called by the media provider.
%% @end 
%%--------------------------------------------------------------------
start_ts_lander(URL) -> supervisor:start_child(ts_lander_sup, [URL]).



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
init([media_entry]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {media_entry,start_link,[]},             % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [media_entry]                            % Modules  = [Module] | dynamic
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
init([Port]) when is_integer(Port) ->
  ets:new(rtmp_sessions, [set, public, named_table]),
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [ % EMS Listener
              {   ems_sup,                                 % Id       = internal id
                  {ems_server,start_link,[Port]},          % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [ems_server]                             % Modules  = [Module] | dynamic
              },
              {   media_provider_sup,                      % Id       = internal id
                  {media_provider,start_link,[]},          % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [media_provider]                         % Modules  = [Module] | dynamic
              },
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
              % Media entry supervisor
              {   media_entry_sup,
                  {supervisor,start_link,[{local, media_entry_sup}, ?MODULE, [media_entry]]},
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
            ]
        }
    }.


%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------



