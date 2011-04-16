%%% @private
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTSP supervisor module
%%% @end
%%% @reference  See <a href="http://erlyvideo.org/rtsp" target="_top">http://erlyvideo.org</a> for common information.
%%% @end
%%%
%%% This file is part of erlang-rtsp.
%%%
%%% erlang-rtsp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtsp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtsp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtsp_sup).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-behaviour(supervisor).

-export ([init/1,start_link/0]).
-export([start_rtsp_listener/3, start_rtsp_socket/1]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {error,_} | {ok,pid()}).
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_rtsp_socket(Callback) ->
  supervisor:start_child(rtsp_socket_sup, [Callback]).


%% {ok, Pid} = rtsp_sup:start_rtsp_media("rtsp://212.90.177.134:41554/axis-media/media.amp", rtsp, [{host,default},{name,<<"camera">>}]).

start_rtsp_listener(Port, Name, Callback) ->
  Listener = {Name,
  {rtsp_listener, start_link ,[Port, Name, Callback]},
  permanent,
  10000,
  worker,
  [rtsp_listener]},
  supervisor:start_child(?MODULE, Listener).

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
    {rtsp_socket_sup,
      {supervisor,start_link,[{local, rtsp_socket_sup}, ?MODULE, [rtsp_socket]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    }
  ],

  {ok, {{one_for_one, 3, 10}, Supervisors}}.
