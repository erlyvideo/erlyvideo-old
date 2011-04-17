%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @private
%%% @doc        MPEG-TS supervisor
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
-module(mpegts_sup).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(supervisor).

-export([start_reader/1, start_file_reader/2]).

-export([init/1, start/0, stop/0, start/2, stop/1, start_link/0]).

-define(MAX_RESTART, 10).
-define(MAX_TIME, 100).


start() ->
  application:start(mpegts),
  ok.

start(_Type, _Args) ->
  mpegts_sup:start_link().

stop() ->
  application:stop(mpegts),
  application:unload(mpegts),
  ok.

stop(_S) ->
  ok.
  
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_reader(Consumer) ->
  supervisor:start_child(mpegts_reader_sup, [Consumer]).

start_file_reader(Path, Options) ->
  supervisor:start_child(mpegts_file_reader_sup, [Path, Options]).

init([mpegts_reader]) ->
  {ok, {{simple_one_for_one, ?MAX_RESTART, ?MAX_TIME}, [
    {   undefined,                               % Id       = internal id
    {mpegts_reader,start_link,[]},                  % StartFun = {M, F, A}
    temporary,                               % Restart  = permanent | transient | temporary
    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                  % Type     = worker | supervisor
    []                                       % Modules  = [Module] | dynamic
    }
  ]}};

init([mpegts_file_reader]) ->
  {ok, {{simple_one_for_one, ?MAX_RESTART, ?MAX_TIME}, [
    {   undefined,                               % Id       = internal id
    {mpegts_file_reader,start_link,[]},                  % StartFun = {M, F, A}
    temporary,                               % Restart  = permanent | transient | temporary
    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                  % Type     = worker | supervisor
    []                                       % Modules  = [Module] | dynamic
    }
  ]}};


init([]) ->
  Supervisors = [
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
  }],
  {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME}, Supervisors}}.
  
  