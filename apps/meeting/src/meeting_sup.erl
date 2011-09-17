%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        Meeting subsystem for erlyvideo
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%%---------------------------------------------------------------------------------------
-module(meeting_sup).
-author('Max Lapshin <max@maxidoors.ru>').
-version(1.0).
-include("meeting.hrl").

-behaviour(supervisor).
-behaviour(application).

-export([start/0, start/2, stop/0, stop/1]).
-export([init/1, start_link/0]).

-export([start_meeting/2, start_meeting_saver/2, start_meeting_player/2]).


start() ->
  application:start(meeting).

stop() ->
  application:stop(meeting).
  
start(_, _) ->
  start_link().

stop(_) ->
  ok.



%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {'error',_} | {'ok',pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_meeting(Module, Options) ->
  supervisor:start_child(meeting_sup, [Module, Options]).

start_meeting_saver(Source, Options) ->
  supervisor:start_child(meeting_saver_sup, [Source, Options]).

start_meeting_player(Meeting, Options) ->
  supervisor:start_child(meeting_player_sup, [Meeting, Options]).

init([meeting]) ->
  {ok, {{simple_one_for_one, 5, 60}, [
  { undefined,                               % Id       = internal id
    {meeting,start_link,[]},             % StartFun = {M, F, A}
    temporary,                               % Restart  = permanent | transient | temporary
    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                  % Type     = worker | supervisor
    []                            % Modules  = [Module] | dynamic
  }]}
  };

init([meeting_saver]) ->
  {ok, {{simple_one_for_one, 5, 60}, [
  { undefined,                               % Id       = internal id
    {meeting_saver,start_link,[]},             % StartFun = {M, F, A}
    temporary,                               % Restart  = permanent | transient | temporary
    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                  % Type     = worker | supervisor
    []                            % Modules  = [Module] | dynamic
  }]}
  };

init([meeting_player]) ->
  {ok, {{simple_one_for_one, 5, 60}, [
  { undefined,                               % Id       = internal id
    {meeting_player,start_link,[]},             % StartFun = {M, F, A}
    temporary,                               % Restart  = permanent | transient | temporary
    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                  % Type     = worker | supervisor
    []                            % Modules  = [Module] | dynamic
  }]}
  };

init([]) ->
  Supervisors = [
    { meeting_sup,
      {supervisor,start_link,[{local, meeting_sup}, ?MODULE, [meeting]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    },
    { meeting_saver_sup,
      {supervisor,start_link,[{local, meeting_saver_sup}, ?MODULE, [meeting_saver]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    },
    { meeting_player_sup,
      {supervisor,start_link,[{local, meeting_player_sup}, ?MODULE, [meeting_player]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    }
  ],

  {ok, {{one_for_one, 100, 5}, Supervisors}}.
