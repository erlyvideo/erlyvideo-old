-module(http_file_sup).
-author('Max Lapshin <max@maxidoors.ru>').
-version(1.0).

-behaviour(supervisor).

-export([init/1,start_link/0]).

-export([start_file/2]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end 
%%--------------------------------------------------------------------
-spec start_link() -> {'error',_} | {'ok',pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_file(URL, Options) ->
  supervisor:start_child(http_one_file_sup, [URL, Options]).

init([http_file]) ->
    {ok,
        {{simple_one_for_one, 3, 10},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {http_file,start_link,[]},             % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [http_file]                            % Modules  = [Module] | dynamic
              }
            ]
        }
    };


init([]) ->
  {ok, CachePath} = application:get_env(http_file, cache_path),
  Supervisors = [
    {   http_file_tracker_sup,
        {http_file_tracker,start_link,[CachePath]},
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [http_file_tracker]                      % Modules  = [Module] | dynamic
    },
    {   http_one_file_sup,
        {supervisor,start_link,[{local, http_one_file_sup}, ?MODULE, [http_file]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    }
  ],
  
  {ok, {{one_for_one, 3, 10}, Supervisors}}.
