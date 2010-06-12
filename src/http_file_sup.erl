-module(http_file_sup).
-author('Max Lapshin <max@maxidoors.ru>').
-version(1.0).

-behaviour(supervisor).

-export([init/1,start_link/0]).

-export([start_request/3]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end 
%%--------------------------------------------------------------------
-spec start_link() -> {'error',_} | {'ok',pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_request(Consumer, URL, Offset) ->
  supervisor:start_child(http_file_request_sup, [Consumer, URL, Offset]).

start_file(URL, Options) ->
  supervisor:start_child(http_file_sup, [URL, Options]).

init([http_file_request]) ->
    {ok,
        {{simple_one_for_one, 3, 10},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {http_file_request,start_link,[]},             % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [http_file_request]                            % Modules  = [Module] | dynamic
              }
            ]
        }
    };

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
  Supervisors = [
    {   http_file_sup,
        {supervisor,start_link,[{local, http_file_sup}, ?MODULE, [http_file]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    },
    {   http_file_request_sup,
        {supervisor,start_link,[{local, http_file_request_sup}, ?MODULE, [http_file_request]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    }
  ],
  
  {ok, {{one_for_one, 3, 10}, Supervisors}}.
