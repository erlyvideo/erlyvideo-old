-module(rtp_sup).
-include("log.hrl").

-behaviour(supervisor).

-export ([init/1,start_link/0]).
-export([start_server/1]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {error,_} | {ok,pid()}).
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_server(Args) ->
  supervisor:start_child(rtp_server_sup, [Args]).

init([rtp_server]) ->
  {ok,
    {{simple_one_for_one, 5, 60},
      [
        {   undefined,                               % Id       = internal id
          {rtp_server,start_link,[]},             % StartFun = {M, F, A}
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
    {rtp_server_sup,
      {supervisor,start_link,[{local, rtp_server_sup}, ?MODULE, [rtp_server]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    }
  ],

  {ok, {{one_for_one, 3, 10}, Supervisors}}.
