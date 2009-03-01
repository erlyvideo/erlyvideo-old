-module(log4erl_sup).

-author("Ahmed Al-Issaei").
-license("MPL-1.1").

-behaviour(supervisor).

-include("../include/log4erl.hrl").

%% API
-export([start_link/1]).
-export([add_logger/1]).
-export([add_guard/4]).

%% Supervisor callbacks
-export([init/1]).

start_link(Default_logger) ->
    R = supervisor:start_link({local, ?MODULE}, ?MODULE, [Default_logger]),
    %log4erl:start_link(Default_logger),
    add_logger(Default_logger),
    ?LOG2("Result in supervisor is ~p~n",[R]),
    R.

add_guard(Logger, Appender, Name, Conf) ->
    C = {Name,
	 {logger_guard, start_link ,[Logger, Appender, Name, Conf]},
	 permanent,
	 10000,
	 worker,
	 [logger_guard]},
    ?LOG2("Adding ~p to ~p~n",[C, ?MODULE]),
    supervisor:start_child(?MODULE, C).
    
add_logger(Name) when is_atom(Name) ->
    N = atom_to_list(Name),
    add_logger(N);
add_logger(Name) when is_list(Name) ->
    C1 = {Name,
	  {log_manager, start_link ,[Name]},
	  permanent,
	  10000,
	  worker,
	  [log_manager]},
    
    ?LOG2("Adding ~p to ~p~n",[C1, ?MODULE]),
    supervisor:start_child(?MODULE, C1).
    %add_guard(N2).

%%======================================
%% supervisor callback functions
%%======================================
init([Default_logger]) ->
    ?LOG("Starting supervisor~n"),
    %% No children to be added yet.
    %% The default has to be added from log4erl
    
    % start log4erl gen_server
    _Child =  {log4erl_p,
	  {log4erl, start_link ,[Default_logger]},
	  permanent,
	  10000,
	  worker,
	  [log4erl]},
    
    {ok,
     {
       {one_for_one,3,10}, 
       [_Child]
       %[]
      }
    }.


