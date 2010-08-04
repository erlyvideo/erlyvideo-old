-module(ertp).
-behaviour(application).
-include("log.hrl").


-export([start/2, stop/1, config_change/3]).
-export([start_server/1]).


%%--------------------------------------------------------------------
%% @spec (Type::any(), Args::list()) -> any()
%% @doc Starts RTP library
%% @end
%%--------------------------------------------------------------------
start(_Type, _Args) ->
  ertp_sup:start_link().



%%--------------------------------------------------------------------
%% @spec (Any::any()) -> ok()
%% @doc Stop RTP library
%% @end
%%--------------------------------------------------------------------
stop(_S) ->
  ok.


%%--------------------------------------------------------------------
%% @spec (Any::any(),Any::any(),Any::any()) -> any()
%% @doc Reload RTP config
%% @end
%%--------------------------------------------------------------------
config_change(_Changed, _New, _Remove) ->
  ok.

%%
start_server(Name) ->
  ertp_sup:start_server(Name).
