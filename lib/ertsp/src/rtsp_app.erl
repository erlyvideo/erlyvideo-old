%% @private
-module(rtsp_app).
-author(max@maxidoors.ru).
-behaviour(application).

-export([start/2, stop/1, config_change/3]).



%%--------------------------------------------------------------------
%% @spec (Type::any(), Args::list()) -> any()
%% @doc Starts RTSP library
%% @end 
%%--------------------------------------------------------------------

start(_Type, _Args) -> 
  rtsp_sup:start_link().
  


%%--------------------------------------------------------------------
%% @spec (Any::any()) -> ok()
%% @doc Stop RTSP library
%% @end 
%%--------------------------------------------------------------------
stop(_S) ->
  ok.


%%--------------------------------------------------------------------
%% @spec (Any::any(),Any::any(),Any::any()) -> any()
%% @doc Reload RTSP config
%% @end 
%%--------------------------------------------------------------------
config_change(_Changed, _New, _Remove) ->
  ok.
