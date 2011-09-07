%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2011 Max Lapshin
%%% @doc        HTTP API for sending JSON messages to RTMP clients
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% Copyright (c) 2011 Max Lapshin
%%%
%%%---------------------------------------------------------------------------------------
-module(ems_http_notify).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/video_frame.hrl").
-include("../log.hrl").

-export([handle_info/2]).
-export([http/4]).

handle_info({notify_message, Message}, State) ->
  Socket = rtmp_session:get(State, socket),
  rtmp_socket:status(Socket, 0, <<"NetConnection.Message">>, Message),
  {noreply, State};

handle_info(_, _State) ->
  unhandled.

get_pids(Host) ->
  Pids = [Pid || {_, Pid, _, _} <- supervisor:which_children(rtmp_session_sup)],
  lists:filter(fun(Pid) -> rtmp_session:get(Pid, host) == Host end, Pids).

get_info(Pid) ->
  UserId = rtmp_session:get(Pid, user_id),
  Channels = case rtmp_session:get(Pid, session_data) of
    undefined -> [];
    Else -> proplists:get_value(channels, Else, [])
  end,
  {UserId, Channels}.

http(Host, 'GET', ["notify", "list"], Req) ->
  List = [get_info(Pid) || Pid <- get_pids(Host)],
  Info = [[{user_id,UserId},{channels,Channels}] || {UserId, Channels} <- List],
  Req:ok([{'Content-Type', "application/json"}], [mochijson2:encode(Info), "\n"]);

http(Host, 'POST', ["notify"], Req) ->
  Post = Req:get(body),
  {object, Query} = mochijson2:decode(Post),
  UserId = proplists:get_value(user_id, Query),
  {object, Message} = proplists:get_value(message, Query),
  Pids = lists:filter(fun(Pid) ->
    UID = rtmp_session:get(Pid, user_id),
    UID == UserId
  end, get_pids(Host)),
  [Pid ! {notify_message, Message} || Pid <- Pids],
  Req:ok([], "notified\n");

http(_Http, _Method, _Path, _Req) ->
  unhandled.
