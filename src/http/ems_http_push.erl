-module(ems_http_push).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../../include/ems.hrl").

-export([http/4]).


http(Host, 'POST', ["channels", ChannelS, "message"], Req) ->
  Message = proplists:get_value("message", Req:parse_post()),
  Channel = list_to_integer(ChannelS),
  ems_users:send_to_channel(Host, Channel, list_to_binary(Message)),
  Req:respond(200, [{"Content-Type", "text/plain"}], "200 OK\n");

http(Host, 'POST', ["users", UserS, "message"], Req) ->
  Message = proplists:get_value("message", Req:parse_post()),
  User = list_to_integer(UserS),
  ems_users:send_to_user(Host, User, list_to_binary(Message)),
  Req:respond(200, [{"Content-Type", "text/plain"}], "200 OK\n");

http(_Host, _Method, _Path, _Req) ->
  unhandled.
