%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Push channel for HTTP
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
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
-module(ems_http_push).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../ems.hrl").

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
