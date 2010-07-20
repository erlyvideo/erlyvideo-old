%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        serve flv streams by http
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
-module(ems_http_flv).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../../include/ems.hrl").

-export([http/4]).



http(Host, 'GET', ["flv" | Name], Req) ->
  Query = Req:parse_qs(),
  Seek = list_to_integer(proplists:get_value("start", Query, "0")),
  Req:stream(head, [{"Content-Type", "video/x-flv"}, {"Connection", "close"}]),
  case media_provider:play(Host, string:join(Name, "/"), [{stream_id, 1}, {seek, {before, Seek}}]) of
    {ok, PlayerPid} ->
      link(PlayerPid),
      link(Req:socket_pid()),
      case proplists:get_value("session_id", Query) of
        undefined -> ok;
        SessionId -> ems_flv_streams:register({Host,SessionId}, PlayerPid)
      end,
      PlayerPid ! start,
      flv_writer:init([fun(Data) -> Req:stream(Data) end]),
      PlayerPid ! stop,
      Req:stream(close),
      ok;
    {notfound, Reason} ->
      Req:stream(io_lib:format("404 Page not found.\n ~p: ~s ~s\n", [Name, Host, Reason])),
      Req:stream(close);
    Reason -> 
      Req:stream(io_lib:format("500 Internal Server Error.~n Failed to start video player: ~p~n ~p: ~p", [Reason, Name, Req])),
      Req:stream(close)
  end;

http(Host, 'GET', ["flvcontrol", SessionId, "pause"], Req) ->
  case ems_flv_streams:command({Host,SessionId}, pause) of
    undefined -> Req:respond(404, [{'Content-Type', "text/plain"}], "404 Not Found");
    _ -> Req:ok([{'Content-Type', "text/plain"}], "ok")
  end;

http(Host, 'GET', ["flvcontrol", SessionId, "resume"], Req) ->
  case ems_flv_streams:command({Host,SessionId}, resume) of
    undefined -> Req:respond(404, [{'Content-Type', "text/plain"}], "404 Not Found");
    _ -> Req:ok([{'Content-Type', "text/plain"}], "ok")
  end;

http(Host, 'GET', ["flvcontrol", SessionId, "seek", Timestamp], Req) ->
  case ems_flv_streams:command({Host,SessionId}, {seek, before, list_to_integer(Timestamp)}) of
    undefined -> Req:respond(404, [{'Content-Type', "text/plain"}], "404 Not Found");
    _ -> Req:ok([{'Content-Type', "text/plain"}], "ok")
  end;

http(_Host, _Method, _Path, _Req) ->
  unhandled.
