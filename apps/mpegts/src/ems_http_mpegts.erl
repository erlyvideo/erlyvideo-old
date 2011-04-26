%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        MPEG-TS by http
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
-module(ems_http_mpegts).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([http/4]).

http(Host, 'GET', ["stream" | Name], Req) ->
  Query = Req:parse_qs(),
  Options1 = [{stream_id,1},{client_buffer,30000}],
  StartOptions = case proplists:get_value("start", Query) of
    undefined -> [];
    Seek -> [{start,list_to_integer(Seek)*1000}]
  end,
  DurationOptions = case proplists:get_value("duration", Query) of
    undefined -> [];
    Dur -> [{duration,list_to_integer(Dur)*1000}]
  end,
  Options2 = Options1 ++ StartOptions ++ DurationOptions,
  case media_provider:play(Host, string:join(Name, "/"), Options2) of
    {ok, Stream} ->
      mpegts_play:play(Name, Stream, Req),
      ok;
    {notfound, Reason} ->
      Req:stream(io_lib:format("404 Page not found.\n ~p: ~s ~s\n", [Name, Host, Reason])),
      Req:stream(close);
    Reason -> 
      Req:stream(io_lib:format("500 Internal Server Error.~n Failed to start video player: ~p~n ~p: ~p", [Reason, Name, Req])),
      Req:stream(close)
  end;

http(Host, 'GET', ["iphone", "playlists" | StreamName] = Path, Req) ->
  ems_log:access(Host, "GET ~p ~s /~s", [Req:get(peer_addr), "-", string:join(Path, "/")]),
  FullName = string:join(StreamName, "/"),
  {ok, Re} = re:compile("^(.+).m3u8$"),
  {match, [_, Name]} = re:run(FullName, Re, [{capture, all, binary}]),

  Playlist = iphone_streams:playlist(Host, Name),

  Req:respond(200, [{"Content-Type", "application/vnd.apple.mpegurl"}], Playlist);


http(Host, 'GET', ["iphone", "segments" | StreamName] = Path, Req) ->
  ems_log:access(Host, "GET ~p ~s /~s", [Req:get(peer_addr), "-", string:join(Path, "/")]),
  
  {Name, [SegmentId]} = lists:split(length(StreamName) - 1, StreamName),

  Segment = list_to_integer(hd(string:tokens(SegmentId, "."))),
  iphone_streams:play(Host, string:join(Name, "/"), Segment, Req);

http(Host, 'PUT', ["stream", Name], Req) ->
  {Module, Function} = ems:check_app(Host, auth, 3),
  _Session = Module:Function(Host, http, proplists:get_value('Authorization', Req:get(headers))),

  ems_log:access(Host, "MPEGTS PUT ~s ~s", [Host, Name]),
  {ok, Stream} = media_provider:open(Host, Name, [{type, mpegts_passive}]),
  ems_media:set_socket(Stream, Req:socket()),
  ok;

http(_Host, _Method, _Path, _Req) ->
  unhandled.
