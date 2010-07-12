-module(ems_http_mpegts).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../../include/ems.hrl").

-export([http/4]).

http(Host, 'GET', ["stream" | Name], Req) ->
  Query = Req:parse_qs(),
  Options1 = [{stream_id,1},{client_buffer,30000}],
  Options2 = case proplists:get_value("start", Query) of
    undefined -> Options1;
    Seek -> [{seek,{before,list_to_integer(Seek)}}|Options1]
  end,
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
  exit(leave);

http(_Host, _Method, _Path, _Req) ->
  unhandled.
