-module(auth_iphone).
-author('Max Lapshin <max@maxidoors.ru>').

-export([http/4, sign/3]).

-define(TIMEOUT, 5*3600).


sign(Timestamp, Name, Host) ->
  Secret = ems:get_var(secret_key, Host, undefined),
  json_session:binary_to_hexbin(crypto:md5(string:join([Timestamp, Name, Secret], "/"))).
  

check_sign(Name, Timestamp, Hash, Host) ->
  {MegaSecs, Secs, _MicroSecs} = erlang:now(),
  case MegaSecs*1000000 + Secs < list_to_integer(Timestamp) of
    true ->
      sign(Timestamp, Name, Host) == list_to_binary(Hash);
    false ->
      false
  end.
  

http(Host, 'GET', ["iphone.html"], Req) ->
  erlydtl:compile(code:lib_dir(auth_iphone, priv) ++ "/iphone.html", iphone_template),
  URL = "video.mp4.m3u8",
  {MegaSecs, Secs, _MicroSecs} = erlang:now(),
  Timestamp = integer_to_list(MegaSecs*1000000 + Secs + ?TIMEOUT),
  Hash = binary_to_list(auth_iphone:sign(Timestamp, URL, Host)),
  SignedURL = string:join([Timestamp, Hash, URL], "/"),
  {ok, Index} = iphone_template:render([
    {url, SignedURL}
  ]),
  Req:ok([{'Content-Type', "text/html; charset=utf8"}], Index);



http(Host, 'GET', ["iphone", "playlists", TimestampS, Hash | StreamName] = Path, Req) ->
  FullName = string:join(StreamName, "/"),
  
  case check_sign(FullName, TimestampS, Hash, Host) of
    true ->
      ems_log:access(Host, "GET ~p ~s /~s", [Req:get(peer_addr), "-", string:join(Path, "/")]),
  
  
      {ok, Re} = re:compile("^(.+).m3u8$"),
      {match, [_, Name]} = re:run(FullName, Re, [{capture, all, binary}]),

      {MegaSecs, Secs, _MicroSecs} = erlang:now(),
      Timestamp = integer_to_list(MegaSecs*1000000 + Secs + ?TIMEOUT),
      Hash1 = binary_to_list(auth_iphone:sign(Timestamp, binary_to_list(Name), Host)),

      Playlist = iphone_streams:playlist(Host, Name, [{generator, fun(Duration, SName, Number) ->
        
        io_lib:format("#EXTINF:~p,~n/iphone/segments/~s/~s/~s/~p.ts~n", [Duration, Timestamp, Hash1, SName, Number])
      end}]),

      Req:respond(200, [{"Content-Type", "application/vnd.apple.mpegurl"}], Playlist);
    false ->
      ems_log:access(Host, "FAIL ~p ~s /~s", [Req:get(peer_addr), "-", string:join(Path, "/")]),
      Req:respond(403, [{"Content-Type", "text/plain"}], "secure link expired")
  end;
      


http(Host, 'GET', ["iphone", "segments", TimestampS, Hash | StreamName] = Path, Req) ->
  {Name, [SegmentId]} = lists:split(length(StreamName) - 1, StreamName),
  FullName = string:join(Name, "/"),
  case check_sign(FullName, TimestampS, Hash, Host) of
    true ->
      ems_log:access(Host, "GET ~p ~s /~s", [Req:get(peer_addr), "-", string:join(Path, "/")]),
  
      Segment = list_to_integer(hd(string:tokens(SegmentId, "."))),
      iphone_streams:play(Host, FullName, Segment, Req);
    false ->
      ems_log:access(Host, "FAIL ~p ~s /~s", [Req:get(peer_addr), "-", string:join(Path, "/")]),
      Req:respond(403, [{"Content-Type", "text/plain"}], "secure link expired")
  end;
      

http(_Host, _Method, _Path, _Req) ->
  unhandled.
