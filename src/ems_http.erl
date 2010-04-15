-module(ems_http).
-export([start_link/1, stop/0, handle_http/1]).
-include("../include/ems.hrl").

% start misultin http server
start_link(Port) ->
	misultin:start_link([{port, Port}, {loop, fun handle_http/1}]).

% stop misultin
stop() ->
	misultin:stop().

% callback on request received
handle_http(Req) ->	
  random:seed(now()),
  handle(ems:host(Req:host()), Req:get(method), Req:resource([urldecode]), Req).

wwwroot(Host) ->
  ems:get_var(wwwroot, Host, ems:get_var(wwwroot, "wwwroot")).

handle(Host, 'GET', [], Req) ->
  localhost,
  erlydtl:compile(wwwroot(Host) ++ "/index.html", index_template),
  
  Query = Req:parse_qs(),
  io:format("GET / ~p~n", [Query]),
  File = proplists:get_value("file", Query, "video.mp4"),
  Autostart = proplists:get_value("autostart", Query, "false"),
  case file:list_dir(ems_stream:file_dir(Host)) of
    {ok, FileList} -> ok;
    {error, Error} -> 
      FileList = [],
      error_logger:error_msg("Invalid file_dir directory: ~p (~p)~n", [ems_stream:file_dir(Req:host()), Error])
  end,
  Secret = ems:get_var(secret_key, Host, undefined),
  {ok, Index} = index_template:render([
    {files, FileList},
    {hostname, ems:get_var(host, Host, "rtmp://localhost")},
    {autostart, Autostart},
    {live_id, uuid:to_string(uuid:v4())},
    {url, File},
    {session, json_session:encode([{channels, [10, 12]}, {user_id, 5}], Secret)}]),
  Req:ok([{'Content-Type', "text/html; charset=utf8"}], Index);


handle(_, 'GET', ["admin", "config.xml"], Req) ->
  Config = mochijson2:encode({struct, application:get_all_env(erlyvideo)}),
  Req:ok([{'Content-Type', "text/json; charset=utf8"}], iolist_to_binary(Config));

handle(Host, 'GET', ["admin"], Req) ->
  erlydtl:compile(wwwroot(Host) ++ "/admin.html", admin_template),
  % {ok, Contents} = file:read_file("player/player.html"),

  Entries = media_provider:entries(Host),
  {ok, Index} = admin_template:render([
  {entries, Entries}]),
  Req:ok([{'Content-Type', "text/html; charset=utf8"}], Index);


handle(Host, 'GET', ["chat.html"], Req) ->
  erlydtl:compile(wwwroot(Host) ++ "/chat.html", chat_template),
  Secret = ems:get_var(secret_key, Host, undefined),
  {ok, Index} = chat_template:render([
    {hostname, ems:get_var(host, Host, "rtmp://localhost")},
    {session, json_session:encode([{channels, [10, 12]}, {user_id, 5}], Secret)}
  ]),
  Req:ok([{'Content-Type', "text/html; charset=utf8"}], Index);

handle(Host, 'GET', ["videoconf.html"], Req) ->
  erlydtl:compile(wwwroot(Host) ++ "/videoconf.html", chat_template),
  Query = Req:parse_qs(),
  File = proplists:get_value("file", Query, "conference"),
  Secret = ems:get_var(secret_key, Host, undefined),
  {ok, Index} = chat_template:render([
    {hostname, ems:get_var(host, Host, "rtmp://localhost")},
    {url, File},
    {session, json_session:encode([{channels, [10, 12]}, {user_id, 5}], Secret)}
  ]),
  Req:ok([{'Content-Type', "text/html; charset=utf8"}], Index);

  
handle(Host, 'POST', ["open", ChunkNumber], Req) ->
  <<_Timeout>> = Req:get(body),
  {ok, Pid, SessionId} = rtmpt:open(Req:get(peer_addr), rtmp_session),
  ems_log:access(Host, "RTMPT OPEN ~p ~p ~p", [SessionId, ChunkNumber, Pid]),
  Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], [SessionId, "\n"]);
  
handle(Host, 'POST', ["idle", SessionId, SequenceNumber], Req) ->
  case rtmpt:idle(SessionId, Req:get(peer_addr), list_to_int(SequenceNumber)) of
    {ok, Data} ->
      Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], [33, Data]);
    {error, _} ->
      ems_log:error(Host, "RTMPT IDLE to closed session ~p", [SessionId]),
      Req:stream(<<0>>),
      Req:stream(close)
  end;

handle(Host, 'POST', ["send", SessionId, SequenceNumber], Req) ->
  % error_logger:info_msg("Request: send/~p/~p.\n", [SessionId, SequenceNumber]),
  case rtmpt:send(SessionId, Req:get(peer_addr), list_to_int(SequenceNumber), Req:get(body)) of
    {ok, Data} ->
      Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], [33, Data]);
    {error, _} ->
      ems_log:error(Host, "RTMPT SEND to closed session ~p", [SessionId]),
      Req:stream(<<0>>),
      Req:stream(close)
  end;
  
  
handle(Host, 'POST', ["close", SessionId, _ChunkNumber], Req) ->
  ems_log:error(Host, "RTMPT CLOSE ~p", [SessionId]),
  rtmpt:close(SessionId, Req:get(peer_addr)),
  Req:stream(<<0>>),
  Req:stream(close);
    
handle(_Host, 'POST', ["fcs", "ident", _ChunkNumber], Req) ->
  % ems_log:access(Host, "RTMPT ident/~p", [ChunkNumber]),
  Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], "0.1");
    
handle(_Host, 'POST', ["fcs", "ident2"], Req) ->
  % ems_log:access(Host, "RTMPT ident2", []),
  Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], "0.1");
  
handle(Host, 'POST', ["channels", ChannelS, "message"], Req) ->
  Message = proplists:get_value("message", Req:parse_post()),
  Channel = list_to_integer(ChannelS),
  ems_users:send_to_channel(Host, Channel, list_to_binary(Message)),
  Req:respond(200, [{"Content-Type", "text/plain"}], "200 OK\n");

handle(Host, 'POST', ["users", UserS, "message"], Req) ->
  Message = proplists:get_value("message", Req:parse_post()),
  User = list_to_integer(UserS),
  ems_users:send_to_user(Host, User, list_to_binary(Message)),
  Req:respond(200, [{"Content-Type", "text/plain"}], "200 OK\n");

handle(Host, 'GET', ["stats.json"], Req) ->
  Stats = lists:map(fun({Name, Clients}) ->
    {Name, {struct, [{clients, length(Clients)}]}}
  end, media_provider:entries(Host)),
  Req:respond(200, [{"Content-Type", "application/json"}], [mochijson2:encode({struct, Stats}), "\n"]);
  
handle(Host, 'GET', ["stream" | Name], Req) ->
  Query = Req:parse_qs(),
  Seek = list_to_integer(proplists:get_value("start", Query, "0")),
  Req:stream(head, [{"Content-Type", "video/mpeg2"}, {"Connection", "close"}]),
  case media_provider:play(Host, string:join(Name, "/"), [{stream_id, 1}, {seek, {before, Seek}}]) of
    {ok, PlayerPid} ->
      mpegts:play(Name, PlayerPid, Req),
      ok;
    {notfound, Reason} ->
      Req:stream(io_lib:format("404 Page not found.\n ~p: ~s ~s\n", [Name, Host, Reason])),
      Req:stream(close);
    Reason -> 
      Req:stream(io_lib:format("500 Internal Server Error.~n Failed to start video player: ~p~n ~p: ~p", [Reason, Name, Req])),
      Req:stream(close)
  end;

handle(Host, 'GET', ["flv" | Name], Req) ->
  Query = Req:parse_qs(),
  Seek = list_to_integer(proplists:get_value("start", Query, "0")),
  Req:stream(head, [{"Content-Type", "video/mpeg2"}, {"Connection", "close"}]),
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

handle(Host, 'GET', ["flvcontrol", SessionId, "pause"], Req) ->
  case ems_flv_streams:command({Host,SessionId}, pause) of
    undefined -> Req:respond(404, [{'Content-Type', "text/plain"}], "404 Not Found");
    _ -> Req:ok([{'Content-Type', "text/plain"}], "ok")
  end;

handle(Host, 'GET', ["flvcontrol", SessionId, "resume"], Req) ->
  case ems_flv_streams:command({Host,SessionId}, resume) of
    undefined -> Req:respond(404, [{'Content-Type', "text/plain"}], "404 Not Found");
    _ -> Req:ok([{'Content-Type', "text/plain"}], "ok")
  end;

handle(Host, 'GET', ["flvcontrol", SessionId, "seek", Timestamp], Req) ->
  case ems_flv_streams:command({Host,SessionId}, {seek, before, list_to_integer(Timestamp)}) of
    undefined -> Req:respond(404, [{'Content-Type', "text/plain"}], "404 Not Found");
    _ -> Req:ok([{'Content-Type', "text/plain"}], "ok")
  end;


handle(Host, 'GET', ["iphone", "playlists" | StreamName] = Path, Req) ->
  _Hostname = proplists:get_value('Host', Req:get(headers)),
  
  ems_log:access(Host, "GET ~p ~s /~s", [Req:get(peer_addr), "-", string:join(Path, "/")]),
  FullName = string:join(StreamName, "/"),
  {ok, Re} = re:compile("^(.+).m3u8$"),
  {match, [_, Name]} = re:run(FullName, Re, [{capture, all, binary}]),
  
  {Start,Count,SegmentLength} = iphone_streams:segments(Host, Name),
  SegmentList = lists:map(fun(N) ->
    io_lib:format("#EXTINF:~p,~n/iphone/segments/~s/~p.ts~n", [SegmentLength, Name, N])
  end, lists:seq(Start, Count)),
  Playlist = [
    io_lib:format("#EXTM3U~n#EXT-X-MEDIA-SEQUENCE:0~n#EXT-X-TARGETDURATION:~p~n", [SegmentLength]),
    SegmentList,
    "#EXT-X-ENDLIST\n"
  ],
  Req:respond(200, [{"Content-Type", "application/vnd.apple.mpegurl"}], Playlist);
  

handle(Host, 'GET', ["iphone", "segments" | StreamName] = Path, Req) ->
  ems_log:access(Host, "GET ~p ~s /~s", [Req:get(peer_addr), "-", string:join(Path, "/")]),
  {ok, Re} = re:compile("^(.+)/(\\d+).ts$"),
  {match, [_, Name, SegmentId]} = re:run(string:join(StreamName, "/"), Re, [{capture, all, binary}]),
  
  Segment = list_to_integer(binary_to_list(SegmentId)),
  Req:stream(head, [{"Content-Type", "video/MP2T"}, {"Connection", "close"}]),
  case iphone_streams:find(Host, Name, Segment) of
    {ok, PlayerPid} ->
      mpegts:play(Name, PlayerPid, Req),
      ok;
    {notfound, Reason} ->
      Req:stream(io_lib:format("404 Page not found.\n ~p: ~s ~s\n", [Name, Host, Reason])),
      Req:stream(close);
    Reason -> 
      Req:stream(io_lib:format("500 Internal Server Error.~n Failed to start video player: ~p~n ~p: ~p", [Reason, Name, Req])),
      Req:stream(close)
  end;
  
handle(Host, 'GET', Path, Req) ->
  FileName = filename:absname(filename:join([wwwroot(Host) | Path])),
  case filelib:is_regular(FileName) of
    true ->
      ems_log:access(Host, "GET ~p ~s /~s", [Req:get(peer_addr), "-", string:join(Path, "/")]),
      Req:file(FileName);
    false ->
      ems_log:access(Host, "NOTFOUND ~p ~s /~s", [Req:get(peer_addr), "-", string:join(Path, "/")]),
      Req:respond(404, [{"Content-Type", "text/plain"}], "404 Page not found. ~p: ~p", [Path, Req])
  end;

handle(Host, 'PUT', ["stream", Name], Req) ->
  ?D({"Stream", Name}),
  ems_log:access(Host, "MPEGTS PUT ~s ~s", [Host, Name]),
  Stream = media_provider:open(Host, Name, [{type, mpegts_passive}]),
  stream_media:pass_socket(Stream, Req:socket()),
  exit(leave);
  
% handle the 404 page not found
handle(_Host, _, Path, Req) ->
	Req:respond(404, [{"Content-Type", "text/plain"}], "404 Page not found. ~p: ~p", [Path, Req]).





-spec list_to_int(list()) -> integer().
list_to_int(String) ->
    case io_lib:fread("~u", String) of
        {ok, [Num], _} ->
            Num;
        _ -> undefined
    end.
