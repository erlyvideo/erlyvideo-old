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


handle(Host, 'GET', [], Req) ->
  localhost,
  erlydtl:compile("wwwroot/index.html", index_template),
  
  Query = Req:parse_qs(),
  io:format("GET / ~p~n", [Query]),
  File = proplists:get_value("file", Query, "video.mp4"),
  case file:list_dir(file_play:file_dir(Host)) of
    {ok, FileList} -> ok;
    {error, Error} -> 
      FileList = [],
      error_logger:error_msg("Invalid HTTP root directory: ~p (~p)~n", [file_play:file_dir(Req:host()), Error])
  end,
  Secret = ems:get_var(secret_key, Host, undefined),
  {ok, Index} = index_template:render([
    {files, FileList},
    {hostname, ems:get_var(host, Host, "rtmp://localhost")},
    {live_id, uuid:to_string(uuid:v4())},
    {url, File},
    {session, json_session:encode([{channels, [10, 12]}, {user_id, 5}], Secret)}]),
  Req:ok([{'Content-Type', "text/html; charset=utf8"}], Index);


handle(Host, 'GET', ["admin"], Req) ->
  erlydtl:compile("wwwroot/admin.html", admin_template),
  % {ok, Contents} = file:read_file("player/player.html"),

  Entries = media_provider:entries(Host),
  {ok, Index} = admin_template:render([
  {entries, Entries}]),
  Req:ok([{'Content-Type', "text/html; charset=utf8"}], Index);


handle(Host, 'GET', ["chat.html"], Req) ->
  erlydtl:compile("wwwroot/chat.html", chat_template),
  Secret = ems:get_var(secret_key, Host, undefined),
  {ok, Index} = chat_template:render([
    {hostname, ems:get_var(host, Host, "rtmp://localhost")},
    {session, json_session:encode([{channels, [10, 12]}, {user_id, 5}], Secret)}
  ]),
  Req:ok([{'Content-Type', "text/html; charset=utf8"}], Index);

  
handle(_Host, 'POST', ["open", ChunkNumber], Req) ->
  error_logger:info_msg("Request: open/~p.\n", [ChunkNumber]),
  SessionId = generate_session_id(),
  <<Timeout>> = Req:get(body),
  {ok, _Pid} = rtmpt_session:start(SessionId),
  error_logger:info_msg("Opened session ~p, timeout ~p.\n", [SessionId, Timeout]),
  Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], [SessionId, "\n"]);
  
handle(_Host, 'POST', ["idle", SessionId, SequenceNumber], Req) ->
  % error_logger:info_msg("Request: idle/~p/~p.\n", [SessionId, SequenceNumber]),
  case ets:match_object(rtmp_sessions, {SessionId, '$2'}) of
      [{SessionId, Rtmp}] ->
          {Buffer} = gen_fsm:sync_send_event(Rtmp, {recv, list_to_int(SequenceNumber)}),
          % io:format("Returning ~p~n", [size(Buffer)]),
          Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], [33, Buffer]);
      _ ->
          error_logger:info_msg("Request 'idle' to closed session ~p\n", [SessionId]),
          Req:stream(<<0>>),
          Req:stream(close)
  end;


handle(_Host, 'POST', ["send", SessionId, SequenceNumber], Req) ->
  % error_logger:info_msg("Request: send/~p/~p.\n", [SessionId, SequenceNumber]),
  case ets:match_object(rtmp_sessions, {SessionId, '$2'}) of
      [{SessionId, Rtmp}] ->
          gen_fsm:send_event(Rtmp, {client_data, Req:get(body)}),
          {Buffer} = gen_fsm:sync_send_event(Rtmp, {recv, list_to_int(SequenceNumber)}),
          % io:format("Returning ~p~n", [size(Buffer)]),
          Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], [33, Buffer]);
      _ ->
          error_logger:info_msg("Request 'idle' to closed session ~p\n", [SessionId]),
          Req:stream(<<0>>),
          Req:stream(close)
  end;
  
  
handle(_Host, 'POST', ["close", SessionId, ChunkNumber], Req) ->
    error_logger:info_msg("Request: close/~p/~p.\n", [SessionId, ChunkNumber]),
    Req:stream(<<0>>),
    Req:stream(close);
    
handle(_Host, 'POST', ["fcs", "ident", ChunkNumber], Req) ->
    error_logger:info_msg("Request: ident/~p.\n", [ChunkNumber]),
    Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], "0.1");
    
handle(_Host, 'POST', ["fcs", "ident2"], Req) ->
    error_logger:info_msg("Request: ident2.\n"),
    Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], "0.1");
  
handle(_Host, 'POST', ["channels", ChannelS, "message"], Req) ->
  Message = proplists:get_value("message", Req:parse_post()),
  Channel = list_to_integer(ChannelS),
  rtmp_listener:send_to_channel(Channel, list_to_binary(Message)),
  Req:respond(200, [{"Content-Type", "text/plain"}], "200 OK\n");

handle(_Host, 'POST', ["users", UserS, "message"], Req) ->
  Message = proplists:get_value("message", Req:parse_post()),
  User = list_to_integer(UserS),
  rtmp_listener:send_to_user(User, list_to_binary(Message)),
  Req:respond(200, [{"Content-Type", "text/plain"}], "200 OK\n");

  
handle(Host, 'GET', ["stream", Name], Req) ->
  case media_provider:play(Host, Name, [{stream_id, 1}]) of
    {ok, PlayerPid} ->
      mpeg_ts:play(Name, PlayerPid, Req);
    {notfound} ->
      Req:respond(404, [{"Content-Type", "text/plain"}], "404 Page not found. ~p: ~p", [Name, Req]);
    Reason -> 
      Req:respond(500, [{"Content-Type", "text/plain"}], "500 Internal Server Error.~n Failed to start video player: ~p~n ~p: ~p", [Reason, Name, Req])
  end;
  
handle(_Host, 'GET', Path, Req) ->
  FileName = filename:absname(filename:join(["wwwroot" | Path])),
  case filelib:is_regular(FileName) of
    true ->
      ?D({"GET", FileName}),
      Req:file(FileName);
    false ->
      Req:respond(404, [{"Content-Type", "text/plain"}], "404 Page not found. ~p: ~p", [Path, Req])
  end;

handle(Host, 'PUT', ["stream", Name], Req) ->
  ?D({"Stream", Name}),
  Stream = media_provider:create(Host, Name, mpeg_ts_passive),
  gen_tcp:controlling_process(Req:socket(), Stream),
  gen_server:call(Stream, {set_socket, Req:socket()}),
  exit(leave);
  
% handle the 404 page not found
handle(_Host, _, Path, Req) ->
	Req:respond(404, [{"Content-Type", "text/plain"}], "404 Page not found. ~p: ~p", [Path, Req]).




-spec generate_session_id() -> list().
generate_session_id() ->
    {T1, T2, T3} = now(),
    lists:flatten(io_lib:format("~p:~p:~p", [T1, T2, T3])).


-spec list_to_int(list()) -> integer().
list_to_int(String) ->
    case io_lib:fread("~u", String) of
        {ok, [Num], _} ->
            Num;
        _ -> undefined
    end.
