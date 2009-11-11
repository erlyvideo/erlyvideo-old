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
  handle(Req:get(method), Req:resource([lowercase, urldecode]), Req).


handle('GET', [], Req) ->
  erlydtl:compile("player/player.html", index_template),
  % {ok, Contents} = file:read_file("player/player.html"),
  
  Query = Req:parse_qs(),
  io:format("GET / ~p~n", [Query]),
  File = case lists:keyfind("file", 1, Query) of
    {"file", _File} -> _File;
    false -> "video.mp4"
  end,
  case file:list_dir(file_play:file_dir()) of
    {ok, FileList} -> ok;
    {error, Error} -> 
      FileList = [],
      error_logger:error_msg("Invalid HTTP root directory: ~p (~p)~n", [file_play:file_dir(), Error])
  end,
  {ok, Index} = index_template:render([
    {files, FileList},
    {hostname, ems:get_var(host, "rtmp://localhost")},
    {live_id, uuid:to_string(uuid:v4())},
    {url, File},
    {session, rtmp_session:encode([{channels, [10, 12]}, {user_id, 5}]) }]),
  Req:ok([{'Content-Type', "text/html; charset=utf8"}], Index);


handle('GET', ["admin"], Req) ->
  erlydtl:compile("player/admin.html", admin_template),
  % {ok, Contents} = file:read_file("player/player.html"),

  FileList = [],
  {ok, Index} = admin_template:render([
  {files, FileList}]),
  Req:ok([{'Content-Type', "text/html; charset=utf8"}], Index);

  
handle('POST', ["open", ChunkNumber], Req) ->
  error_logger:info_msg("Request: open/~p.\n", [ChunkNumber]),
  SessionId = generate_session_id(),
  <<Timeout>> = Req:get(body),
  {ok, _Pid} = rtmp_client:start_link(SessionId),
  error_logger:info_msg("Opened session ~p, timeout ~p.\n", [SessionId, Timeout]),
  Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], [SessionId, "\n"]);
  
handle('POST', ["idle", SessionId, SequenceNumber], Req) ->
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


handle('POST', ["send", SessionId, SequenceNumber], Req) ->
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
  
  
handle('POST', ["close", SessionId, ChunkNumber], Req) ->
    error_logger:info_msg("Request: close/~p/~p.\n", [SessionId, ChunkNumber]),
    Req:stream(<<0>>),
    Req:stream(close);
    
handle('POST', ["fcs", "ident", ChunkNumber], Req) ->
    error_logger:info_msg("Request: ident/~p.\n", [ChunkNumber]),
    Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], "0.1");
    
handle('POST', ["fcs", "ident2"], Req) ->
    error_logger:info_msg("Request: ident2.\n"),
    Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], "0.1");
  
handle('GET', ["stream", Name], Req) ->
  case media_provider:play(Name) of
    {ok, PlayerPid} ->
      mpeg_ts:play(Name, PlayerPid, Req);
    {notfound} ->
      Req:respond(404, [{"Content-Type", "text/plain"}], "404 Page not found. ~p: ~p", [Name, Req]);
    Reason -> 
      Req:respond(500, [{"Content-Type", "text/plain"}], "500 Internal Server Error.~n Failed to start video player: ~p~n ~p: ~p", [Reason, Name, Req])
  end;
  
handle('GET', Path, Req) ->
  FileName = filename:absname(filename:join(["player" | Path])),
  ?D({"GET", FileName, file:read_file_info(FileName)}),
  case filelib:is_regular(FileName) of
    true ->
      Req:file(FileName);
    false ->
      Req:respond(404, [{"Content-Type", "text/plain"}], "404 Page not found. ~p: ~p", [Path, Req])
  end;

  
% handle the 404 page not found
handle(_, Path, Req) ->
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
