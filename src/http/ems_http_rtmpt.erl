-module(ems_http_rtmpt).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../../include/ems.hrl").

-export([http/4]).

http(Host, 'POST', ["open", ChunkNumber], Req) ->
  <<_Timeout>> = Req:get(body),
  {ok, Pid, SessionId} = rtmpt:open(Req:get(peer_addr), rtmp_session),
  ems_log:access(Host, "RTMPT OPEN ~p ~p ~p", [SessionId, ChunkNumber, Pid]),
  Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], [SessionId, "\n"]);
  
http(Host, 'POST', ["idle", SessionId, SequenceNumber], Req) ->
  case rtmpt:idle(SessionId, Req:get(peer_addr), list_to_integer(SequenceNumber)) of
    {ok, Data} ->
      Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], [33, Data]);
    {error, _} ->
      ems_log:error(Host, "RTMPT IDLE to closed session ~p", [SessionId]),
      Req:stream(<<0>>),
      Req:stream(close)
  end;

http(Host, 'POST', ["send", SessionId, SequenceNumber], Req) ->
  % error_logger:info_msg("Request: send/~p/~p.\n", [SessionId, SequenceNumber]),
  case rtmpt:send(SessionId, Req:get(peer_addr), list_to_integer(SequenceNumber), Req:get(body)) of
    {ok, Data} ->
      Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], [33, Data]);
    {error, _} ->
      ems_log:error(Host, "RTMPT SEND to closed session ~p", [SessionId]),
      Req:stream(<<0>>),
      Req:stream(close)
  end;
  
  
http(Host, 'POST', ["close", SessionId, _ChunkNumber], Req) ->
  ems_log:error(Host, "RTMPT CLOSE ~p", [SessionId]),
  rtmpt:close(SessionId, Req:get(peer_addr)),
  Req:stream(<<0>>),
  Req:stream(close);
    
http(_Host, 'POST', ["fcs", "ident", _ChunkNumber], Req) ->
  % ems_log:access(Host, "RTMPT ident/~p", [ChunkNumber]),
  Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], "0.1");
    
http(_Host, 'POST', ["fcs", "ident2"], Req) ->
  % ems_log:access(Host, "RTMPT ident2", []),
  Req:ok([{'Content-Type', ?CONTENT_TYPE}, ?SERVER_HEADER], "0.1");

http(_Host, _Method, _Path, _Req) ->
  unhandled.
