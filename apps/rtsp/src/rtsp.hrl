-define(FRAMES_BUFFER, 15).
-define(REORDER_FRAMES, 10).
-define(DEFAULT_TIMEOUT, 30000).
-define(TIMEOUT, 10000).

-define(SERVER_NAME, "Erlyvideo").

-record(rtsp_socket, {
  callback,
  direction,
  buffer = <<>>,
  user_agent,
  transport = interleaved :: interleaved | udp,
  addr,
  port,
  url,
  auth = "",
  socket,
  options,
  rtp_streams = {},
  control_map,
  media         :: pid(),
  media_info,
  rtp_ref       :: reference(),
  state,
  pending,
  pending_reply = ok,
  seq = 0,
  timeout = ?DEFAULT_TIMEOUT,
  session,
  rtp_udp,
  dump_traffic = true,
  
  rtp           :: pid()
  
}).
