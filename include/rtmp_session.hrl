-record(rtmp_session, {
  host,
  path,
	socket,    % client socket
	addr,      % client address
	port,
	session_id,
	amf_ver = 0,
	plugin_data,
	user_id     = undefined,
	player_info = [],
	streams1      = [],
	bytes_recv   = 0,
	bytes_sent   = 0,
	play_stats   = [],
	cached_shared_objects = []
	}).

-record(rtmp_stream, {
  pid,
  stream_id,
  base_dts,
  started = false,
  seeking = false
}).