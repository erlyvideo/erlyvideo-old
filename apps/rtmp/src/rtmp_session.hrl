-record(rtmp_session, {
  path,
  module,
	socket,    % client socket
	addr,      % client address
	port,
	session_id,
	amf_ver = 0,
	plugin_data = [],
	user_id     = undefined,
	player_info = [],
	streams1      = [],
	bytes_recv   = 0,
	bytes_sent   = 0,
	play_stats   = [],
	cached_shared_objects = [],
	properties = []
	}).

-record(rtmp_stream, {
  pid,
  stream_id,
  options = [],
  base_dts,
  started = false,
  seeking = false,
  receive_audio = true,
  receive_video = true,
  recording = false,
  recording_ref,
  name,
  properties = []
}).