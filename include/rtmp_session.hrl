-record(rtmp_session, {
  host,
  path,
	socket,    % client socket
	addr,      % client address
	port,
	session_id,
	amf_ver = 0,
	user_id     = undefined,
	player_info = [],
	streams      = {},
	cached_shared_objects = []
	}).
