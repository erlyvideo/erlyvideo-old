-record(rtmp_session, {
  host,
  path,
	socket,    % client socket
	addr,      % client address
	port,
	session_id,
	user_id     = undefined,
	player_info = [],
	streams      = undefined,
	cached_shared_objects = []
	}).
