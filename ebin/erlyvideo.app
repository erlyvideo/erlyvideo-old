{application, erlyvideo,
 [
  {description, "Erlang Video Server"},
  {vsn, "1.0"},
  {id, "erlyvideo"},
  {modules,      [
	apps_push,
	apps_recording,
	apps_rtmp,
	apps_streaming,
	hmac256,
	sha2,
	uuid,
	ems,
	ems_app,
	ems_encoding,
	ems_flv,
	rtmp_session,
	ems_http,
	rtmp,
	rtmp_listener,
	ems_sup,
	flv,
	gen_format,
	mp4,
	ems_play,
	ems_stream,
	media_entry,
	media_provider,
	mpeg_ts,
	mochijson2,
	mochinum,
	rtmp_session,
	rtmp_handshake,
	rtmp_session,
	fitc_demo,
	ems_script
  ]},
  {registered,   []},
  {applications, [kernel,stdlib,crypto,rtsp,rtmp,log4erl]},
  {mod, {erlyvideo, []}},
  {env, []}
 ]
}.








