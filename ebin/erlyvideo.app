{application, erlyvideo,
 [
  {description, "Erlang Video Server"},
  {vsn, "1.0"},
  {id, "erlyvideo"},
  {modules,      [
	apps_push,
	apps_recording,
	apps_shared_objects,
	apps_streaming,
	array_timeshift,
	directory_playlist,
	ems,
	ems_encoding,
	ems_event,
	ems_flv_streams,
	ems_http,
	ems_log,
	ems_media,
	ems_media_clients,
	ems_rtsp,
	ems_script,
	ems_sup,
	ems_users,
	ems_vhosts,
	erlyvideo,
	erlyvideo_ctl,
	file_media,
	fitc_demo,
	flv_reader,
	gen_cache,
	gen_format,
	gen_server2,
	http_media,
	iphone_streams,
	json_session,
	live_media,
	media_provider,
	media_ticker,
	misultin,
	misultin_req,
	misultin_socket,
	mochijson2,
	mp4_reader,
	mpegts_file_media,
	mpegts_media,
	mpegts_play,
	protected_play,
	referer_check,
	rtmp_media,
	rtmp_publish,
	rtmp_session,
	rtsp_media,
	shared_object,
	shared_objects,
	trusted_login,
	uuid
  ]},
  {registered,   []},
  {applications, [kernel,stdlib,crypto,rtsp,rtmp,log4erl]},
  {mod, {erlyvideo, []}},
  {env, []}
 ]
}.








