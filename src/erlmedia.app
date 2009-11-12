{application, erlmedia,
 [
  {description, "Erlang Media Server"},
  {vsn, "0.0.6"},
  {id, "erlmedia"},
  {modules,      [
		  amf0,
		  amf3,
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
			ems_client,
			ems_http,
			ems_rtmp,
			ems_server,
			ems_sup,
			flv,
			gen_format,
			mp4,
			ems_play,
			file_play,
			media_entry,
			media_provider,
			mpeg_ts,
			mochijson2,
			mochinum,
			rtmp_client,
			rtmp_handshake,
			rtmp_session
 				]},
  {registered,   []},
  {applications, [kernel, stdlib]},
  {mod, {ems_app, []}},
  {env, [
	{listen_port, 1935}, 
	{file_dir, "/tmp"},
	{tmp_dir, "/tmp/erlyvideo"},
	{host, "rtmp://localhost"},
	{applications, [apps_streaming, apps_recording, apps_push, apps_rtmp]},
	{http_port, 8082},
	{secret_key, "fddbb018f51cb867a2e6d10b9eea7bd5eaec2d9ee1b814856251776f08e8acdef49154b0317b432ff6dfbd7c7e0e74d5db53287cc991e8cf46da8d6a27fc5ae8"},
	{netstream, 'netstream@localhost'}
	]}
 ]
}.








