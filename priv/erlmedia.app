{application, erlmedia,
 [
  {description, "Erlang Media Server"},
  {vsn, "1.0"},
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
			rtmp_session,
			ems_http,
			rtmp,
			rtmp_listener,
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
			rtmp_session,
			rtmp_handshake,
			rtmp_session
 				]},
  {registered,   []},
  {applications, [kernel, stdlib]},
  {mod, {ems_app, []}},
  {env, [
		{rtmp_port, 1935},
		{rtsp_port, 8080},
		{http_port, 8082},
		{vhosts, [
		   {default, [
		      {hostname, ["localhost"]},
		      {applications, [apps_push, apps_streaming, apps_recording, apps_rtmp, apps_shared_objects]},
		      {secret_key, "123"},
					{host, "rtmp://localhost"},
		      {file_dir, "/tmp"}
		   ]},
		   {production, [
		      {hostname, ["production.local"]},
		      {applications, [apps_push, apps_streaming, apps_recording, apps_rtmp, apps_shared_objects]},
					{auth_module, json_session},
		      {secret_key, "fddbb018f51cb867a2e6d10b9eea7bd5eaec2d9ee1b814856251776f08e8acdef49154b0317b432ff6dfbd7c7e0e74d5db53287cc991e8cf46da8d6a27fc5ae8"},
					{host, "rtmp://localhost"},
		      {file_dir, "/tmp"}
		   ]}
		]}
  ]}
 ]
}.








