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
  {env, []}
 ]
}.








