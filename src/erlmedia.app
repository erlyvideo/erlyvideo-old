{application, erlmedia,
 [
  {description, "Erlang Media Server"},
  {vsn, "0.0.6"},
  {id, "erlmedia"},
  {modules,      [
  			ems,
				ems_amf,
				ems_app,
				ems_cluster,
				ems_flv,
				ems_fsm,
				ems_play,
				ems_rtmp,
				ems_server,
				ems_sup,
				gen_rtmp,
				gen_server_cluster,
				mochijson2,
				hmac256,
				mp4,
				rtmp_client,
				rtmp_handshake,
				rtmp_session,
				sha2
  				]},
  {registered,   []},
  {applications, [kernel, stdlib]},
  {mod, {ems_app, []}},
  {env, [
	{listen_port, 1935}, 
	{file_dir, "/tmp"},
	{host, "rtmp://localhost"},
	{http_port, 8082},
	{secret_key, "fddbb018f51cb867a2e6d10b9eea7bd5eaec2d9ee1b814856251776f08e8acdef49154b0317b432ff6dfbd7c7e0e74d5db53287cc991e8cf46da8d6a27fc5ae8"},
	{netstream, "netstream@localhost"}	
	]}
 ]
}.








