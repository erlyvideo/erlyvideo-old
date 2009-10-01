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
				hmac256,
				mp4,
				rtmp_handshake,
				sha2
  				]},
  {registered,   []},
  {applications, [kernel, stdlib]},
  {mod, {ems_app, []}},
  {env, [
	{listen_port, 1935}, 
	{file_dir, "/tmp"},
	{netstream, "netstream@localhost"}	
	]}
 ]
}.








