{application, erlmedia,
 [
  {description, "Erlang Media Server"},
  {vsn, "0.0.6"},
  {id, "erlmedia"},
  {modules,      [
  				ems,
				ems_amf,
				ems_app,
				ems_demo,
				ems_flv,
				ems_fsm,
				ems_rtmp,
				ems_server,
				ems_cluster,
				gen_rtmp,
				gen_server_cluster
  				]},
  {registered,   []},
  {applications, [kernel, stdlib]},
  {mod, {ems_app, []}},
  {env, [
	{listen_port, 1935}, 
	{flv_dir, "/tmp"}
	]}
 ]
}.








