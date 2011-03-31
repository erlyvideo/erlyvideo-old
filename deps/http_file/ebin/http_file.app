{application, http_file,
[{description, "http_file"},
 {vsn, "0.5"},
 {modules, [
	erlyvideo_http_file,
	http_file,
	http_file_sup,
	http_file_tracker
  ]},
 {registered,[http_file]},
 {applications, [kernel,stdlib]},
 {mod, {http_file,[]}},
 {env, [
	{cache_path,"/tmp"}
 ]}
]}.

