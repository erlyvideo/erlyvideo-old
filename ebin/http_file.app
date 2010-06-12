{application, http_file,
[{description, "http_file"},
 {vsn, "0.5"},
 {modules, [
	ems_http_file,
	http_file,
	http_file_request,
	http_file_sup
  ]},
 {registered,[http_file]},
 {applications, [kernel,stdlib]},
 {mod, {http_file,[]}},
 {env, [
 ]}
]}.

