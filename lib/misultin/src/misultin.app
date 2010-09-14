{application, misultin,
[
	{description, "Lightweight HTTP Server Library"},
	{vsn, '0.3'},
	{modules, [misultin_socket, misultin_req]},
	{registered, [misultin]},
	{env, []},
	{applications, [kernel, stdlib]}
]}.
