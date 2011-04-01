{application, rtsp,
[{description, "RTSP handling library"},
 {vsn, "0.1"},
 {modules, [rtsp,rtsp_example_callback,rtsp_inbound, rtsp_listener,rtsp_outbound,rtsp_socket,rtsp_sup,rtsp_test_client,rtsp_tests]},
 {registered,[rtsp]},
 {applications, [kernel,stdlib]},
 {mod, {rtsp,[]}}
]}.
