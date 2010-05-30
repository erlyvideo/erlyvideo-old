%% This is the application resource file (.app file) for the 'base'
%% application.
{application, rtsp,
[{description, "RTSP handling library"},
 {vsn, "0.1"},
 {modules, [rtsp,rtp_server,rtsp_example_callback,rtsp_listener,rtsp_socket,rtsp_sup,sdp]},
 {registered,[rtsp]},
 {applications, [kernel,stdlib]},
 {mod, {rtsp,[]}}
]}.
