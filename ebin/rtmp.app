%% This is the application resource file (.app file) for the 'base'
%% application.
{application, rtmp,
[{description, "RTMP handling library"},
 {vsn, "0.1"},
 {modules, [rtmp,rtmp_app,rtmp_bench,rtmp_listener,rtmp_sup,rtmpt,rtmpt_sessions,sha2,hmac256,rtmp_handshake,rtmp_lib,rtmp_socket,rtmp_monitor]},
 {registered,[rtmp,rtmpt_sessions_sup,rtmpt_session_sup,rtmp_socket_sup,rtmp_monitor_sup,rtmp_monitor]},
 {applications, [kernel,stdlib]},
 {mod, {rtmp_app,[]}}
]}.
