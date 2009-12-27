%% This is the application resource file (.app file) for the 'base'
%% application.
{application, rtmp,
[{description, "RTMP handling library"},
 {vsn, "0.1"},
 {modules, [rtmp,hmac256,rtmp_handshake,rtmp_lib,rtmp_socket]},
 {registered,[rtmp]},
 {applications, [kernel,stdlib]},
 {mod, {rtmp_app,[]}}
]}.
