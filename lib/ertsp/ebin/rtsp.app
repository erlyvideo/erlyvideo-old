%% This is the application resource file (.app file) for the 'base'
%% application.
{application, rtsp,
[{description, "RTSP handling library"},
 {vsn, "0.1"},
 {modules, [rtsp]},
 {registered,[rtsp]},
 {applications, [kernel,stdlib]},
 {mod, {rtsp_app,[]}}
]}.
