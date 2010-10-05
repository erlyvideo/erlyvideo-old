%% This is the application resource file (.app file) for the 'base'
%% application.
{application, ertp,
[{description, "RTP handling library"},
 {vsn, "0.1"},
 {modules, [
            ertp,
            ertp_sup,
            rtp_server,
            sdp,
            sdp_tests
           ]},
 {registered,[]},
 {applications, [kernel,stdlib]},
 {mod, {ertp,[]}}
]}.
