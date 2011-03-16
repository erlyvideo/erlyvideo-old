{application, rtp,
[{description, "RTP handling library"},
 {vsn, "0.1"},
 {modules, [
            ertp,
            ertp_sup,
            rtp_decoder,
            rtp_server
            ]},
 {registered,[]},
 {applications, [kernel,stdlib]},
 {mod, {ertp,[]}}
]}.
