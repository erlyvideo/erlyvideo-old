{application, rtp,
[{description, "RTP handling library"},
 {vsn, "0.1"},
 {modules, [
            rtp,
            rtp_sup,
            rtp_decoder,
            rtp_encoder
            ]},
 {registered,[]},
 {applications, [kernel,stdlib]},
 {mod, {rtp,[]}}
]}.
