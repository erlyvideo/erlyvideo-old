{application, rtp,
[{description, "RTP handling library"},
 {vsn, "0.1"},
 {modules, [
            rtp,
            rtp_decoder,
            rtp_decoder_tests,
            rtp_encoder,
            rtp_sup            
            ]},
 {registered,[]},
 {applications, [kernel,stdlib]},
 {mod, {rtp,[]}}
]}.
