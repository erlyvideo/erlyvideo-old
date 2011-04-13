{application, erlmedia,
 [
  {description, "Erlmedia"},
  {vsn, "1.8.6"},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {modules, [
             aac,
             asf,
             ems_log,
             flv,
             flv_reader,
             flv_video_frame,
             flv_writer,
             gen_format,
             gen_listener,
             h264,
             http_stream,
             http_uri2,
             mjpeg_reader,
             mkv,
             mp3,
             mp3_reader,
             mp4,
             mp4_reader,
             mp4_writer,
             packet_codec,
             sdp,
             sdp_encoder,
             sdp_tests,
             shoutcast_reader,
             video_frame,
             wav
            ]},
  {env, []}
 ]}.
