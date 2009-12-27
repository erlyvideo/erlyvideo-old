-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-define(TIMEOUT, 1000).

-record(rtsp_stream, {
  id,
  type,
  transport,
  payload_type,
  clock_map,
  pps,
  sps,
  config
}).

-record(video_frame,{
  decoder_config = false,
  raw_body       = false,
	type           = undefined,
	timestamp      = undefined,
	timestamp_ext  = undefined,
	stream_id      = 0,
	body           = <<>>,
	codec_id 	     = undefined,
	frame_type     = undefined,
	sound_type	   = undefined,
	sound_size	   = undefined,
	sound_rate	   = undefined,
	sound_format	 = undefined
	}).
	
