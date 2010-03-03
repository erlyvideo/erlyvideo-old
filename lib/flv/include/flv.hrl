
-record(flv_tag, {
  type,
  frame_type,  % keyframe or frame
  timestamp,
  composition_offset,
  size,
  offset,
  next_tag_offset,
  body
}).

-record(flv_audio_tag, {
  codec,
  rate,
  bitsize,
  channels,
  decoder_config,
  body
}).

-record(flv_video_tag, {
  codec,
  frame_type,
  composition_time,
  decoder_config,
  body
}).


-record(flv_header,{
	version = 1,
	audio = 0,
	video = 0
}).
