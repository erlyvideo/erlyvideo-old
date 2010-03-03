
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


-record(flv_header,{
	version = 1,
	audio = 0,
	video = 0
}).
