-type(frame_content() ::audio|video|metadata).
-type(frame_flavor()  ::audio|video|metadata).
-type(frame_video_codec() ::h264|sorensen|vp6|vp6f).
-type(frame_audio_codec() ::aac|mp3|speex|nelly_moser|nelly_moser8).
-type(frame_codec()   ::frame_video_codec()|frame_audio_codec()|atom()).

-type(frame_sound_type() ::mono|stereo).
-type(frame_sound_size() ::bit8|bit16).
-type(frame_sound_rate() ::rate5|rate11|rate22|rate44).
-type(frame_sound() ::{frame_sound_type(), frame_sound_size(), frame_sound_rate()}).

-record(video_frame,{
	content        = undefined ::frame_content(),
	dts            = undefined ::non_neg_number(),
	pts            = undefined ::non_neg_number(),
	stream_id      = 0         ::non_neg_integer(),
	codec 	       = undefined ::frame_codec()|undefined,
	flavor         = undefined ::frame_flavor(),
	sound          = {undefined, undefined, undefined} ::frame_sound(),
	body           = <<>>      ::binary(),
	next_id        = undefined ::any()
}).
	
