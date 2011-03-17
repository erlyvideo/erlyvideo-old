
-type(stream_decoder_config() :: any()).
-type(stream_video_params() :: {non_neg_integer()}).

-record(video_params, {
  width  = 0 ::non_neg_integer(),
  height = 0 ::non_neg_integer(),
  fps = 0 ::non_neg_integer()
}).

-type(video_params() :: #video_params{}).

-record(audio_params, {
  channels    = 0 ::non_neg_integer(),
  sample_rate = 0 ::non_neg_integer()
}).

-type(audio_params() :: #audio_params{}).

-record(stream_info, {
  content        = undefined ::frame_content(),
	stream_id      = 0         ::non_neg_integer(),
  codec 	       = undefined ::frame_codec()|undefined,
  config         = undefined ::stream_decoder_config(),
  bitrate        = undefined ::non_neg_integer(),
  language       = undefined ::string()|undefined,
  params         = undefined ::audio_params()|video_params(),
  timescale      = 1         ::non_neg_integer(), % How many DTS units are in one millisecond. Erlyvideo uses milliseconds everywhere
  options        = []        ::any()
}).

-type(stream_info() :: #stream_info{}).

-type(flow_type() :: file|stream).

-record(media_info, {
  flow_type  = undefined ::flow_type(),
  audio      = [] :: [stream_info()],
  video      = [] :: [stream_info()],
  metadata   = [] :: [stream_info()],
  duration   = undefined :: non_neg_integer()|undefined,
  options    = [] :: [any()]
}).
