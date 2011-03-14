
-type(stream_decoder_config() :: any()).
-type(stream_video_params() :: {non_neg_integer()}).

-record(video_params, {
  width  = 0 ::non_neg_integer(),
  height = 0 ::non_neg_integer(),
  fps = 0 ::non_neg_integer()
}).

-type(video_params() :: #video_params{}).

-record(sound_params, {
  channels    = 0 ::non_neg_integer(),
  sample_rate = 0 ::non_neg_integer()
}).

-type(sound_params() :: #sound_params{}).

-record(stream_info, {
  content        = undefined ::frame_content(),
	stream_id      = 0         ::non_neg_integer(),
  codec 	       = undefined ::frame_codec()|undefined,
  config         = undefined ::stream_decoder_config(),
  params         = undefined ::sound_params()|video_params(),
  clock_map      = 1000      ::non_neg_integer() % How many DTS units are in one second. Erlyvideo uses milliseconds everywhere
}).

-type(stream_info() :: #stream_info{}).

-record(media_info, {
  audio = [] :: [stream_info()],
  video = [] :: [stream_info()],
  metadata = [] :: [stream_info()],
  duration = undefined :: non_neg_integer()|undefined,
  options = [] :: [any()]
}).
