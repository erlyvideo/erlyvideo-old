-define(FILE_CACHE_TIME, 10000).

-record(media_info, {
  host,
  type = undefined,
  device = undefined,
  header = undefined,
  name = undefined,
  path = undefined,
  format,
  width = undefined,
  height = undefined,
  duration = undefined,
  timescale,
  seconds,
  audio_codec = aac,
  video_codec = avc,
  video_decoder_config,
  audio_decoder_config,
	frames = undefined,
	owner = undefined,
	clients = undefined,
	base_timestamp = undefined, % Timestamp of first frame
	gop = []
}).
