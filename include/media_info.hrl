-define(FILE_CACHE_TIME, 10000).

-record(media_info, {
  type = undefined,
  device = undefined,
  header = undefined,
  file_name = undefined,
  format,
  width = undefined,
  height = undefined,
  duration = undefined,
  timescale,
  seconds,
  video_decoder_config,
  audio_decoder_config,
	frames = undefined,
	owner = undefined,
	clients = undefined,
	base_timestamp = undefined % Timestamp of first frame
}).
