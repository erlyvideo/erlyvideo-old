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
  audio_codec = ?FLV_AUDIO_FORMAT_AAC,
  video_codec = ?FLV_VIDEO_CODEC_AVC,
  video_decoder_config,
  audio_decoder_config,
	frames = undefined,
	owner = undefined,
	clients = undefined,
	base_timestamp = undefined, % Timestamp of first frame
	gop = []
}).
