-record(frame_id, {
  id,
  a,
  v,
  t
}).

-record(mp4_media, {
  file,
  file_type,
  timescale,
  duration,
  file_types = [],
  tracks = [],
  index,
  width,
  height,
  frames,
  additional = [],
  reader,
  options
}).

-record(mp4_track, {
  data_format,
  content,
  track_id,
  timescale,
  duration,
  width,
  height,
  decoder_config,
  max_bitrate,
  bitrate,
  language,
  frames,
  sample_sizes = [],
  sample_dts = [],
  sample_offsets = [],
  sample_composition = [],
  keyframes = [],
  chunk_offsets = [],
  chunk_sizes = []
}).

-record(mp4_frame, {
  id,
  dts,
  size,
  pts,
  keyframe = false,
  offset,
  codec,
  content,
  body,
  next_id
}).

-record(mp4_sample_description, {

}).

-define(MP4ESDescrTag, 3).
-define(MP4DecConfigDescrTag, 4).
-define(MP4DecSpecificDescrTag, 5).
-define(MP4Unknown6Tag, 6).
