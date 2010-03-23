-record(mp4_header, {
  file,
  file_type,
  timescale,
  duration,
  seconds,
  file_types = [],
  tracks = [],
  frames
}).

-record(mp4_track, {
  data_format,
  track_id,
  timescale,
  duration,
  width,
  height,
  decoder_config,
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
  offset
}).

-record(mp4_sample_description, {

}).

-define(MP4ESDescrTag, 3).
-define(MP4DecConfigDescrTag, 4).
-define(MP4DecSpecificDescrTag, 5).
-define(MP4Unknown6Tag, 6).
