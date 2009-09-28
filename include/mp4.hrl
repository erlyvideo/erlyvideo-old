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
  sample_sizes = [],        % stsz Sample sizes
  sample_durations = [],    % stts Durations
  chunk_table = [],         % stsc Chunk info
  keyframes = [],           % stss  Sample Table Sync Samples
  key_offset = 0,
  chunk_offsets = [],       % stco  Sample Table Chunk Offsets
  frames
}).

-record(mp4_sample_description, {

}).

-record(mp4_frame, {
  id,
  timestamp,
  type,
  offset,
  size,
  keyframe
}).