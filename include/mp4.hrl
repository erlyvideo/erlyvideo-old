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
  keyframes = [],           % stss Sample Table Sync Samples
  key_offset = 0,
  chunk_offsets = [],       % stco Sample Table Chunk Offsets
  composition_offsets = [], % ctts Composition offsets for B-frames
  frames
}).

-record(mp4_sample_description, {

}).

-define(MP4ESDescrTag, 3).
-define(MP4DecConfigDescrTag, 4).
-define(MP4DecSpecificDescrTag, 5).
