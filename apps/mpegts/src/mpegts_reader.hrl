-define(MAX_PAYLOAD, 16#100000000).

-record(decoder, {
  buffer = <<>>,
  pids = [],
  consumer,
  pmt_pid,
  socket,
  options,
  byte_counter = 0,
  program_info = [],
  sdt,
  current_time
}).

-record(mpegts_pat, {
  descriptors
}).

-record(stream, {
  pid,
  program_num,
  demuxer,
  handler,
  codec,
  ts_buffer = undefined,
  es_buffer = <<>>,
  counter = 0,
  payload_size = ?MAX_PAYLOAD,
  pcr,
  start_dts,
  dts,
  pts,
  video_config = undefined,
  send_audio_config = false,
  sample_rate,
  h264
}).
