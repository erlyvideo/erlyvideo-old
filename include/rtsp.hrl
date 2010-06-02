-define(TIMEOUT, 1000).

-record(rtsp_stream, {
  type,
  clock_map,
  track_control,
  codec,
  pps,
  sps,
  config
}).
