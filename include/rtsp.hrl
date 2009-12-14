

-record(rtsp_stream, {
  id,
  type,
  transport,
  payload_type,
  clock_map,
  pps,
  sps,
  config
}).