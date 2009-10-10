-record(video_recorder, {
  type,
  file_name,
  timer_ref,
  buffer,
  ts_pos,
  ts_prev = 0,
  pos = 0,
  device
}).
