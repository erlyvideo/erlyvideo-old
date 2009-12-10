
-define(NAL_SINGLE, 1).
-define(NAL_SLICE_A, 2).
-define(NAL_SLICE_B, 3).
-define(NAL_SLICE_C, 4).
-define(NAL_IDR, 5).
-define(NAL_SPS, 7).
-define(NAL_PPS, 8).
-define(NAL_DELIM, 9).
-define(NAL_STAP_A, 24).
-define(NAL_STAP_B, 25).
-define(NAL_MTAP16, 26).
-define(NAL_MTAP24, 27).
-define(NAL_FUA, 28).
-define(NAL_FUB, 29).


-record(h264, {
  profile,
  profile_compat = 0,
  level,
  sps,
  pps,
  dump_file,
  buffer = <<>>
}).
