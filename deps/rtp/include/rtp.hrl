-define(RTCP_SR, 200).
-define(RTCP_RR, 201).
-define(RTCP_SD, 202).
-define(YEARS_70, 2208988800).  % RTP bases its timestamp on NTP. NTP counts from 1900. Shift it to 1970. This constant is not precise.

-define(SDES_CNAME, 1).
-define(SDES_NAME, 2).
-define(SDES_EMAIL, 3).
-define(SDES_PHONE, 4).
-define(SDES_LOC, 5).
-define(SDES_TOOL, 6).
-define(SDES_NOTE, 7).
-define(SDES_PRIV, 8).


-record(base_rtp, {
  media,
  clock_map,
  base_timecode = undefined,
  sequence = undefined,
  wall_clock = undefined,
  base_wall_clock = undefined,
  timecode = undefined,
  synced = false,
  stream_id,
  last_sr,
  codec,
  marker  = false     :: undefined | true | false,
  framelens    :: integer(),                    % Size of frame length in bytes
  packets = 0  :: integer(),
  bytes   = 0  :: integer()
}).



-record(rtp_state, {
  sequence = undefined,
  wall_clock = undefined,
  timecode = undefined,
  timescale,
  codec,
  buffer,
  stream_info,
  marker = false,
  last_sr  % NTP Time then last sender report was received
}).

