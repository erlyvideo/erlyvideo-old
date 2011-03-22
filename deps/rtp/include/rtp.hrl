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


-record(rtp_udp, {
  server_rtp_port,
  client_rtp_port,
  rtp_socket,
  server_rtcp_port,
  client_rtcp_port,
  rtcp_socket,
  source
}).

-record(rtp_channel, {
  sequence = undefined,
  wall_clock = undefined,
  timecode = undefined,
  stream_id,
  payload_type,
  timescale,
  codec,
  buffer,
  stream_info,
  length_size, % H264 stuff: how long is NAL size
  last_sr  % NTP Time then last sender report was received
}).

