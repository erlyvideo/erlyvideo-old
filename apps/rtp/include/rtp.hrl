-define(RTCP_SR, 200).
-define(RTCP_RR, 201).
-define(RTCP_SD, 202).
-define(YEARS_70, 2208988800).  % RTP bases its timestamp on NTP. NTP counts from 1900. Shift it to 1970. This constant is not precise.
-define(YEARS_100, 3155673600).  % RTP bases its timestamp on NTP. NTP counts from 1900. Shift it to 1970. This constant is not precise.

-define(SDES_CNAME, 1).
-define(SDES_NAME, 2).
-define(SDES_EMAIL, 3).
-define(SDES_PHONE, 4).
-define(SDES_LOC, 5).
-define(SDES_TOOL, 6).
-define(SDES_NOTE, 7).
-define(SDES_PRIV, 8).


-record(rtp_udp, {
  local_rtp_port,
  remote_rtp_port,
  rtp_socket,
  local_rtcp_port,
  remote_rtcp_port,
  rtcp_socket,

  local_addr,
  remote_addr
}).

-record(rtcp, {
  ntp,
  stream_id,
  timecode,
  packet_count,
  octet_count
}).

-record(rtp_state, {
  streams = [],
  channels = {undefined, undefined},
  transport,
  ports,
  tcp_socket,
  udp = {undefined, undefined},
  content_map,
  location,
  sent_audio_config = false,
  frames = [],
  reorder_length = 0
}).

-record(rtp_channel, {
  sequence = undefined,
  wall_clock = undefined,
  timecode = undefined,
  octet_count = 0,
  packet_count = 0,
  stream_id,
  payload_type,
  timescale,
  codec,
  buffer,
  stream_info,
  length_size, % H264 stuff: how long is NAL size
  last_sr  % NTP Time then last sender report was received
}).

