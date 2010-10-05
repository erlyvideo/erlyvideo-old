-record(media_desc, {
  type,
  connect,
  port,
  payload,
  clock_map,
  track_control,
  codec,
  pps,
  sps,
  config
}).

-record(session_desc, {
  version = <<"0">>,
  originator,
  name,
  connect
}).

-record(sdp_o, {
  username = <<"-">>,
  sessionid,
  version,
  nettype = <<"IN">>,
  addrtype = inet4  :: inet4 | inet6,
  address :: string()
}).
