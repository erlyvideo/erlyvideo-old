% -record(payload, {
%           num,
%           codec,
%           clock_map,
%           ms      :: undefined | mono | stereo,
%           ptime   :: integer(),
%           config = []
%          }).
% 
-type(sdp_attr() :: atom() | {atom(), string() | binary()}).
% 
% -record(media_desc, {
%   type,
%   connect,
%   port,
%   payloads       = []    :: [#payload{}],
%   track_control,
%   pps,
%   sps,
%   config,
%   attrs          = []   :: [sdp_attr()]
% }).

-record(sdp_session, {
  version = 0,
  originator,
  name,
  connect,
  time     = {0, 0} :: {integer(), integer()},
  attrs    = []   :: [sdp_attr()]
}).

-record(sdp_o, {
  username = <<"-">>,
  sessionid,
  version,
  netaddrtype = inet4  :: inet4 | inet6,
  address :: string()
}).
