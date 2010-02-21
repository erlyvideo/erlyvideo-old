-module(ems_rtsp).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("h264/include/h264.hrl").
-include_lib("erlyvideo/include/rtmp_session.hrl").
-include_lib("erlyvideo/include/video_frame.hrl").
-include("../include/ems.hrl").
-include_lib("ertsp/include/rtsp.hrl").

-export([announce/2]).

announce(Hostname, Path) -> 
  Host = ems:host(Hostname),
  ?D({"ANNOUNCE", Host, Host}),
  ems_log:access(Host, "RTSP ANNOUNCE ~s ~s", [Host, Path]),
  Media = media_provider:open(Host, Path, live),
  {ok, Media}.


