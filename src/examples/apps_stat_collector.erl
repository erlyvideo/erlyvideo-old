-module(apps_stat_collector).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../../include/ems.hrl").
-include("../../include/rtmp_session.hrl").

-export([entries/2]).

entries(#rtmp_session{host = Host} = State, #rtmp_funcall{} = AMF) -> 
  Entries = erlyvideo:stats(Host),
  rtmp_session:reply(State, AMF#rtmp_funcall{args = [null, Entries]}),
  State.

  