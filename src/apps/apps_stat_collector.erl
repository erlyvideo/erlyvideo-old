-module(apps_stat_collector).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../log.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-include("../../include/erlyvideo.hrl").
-include("../../include/rtmp_session.hrl").

-export([subscribeStats/2, entries/2, handle_event/2, handle_info/2, init/1]).

entries(#rtmp_session{host = Host} = State, #rtmp_funcall{} = AMF) -> 
  Entries = erlyvideo:stats(Host),
  ?D({updatingInfo}),
  rtmp_session:reply(State, AMF#rtmp_funcall{args = [null, Entries]}),
  State.


subscribeStats(#rtmp_session{socket = Socket} = State, _AMF) ->
  ems_event:add_sup_handler(?MODULE, [Socket]),
  State.
  
init([Socket]) ->
  {ok, Socket}.
  
  
handle_event(#erlyvideo_event{} = Event, Socket) ->
  XML = ems_event:to_xml(Event),
  rtmp_socket:status(Socket, 0, <<"NetConnection.Message">>, XML),
  {ok, Socket}.


handle_info(_, Socket) ->
  {ok, Socket}.
