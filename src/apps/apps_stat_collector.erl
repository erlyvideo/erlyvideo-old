-module(apps_stat_collector).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../log.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-include("../../include/erlyvideo.hrl").
-include("../../include/rtmp_session.hrl").

-export([subscribeStats/2, entries/2, handle_event/2, handle_info/2, init/1]).

entries(#rtmp_session{host = Host} = State, #rtmp_funcall{} = AMF) -> 
  Entries = erlyvideo:stats(Host),
  rtmp_session:reply(State, AMF#rtmp_funcall{args = [null, Entries]}),
  State.


subscribeStats(#rtmp_session{} = State, _AMF) ->
  ems_event:add_sup_handler(?MODULE, [self()]),
  timer:send_interval(2000, event_happened),
  State.
  
init([Session]) ->
  {ok, Session}.
  
  
handle_event(#erlyvideo_event{} = _Event, Session) ->
  % Session ! event_happened,
  % XML = ems_event:to_xml(Event),
  % rtmp_socket:status(Socket, 0, <<"NetConnection.Message">>, XML),
  {ok, Session}.

handle_info(event_happened, #rtmp_session{socket = Socket} = Session) ->
  flush_events(),
  rtmp_socket:status(Socket, 0, <<"NetConnection.Message">>, <<"<events></events>">>),
  Session;

handle_info(_, #rtmp_session{}) ->
  unhandled;

handle_info(_, Socket) ->
  {ok, Socket}.


flush_events() ->
  receive
    event_happened -> flush_events()
  after
    0 -> ok
  end.
