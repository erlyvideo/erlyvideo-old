-module(fitc_demo).
-author(max@maxidoors.ru).

-include_lib("erlyvideo/include/rtmp_session.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-export([client_login/2]).

-export([getStreams/2, publish/2]).

-export(['WAIT_FOR_DATA'/2]).

'WAIT_FOR_DATA'({newStreamRegistered, StreamName}, #rtmp_session{socket = Socket} = State) ->
  io:format("New stream ~p~n", [StreamName]),
  rtmp_socket:invoke(Socket, #rtmp_funcall{command = 'newStream', args = [null, StreamName]}),
  {next_state, 'WAIT_FOR_DATA', State};


'WAIT_FOR_DATA'(_Message, #rtmp_session{} =_State) -> {unhandled}.


client_login(#rtmp_session{host = Host, socket = Socket} = State, _) ->
  UserId = random:uniform(10000000),
  Channels = [10],
  {ok, SessionId} = ems_users:login(Host, UserId, Channels),
  rtmp_socket:invoke(Socket, #rtmp_funcall{command = 'setId', args = [null, UserId]}),
	State#rtmp_session{session_id = SessionId}.
	
getStreams(#rtmp_session{host = Host} = State, AMF) ->
  Streams = [Name || {Name, _} <- media_provider:entries(Host)],
  io:format("getStreams() -> ~p~n", [Streams]),
  apps_rtmp:reply(State,AMF#rtmp_funcall{args = [null, Streams]}),
  State.

publish(#rtmp_session{host = Host} = State, #rtmp_funcall{args = [null,URL,_]} = AMF) ->
  io:format("Publish videoConf ~p~n", [URL]),
  State1 = apps_recording:publish(State, AMF),
  {ok, Clients} = ems_users:clients(Host),
  io:format("Notifying ~p~n", [Clients]),
  [gen_fsm:send_event(Client, {newStreamRegistered, URL}) || Client <- Clients, Client /= self()],
  State1.
  
  
