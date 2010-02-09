-module(fitc_demo).
-author(max@maxidoors.ru).

-include_lib("erlyvideo/include/rtmp_session.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-export([connect/2]).

-export([getStreams/2, publish/2]).

-export(['WAIT_FOR_DATA'/2]).

'WAIT_FOR_DATA'({newStreamRegistered, StreamName}, #rtmp_session{socket = Socket} = State) ->
  io:format("New stream ~p~n", [StreamName]),
  rtmp_socket:invoke(Socket, #rtmp_funcall{command = 'newStream', args = [null, StreamName]}),
  {next_state, 'WAIT_FOR_DATA', State};


'WAIT_FOR_DATA'(_Message, #rtmp_session{} =_State) -> {unhandled}.


connect(#rtmp_session{host = Host, addr = Address, socket = Socket, player_info = PlayerInfo} = State, AMF) ->
  UserId = random:uniform(10000000),
  Channels = [10],
  {ok, SessionId} = ems_users:login(Host, UserId, Channels),
	NewState = State#rtmp_session{session_id = SessionId},

	ems_log:access(Host, "CONNECT ~p ~s ~p ~s ~p", [Address, Host, UserId, proplists:get_value(pageUrl, PlayerInfo), self()]),
  rtmp_session:accept_connection(NewState, AMF),
  rtmp_socket:invoke(Socket, #rtmp_funcall{command = 'setId', args = [null, UserId]}),
  NewState.
	
	
getStreams(#rtmp_session{host = Host} = State, AMF) ->
  Streams = [Name || {Name, _} <- media_provider:entries(Host)],
  io:format("getStreams() -> ~p~n", [Streams]),
  rtmp_session:reply(State,AMF#rtmp_funcall{args = [null, Streams]}),
  State.

publish(#rtmp_session{host = Host} = State, #rtmp_funcall{args = [null,URL,_]} = AMF) ->
  io:format("Publish videoConf ~p~n", [URL]),
  State1 = apps_recording:publish(State, AMF),
  {ok, Clients} = ems_users:clients(Host),
  io:format("Notifying ~p~n", [Clients]),
  [gen_fsm:send_event(Client, {newStreamRegistered, URL}) || Client <- Clients, Client /= self()],
  State1.
  
  
