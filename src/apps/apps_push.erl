-module(apps_push).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../../include/ems.hrl").
-include_lib("erlyvideo/include/rtmp_session.hrl").

-export(['WAIT_FOR_DATA'/2]).

-export([sendMessage/2]).

'WAIT_FOR_DATA'({message, Message}, #rtmp_session{socket = Socket} = State) ->
  % io:format("NetConnection.Message ~s~n", [Message]),
  rtmp_socket:status(Socket, 0, <<"NetConnection.Message">>, Message),
  {next_state, 'WAIT_FOR_DATA', State};



'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.


sendMessage(#rtmp_session{host = Host} = State, #rtmp_funcall{args = [null, ChannelId, Message]}) -> 
  Channel = round(ChannelId),
  ems_users:send_to_channel(Host, Channel, Message),
  ems_log:access(Host, "MESSAGE ~p ~p ~p ~p~n", [Host, State#rtmp_session.addr, Channel, Message]),
  State.
