-module(apps_push).
-author('max@maxidoors.ru').
-include("../../include/ems.hrl").

-export(['WAIT_FOR_DATA'/2]).


'WAIT_FOR_DATA'({message, Message}, #rtmp_session{socket = Socket} = State) ->
  % ?D({"NetConnection.Message", Message}),
  rtmp_socket:status(Socket, 0, <<"NetConnection.Message">>, Message),
  {next_state, 'WAIT_FOR_DATA', State};



'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.
