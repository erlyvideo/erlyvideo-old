-module(apps_push).
-author('max@maxidoors.ru').
-include("../../include/ems.hrl").

-export(['WAIT_FOR_DATA'/2]).


'WAIT_FOR_DATA'({message, Message}, State) ->
  gen_fsm:send_event(self(), {status, "NetConnection.Message", 0, Message}),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};



'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.
