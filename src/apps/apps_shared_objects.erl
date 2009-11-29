-module(apps_shared_objects).
-author('max@maxidoors.ru').
-include("../../include/ems.hrl").

-export(['WAIT_FOR_DATA'/2]).
-export([command/2]).
-export([getServiceList/2]).




'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.


command(Command, State) ->
  ?D({"SO command", Command}),
  State.


getServiceList(_AMF, State) -> 
  ?D({"getServiceList", _AMF#amf.args, _AMF}),
  % gen_fsm:send_event(self(), {publish, record, Name}),
  State.
