-module(apps_shared_objects).
-author('max@maxidoors.ru').
-include("../../include/ems.hrl").
-include("../../include/shared_objects.hrl").

-export(['WAIT_FOR_DATA'/2]).
-export([command/2]).
-export([getServiceList/2]).




'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.

command({{Name, Version, Persistent}, ?SO_CONNECT, _}, State) ->
  Object = shared_objects:connect(Name, Version, Persistent),
  ?D({"Connecting to object", Object}),
  State;

command(Command, State) ->
  ?D({"SO command", Command}),
  State.


getServiceList(_AMF, State) -> 
  ?D({"getServiceList", _AMF#amf.args, _AMF}),
  % gen_fsm:send_event(self(), {publish, record, Name}),
  State.

