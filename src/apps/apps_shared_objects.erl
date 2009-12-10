-module(apps_shared_objects).
-author('max@maxidoors.ru').
-include("../../include/ems.hrl").
-include("../../include/shared_objects.hrl").

-export(['WAIT_FOR_DATA'/2]).
-export([command/2]).
-export([getServiceList/2]).




'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.

command(#so_message{name = Name, persistent = Persistent} = Message, State) ->
  Object = shared_objects:open(Name, Persistent),
  shared_object:message(Object, Message),
  ?D({"SO command", Object, Message}),
  State.


getServiceList(AMF, State) -> 
  apps_rtmp:reply(AMF#amf{args = [null, [<<"hello">>, <<"setData">>, <<"getData">>]]}),
  % gen_fsm:send_event(self(), {publish, record, Name}),
  State.

