-module(ems_rtmp).
-author(max@maxidoors.ru).

-export([create_client/1]).


create_client(Socket) ->
  {ok, Pid} = ems_sup:start_rtmp_session(),
  rtmp_session:set_socket(Pid, Socket),
  {ok, Pid}.
