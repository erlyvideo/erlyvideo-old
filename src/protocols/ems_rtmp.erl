-module(ems_rtmp).
-author(max@maxidoors.ru).

-export([create_client/0, create_client/1]).


create_client() ->
  ems_sup:start_rtmp_session().

create_client(Socket) ->
  {ok, Pid} = ems_sup:start_rtmp_session(),
  gen_tcp:controlling_process(Socket, Pid),
  %% Instruct the new FSM that it owns the socket.
  rtmp_session:set_socket(Pid, Socket),
  {ok, Pid}.
