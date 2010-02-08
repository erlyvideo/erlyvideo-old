-module(fitc_demo).
-author(max@maxidoors.ru).

-include_lib("erlyvideo/include/rtmp_session.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-export([client_login/2]).

client_login(#rtmp_session{host = Host, socket = Socket} = State, _) ->
  UserId = random:uniform(10000000),
  Channels = [10],
  {ok, SessionId} = ems_users:login(Host, UserId, Channels),
  rtmp_socket:invoke(Socket, #rtmp_funcall{command = 'setId', args = [null, UserId]}),
	State#rtmp_session{session_id = SessionId};
	
client_login(State, _) ->
  State#rtmp_session{user_id = undefined}.
