-module(trusted_login).
-author(max@maxidoors.ru).

-include_lib("erlyvideo/include/rtmp_session.hrl").
-export([client_login/2]).

client_login(#rtmp_session{host = Host} = State, [_SessionData, UserIdF]) ->
  UserId = round(UserIdF),
  {ok, SessionId} = ems_users:login(Host, UserId, []),
	State#rtmp_session{user_id = UserId, session_id = SessionId};
	
client_login(State, _) ->
  State#rtmp_session{user_id = undefined}.
