-module(trusted_login).
-author(max@maxidoors.ru).

-include("../../include/ems.hrl").
-export([client_login/2]).

client_login(State, [_SessionData, UserIdF]) ->
  UserId = round(UserIdF),
  {ok, SessionId} = ems_users:login(UserId, []),
	State#rtmp_session{user_id = UserId, session_id = SessionId};
	
client_login(State, _) ->
  ?D({"Fully untrusted session"}),
  State#rtmp_session{user_id = undefined}.
