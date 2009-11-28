-module(trusted_login).
-author(max@maxidoors.ru).

-include("../../include/ems.hrl").
-export([client_login/2]).

client_login(State, [_SessionData, {number, UserId}]) ->
  ?D({"Untrusted session", UserId}),
  {ok, SessionId} = rtmp_server:login(UserId, []),
	State#rtmp_client{user_id = UserId, session_id = SessionId};
	
client_login(State, _) ->
  ?D({"Fully untrusted session"}),
  State#rtmp_client{user_id = undefined}.
