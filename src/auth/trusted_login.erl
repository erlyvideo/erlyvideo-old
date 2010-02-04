-module(trusted_login).
-author(max@maxidoors.ru).

-include_lib("erlyvideo/include/rtmp_session.hrl").
-export([client_login/2]).

client_login(#rtmp_session{host = Host} = State, [SessionData, UserIdF]) ->
  UserId = round(UserIdF),
  Channels = case (catch mochijson2:decode(SessionData)) of
    Chan when is_list(Chan) -> Chan;
    _ -> []
  end,
  {ok, SessionId} = ems_users:login(Host, UserId, Channels),
	State#rtmp_session{user_id = UserId, session_id = SessionId};
	
client_login(State, _) ->
  State#rtmp_session{user_id = undefined}.
