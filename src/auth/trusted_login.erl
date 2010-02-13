-module(trusted_login).
-author(max@maxidoors.ru).

-include_lib("erlyvideo/include/rtmp_session.hrl").
-include_lib("rtmp/include/rtmp.hrl").

-export([connect/2]).

connect(#rtmp_session{host = Host, addr = Address, player_info = PlayerInfo} = State, #rtmp_funcall{args = [_, SessionData, UserIdF]} = AMF) ->
  UserId = round(UserIdF),
  Channels = case (catch mochijson2:decode(SessionData)) of
    Chan when is_list(Chan) -> Chan;
    _ -> []
  end,
  {ok, SessionId} = ems_users:login(Host, UserId, Channels),
	NewState = State#rtmp_session{user_id = UserId, session_id = SessionId},
	ems_log:access(Host, "CONNECT ~s ~s ~p ~s", [Address, Host, UserId, proplists:get_value(pageUrl, PlayerInfo)]),
	rtmp_session:accept_connection(NewState, AMF),
  NewState;
  
	
connect(#rtmp_session{host = Host, addr = Address, player_info = PlayerInfo} = State, AMF) ->
  ems_log:access(Host, "CONNECT ~s ~s ~p ~s", [Address, Host, undefined, proplists:get_value(pageUrl, PlayerInfo)]),
	rtmp_session:accept_connection(State, AMF),
  State.
	