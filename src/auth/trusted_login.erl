-module(trusted_login).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("erlyvideo/include/rtmp_session.hrl").
-include_lib("rtmp/include/rtmp.hrl").

-export([connect/2]).

connect(#rtmp_session{host = Host, addr = Address, player_info = PlayerInfo} = State, #rtmp_funcall{args = [_, SessionData, UserIdF]} = AMF) ->
  UserId = round(UserIdF),
  Session = json_session:decode(SessionData, undefined),
  Channels = proplists:get_value(channels, Session, []),
  {ok, SessionId} = ems_users:login(Host, UserId, Channels),
	NewState = State#rtmp_session{user_id = UserId, session_id = SessionId},
	ems_log:access(Host, "CONNECT ~s ~s ~p ~s ~w trusted_login", [Address, Host, UserId, proplists:get_value(pageUrl, PlayerInfo), Channels]),
	rtmp_session:accept_connection(NewState, AMF),
  NewState;
  
	
connect(#rtmp_session{host = Host, addr = Address, player_info = PlayerInfo} = State, AMF) ->
  ems_log:access(Host, "CONNECT ~s ~s ~p ~s ~p trusted_login", [Address, Host, undefined, proplists:get_value(pageUrl, PlayerInfo), []]),
	rtmp_session:accept_connection(State, AMF),
  State.
	