-module(protected_play).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("rtmp/include/rtmp.hrl").
-include("erlyvideo/include/rtmp_session.hrl").


-export([connect/2, play/2]).


connect(#rtmp_session{host = Host, addr = Address, player_info = PlayerInfo} = State, AMF) ->
  try perform_login(State, AMF) of
    NewState -> 
      rtmp_session:accept_connection(NewState),
      NewState
  catch
    _:_ ->
    	ems_log:access(Host, "REJECT ~s ~s ~p ~s json_session", [Address, Host, undefined, proplists:get_value(pageUrl, PlayerInfo)]),
      rtmp_session:reject_connection(State),
      State
  end.


perform_login(#rtmp_session{host = Host, addr = Address, player_info = PlayerInfo} = State, #rtmp_funcall{args = [_, Cookie | _]}) ->
  Secret = ems:get_var(secret_key, Host, undefined),
  Session = json_session:decode(Cookie, Secret),
  UserId = proplists:get_value(user_id, Session),
  Channels = proplists:get_value(channels, Session, []),
  PermitedUrl = proplists:get_value(url, Session),
  {ok, SessionId} = ems_users:login(Host, UserId, Channels),
	NewState = State#rtmp_session{user_id = UserId, session_id = SessionId, plugin_data = PermitedUrl},
	ems_log:access(Host, "CONNECT ~s ~s ~p ~s ~w ~s ~s", [Address, Host, UserId, proplists:get_value(pageUrl, PlayerInfo), Channels, PermitedUrl, ?MODULE]),
	NewState.


play(#rtmp_session{host = Host, streams = Streams, plugin_data = Name} = State, #rtmp_funcall{args = [null, Name | Args], stream_id = StreamId}) ->
  Stream = ems:element(StreamId, Streams),

  Options = apps_streaming:extract_play_args(Args),
  Stream ! {play, Name, Options},
  ems_log:access(Host, "PLAY ~s ~p ~s ~p", [State#rtmp_session.addr, State#rtmp_session.user_id, Name, StreamId]),  
  % gen_fsm:send_event(self(), {play, Name, Options}),
  State.
