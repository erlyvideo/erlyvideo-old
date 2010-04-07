-module(json_session).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("rtmp/include/rtmp.hrl").
-include("../../include/rtmp_session.hrl").
-export([decode/2, encode/2, connect/2, binary_to_hexbin/1]).



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
  Session = decode(Cookie, Secret),
  UserId = proplists:get_value(user_id, Session),
  Channels = proplists:get_value(channels, Session, []),
  {ok, SessionId} = ems_users:login(Host, UserId, Channels),
	NewState = State#rtmp_session{user_id = UserId, session_id = SessionId},
	ems_log:access(Host, "CONNECT ~s ~s ~p ~s ~w json_session", [Address, Host, UserId, proplists:get_value(pageUrl, PlayerInfo), Channels]),
	NewState.
  
decode(undefined, undefined) ->
  [];

decode(Cookie, Secret) when is_list(Cookie) ->
  decode(list_to_binary(Cookie), Secret);
  
decode(Cookie, Secret) when is_binary(Cookie) ->
  {Session64, Sign} = split_cookie(Cookie),
  verify_signature(Session64, Sign, Secret),
  {struct, Session} = mochijson2:decode(base64:decode(Session64)),
  Session.
  

  
encode(Session, undefined) ->
  Json = iolist_to_binary(mochijson2:encode({struct, Session})),
  base64:encode(Json);
  
  
encode(Session, Secret) when is_list(Session) ->
  Json64 = encode(Session, undefined),
  Sign = session_sign(Json64, Secret),
  <<Json64/binary, "--", Sign/binary>>.
  
binary_to_hexbin(L) ->
  list_to_binary(lists:flatten(lists:map(fun(X) -> int_to_hex(X) end, binary_to_list(L)))).

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
  $0+N;
hex(N) when N >= 10, N < 16 ->
  $a + (N-10).
  
session_sign(Session, Secret) ->
  binary_to_hexbin(crypto:sha_mac(Secret, Session)).
  
  
verify_signature(_, _, undefined) ->
  ok;
  
verify_signature(Session64, Sign, Secret) ->
  Sign = session_sign(Session64, Secret).

split_cookie(Cookie) -> split_cookie(0, Cookie).

split_cookie(Offset, Cookie) when Offset >= size(Cookie) - 2 ->
  {Cookie, undefined};
  
split_cookie(Offset, Cookie) ->
  case Cookie of
    <<Session64:Offset/binary, "--", Sign/binary>> ->
      {Session64, Sign};
    _ ->
      split_cookie(Offset + 1, Cookie)
  end.
  

