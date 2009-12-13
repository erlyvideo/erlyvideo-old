-module(json_session).
-author(max@maxidoors.ru).
-include("../../include/ems.hrl").
-export([decode/2, encode/2, client_login/2, binary_to_hexbin/1]).



client_login(#rtmp_session{host = Host} = State, [Cookie, _UserIdObj]) ->
  Secret = ems:get_var(secret_key, Host, undefined),
  Session = decode(Cookie, Secret),
  ?D({"Authed session:", Session}),
  UserId = proplists:get_value(user_id, Session),
  Channels = proplists:get_value(channels, Session, []),
  {ok, SessionId} = rtmp_listener:login(UserId, Channels),
	State#rtmp_session{user_id = UserId, session_id = SessionId};

client_login(_, _) ->
  throw(login_failed).


decode(Session, Secret) when is_binary(Session) ->
  decode(0, Session, Secret);

decode(Session, Secret) when is_list(Session) ->
  decode(0, list_to_binary(Session), Secret).
  
encode(Session, Secret) when is_list(Session) ->
  Json = iolist_to_binary(mochijson2:encode({struct, Session})),
  Json64 = base64:encode(Json),
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
  
decode(Offset, Subscription, _Secret) when Offset >= size(Subscription) - 2 ->
  {error};

decode(Offset, Subscription, Secret) ->
  case Subscription of
    <<Session64:Offset/binary, "--", Sign/binary>> ->
      GeneratedSign = session_sign(Session64, Secret),
      case Sign of
        GeneratedSign ->
          {struct, Session} = mochijson2:decode(base64:decode(Session64)),
          Session;
        _ ->
          {invalid_signature}
      end;
    _ ->
      decode(Offset + 1, Subscription, Secret)
  end.
