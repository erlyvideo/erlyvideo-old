-module(rtmp_session).
-author(max@maxidoors.ru).
-export([decode/1, encode/1]).

decode(Session) when is_binary(Session) ->
  decode(0, Session);

decode(Session) when is_list(Session) ->
  decode(0, list_to_binary(Session)).
  
encode(Session) when is_list(Session) ->
  Json = iolist_to_binary(mochijson2:encode({struct, Session})),
  Json64 = base64:encode(Json),
  Sign = session_sign(Json64),
  <<Json64/binary, "--", Sign/binary>>.
  
binary_to_hexbin(L) ->
  list_to_binary(lists:flatten(lists:map(fun(X) -> int_to_hex(X) end, binary_to_list(L)))).

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
  $0+N;
hex(N) when N >= 10, N < 16 ->
  $a + (N-10).
  
session_sign(Session) ->
  binary_to_hexbin(crypto:sha_mac(ems:get_var(secret_key), Session)).
  
decode(Offset, Subscription) when Offset >= size(Subscription) - 2 ->
  {error};

decode(Offset, Subscription) ->
  case Subscription of
    <<Session64:Offset/binary, "--", Sign/binary>> ->
      GeneratedSign = session_sign(Session64),
      case Sign of
        GeneratedSign ->
          {struct, Session} = mochijson2:decode(base64:decode(Session64)),
          Session;
        _ ->
          {invalid_signature}
      end;
    _ ->
      decode(Offset + 1, Subscription)
  end.
