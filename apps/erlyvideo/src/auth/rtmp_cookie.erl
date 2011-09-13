%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        Rails-like JSON session passed from cookie to flash player connect.
%%% @reference  See <a href="http://erlyvideo.org/authorization" target="_top">http://erlyvideo.org/authorization</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtmp_cookie).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../log.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-export([decode/2, encode/2]).

-export([connect/2]).


connect(State, #rtmp_funcall{args = [_, Cookie | _]}) ->
  Host = rtmp_session:get(State, host),
  Session = decode(Cookie, ems:get_var(secret_key, Host, undefined)),
  UserId = proplists:get_value(user_id, Session, rtmp_session:get(State, user_id)),
  SessionId = proplists:get_value(session_id, Session, rtmp_session:get(State, session_id)),
  rtmp_session:set(State, [{user_id,UserId},{session_id,SessionId},{session_data,Session}]);

connect(State, _AMF) ->
  State.
  
  


decode(undefined, undefined) ->
  [];

decode(Cookie, Secret) when is_list(Cookie) ->
  decode(list_to_binary(Cookie), Secret);

decode(Cookie, Secret) when is_binary(Cookie) ->
  {Session64, Sign} = split_cookie(Cookie),
  verify_signature(Session64, Sign, Secret),
  {object, Session} = mochijson2:decode(base64:decode(Session64)),
  Session.



encode(Session, undefined) ->
  Json = iolist_to_binary(mochijson2:encode({struct, Session})),
  base64:encode(Json);


encode(Session, Secret) when is_list(Session) ->
  Json64 = encode(Session, undefined),
  Sign = session_sign(Json64, Secret),
  <<Json64/binary, "--", Sign/binary>>.

session_sign(Session, Secret) ->
  ems:binary_to_hexbin(crypto:sha_mac(Secret, Session)).


verify_signature(_, _, undefined) ->
  ok;

verify_signature(Session64, Sign, Secret) ->
  Sign == session_sign(Session64, Secret) orelse erlang:error(invalid_cookie_signature).

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


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").

fixtures() -> [
  {valid_cookie_with_secret, 
    {<<"eyJ1c2VyX2lkIjo1LCJzZXNzaW9uX2lkIjoxMzQyfQ==--22c6af490138b3a3a10046d431801f079271e7fa">>, "TopSecrEt", [{user_id,5},{session_id,1342}]}},
  {valid_cookie_without_secret,
    {<<"eyJ1c2VyX2lkIjo1LCJzZXNzaW9uX2lkIjoxMzQyfQ==">>, undefined, [{user_id,5},{session_id,1342}]}},
  {invalid_cookie_with_secret,
    {<<"eyJ1c2VyX2lkIjo1LCJzZXNzaW9uX2lkIjoxMzQyfQ==--22c6af490138b3a3a10046d431801f079271e555">>, "TopSecrEt", [{user_id,5},{session_id,1342}]}}
  ].

fixture(Fixture) ->
  proplists:get_value(Fixture, fixtures()).

decode_valid_signed_cookie_test() ->
  {Cookie, Secret, Session} = fixture(valid_cookie_with_secret),
  ?assertEqual(Session, decode(Cookie, Secret)),
  ?assertEqual(Cookie, encode(Session, Secret)).

decode_valid_unsigned_cookie_test() ->
  {Cookie, Secret, Session} = fixture(valid_cookie_without_secret),
  ?assertEqual(Session, decode(Cookie, Secret)),
  ?assertEqual(Cookie, encode(Session, Secret)).

decode_invalid_cookie_test() ->
  {Cookie, Secret, Session} = fixture(invalid_cookie_with_secret),
  ?assertError(invalid_cookie_signature, decode(Cookie, Secret)),
  ?assertNot(Cookie == encode(Session, Secret)).
