%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        JSON session.
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
-module(json_session).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../rtmp/rtmp_session.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-include("../log.hrl").
-export([decode/2, encode/2, connect/2, binary_to_hexbin/1, auth/3, session_sign/2]).


auth(Host, _Method, LoginInfo) ->
  Secret = ems:get_var(secret_key, Host, undefined),
  decode(LoginInfo, Secret).
  

connect(#rtmp_session{host = Host, addr = Address, player_info = PlayerInfo} = State, AMF) ->
  try perform_login(State, AMF) of
    NewState -> 
      rtmp_session:accept_connection(NewState),
      NewState
  catch
    Class:Error ->
    	ems_log:access(Host, "REJECT ~s ~s ~p ~s json_session", [Address, Host, undefined, proplists:get_value(pageUrl, PlayerInfo)]),
    	?D({Class,Error,erlang:get_stacktrace()}),
      rtmp_session:reject_connection(State),
      State
  end.

perform_login(#rtmp_session{host = Host, addr = Address, player_info = PlayerInfo, session_id = DefaultSessionId} = State, #rtmp_funcall{args = [_, Cookie | _]}) ->
  Session = auth(Host, rtmp, Cookie),
  UserId = proplists:get_value(user_id, Session),
  SessionId = proplists:get_value(session_id, Session, DefaultSessionId),
  Channels = proplists:get_value(channels, Session, []),
	NewState = State#rtmp_session{user_id = UserId, session_id = SessionId},
	ems_log:access(Host, "CONNECT ~s ~s ~p ~p ~s ~w json_session", [Address, Host, UserId, SessionId, proplists:get_value(pageUrl, PlayerInfo), Channels]),
	NewState.
  
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
  

