%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Authorization with checking allowed url
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
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
-module(apps_rewrite_play, [URL]).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("rtmp/include/rtmp.hrl").
-include("../../include/rtmp_session.hrl").


-export([play/2, run_http_request/2]).


dump_session_id(SessionId) when is_integer(SessionId) -> integer_to_list(SessionId);
dump_session_id(SessionId) when is_binary(SessionId) -> binary_to_list(SessionId);
dump_session_id(SessionId) when is_list(SessionId) -> SessionId;
dump_session_id(SessionId) -> lists:flatten(io_lib:format("~p",[SessionId])).


play(#rtmp_session{} = State, #rtmp_funcall{args = [null, Name | _Args]} = AMF) when is_binary(Name) ->
  run_http_request(State, AMF).


run_http_request(#rtmp_session{addr = IP, user_id = UserId, session_id = SessionId} = State, #rtmp_funcall{args = [null, Name | _Args]} = AMF) when is_binary(Name) ->
  {http, _UserInfo, Host, Port, Path, _Query} = http_uri2:parse(URL),
  
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary]),
  Req = io_lib:format("GET ~s?ip=~s&file=~s&user_id=~p&session_id="++dump_session_id(SessionId)++" HTTP/1.1\r\nHost: ~s\r\n\r\n", [Path, IP, Name, UserId, Host]),
  ok = gen_tcp:send(Socket, Req),
  inet:setopts(Socket, [{packet,http},{active,once}]),
  receive
    {http, Socket, {http_response, _, 200, _Status}} ->
      gen_tcp:close(Socket),
      unhandled;
    {http, Socket, {http_response, _, 302, _Status}} ->
      inet:setopts(Socket, [{active,once}]),
      fetch_headers(State, AMF)
  after
    3000 ->
      erlang:error(http_redirect_timeout)
  end.


fetch_headers(State, #rtmp_funcall{args = [null, _OldName | Args]} = AMF) ->
  receive
    {http, Socket, {http_header, _, "X-Location", _, Path}} ->
      gen_tcp:close(Socket),
      {unhandled, State, AMF#rtmp_funcall{args = [null, list_to_binary(Path) | Args]}};
    {http, Socket, {http_header, _, 'Location', _, Path}} ->
      gen_tcp:close(Socket),
      {unhandled, State, AMF#rtmp_funcall{args = [null, list_to_binary(Path) | Args]}};
    {http, Socket, {http_header, _, _Key, _, _Value}} ->
      inet:setopts(Socket, [{active,once}]),
      fetch_headers(State, AMF)
  after
    3000 ->
      erlang:error(http_redirect_timeout)
  end.
         