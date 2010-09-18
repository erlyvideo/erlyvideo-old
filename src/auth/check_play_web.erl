%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTMP functions that support playing
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
-module(check_play_web, [URL]).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../ems.hrl").
-include("../../include/rtmp_session.hrl").

-export([play/2]).

play(#rtmp_session{addr = IP, user_id = UserId, session_id = SessionId}, #rtmp_funcall{args = [null, FullName | _Args]}) when is_binary(FullName) ->
  {RawName, _Args2} = http_uri2:parse_path_query(FullName),
  Name = string:join( [Part || Part <- ems:str_split(RawName, "/"), Part =/= ".."], "/"),
  {http, _UserInfo, Host, Port, Path, _Query} = http_uri2:parse(URL),
  
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary]),
  Req = io_lib:format("GET ~s?ip=~s&file=~s&user_id=~p&session_id=~p HTTP/1.1\r\nHost: ~s\r\n\r\n", [Path, IP, Name, Host, UserId, SessionId]),
  ok = gen_tcp:send(Socket, Req),
  inet:setopts(Socket, [{packet,http},{active,once}]),
  receive
    {http, Socket, {http_response, _, Code, _Status}} ->
      Code = 200,
      gen_tcp:close(Socket),
      ?D({"URL accepted play", URL, FullName, Name}),
      unhandled
  after
    3000 ->
      erlang:error(http_verify_timeout)
  end.


  
