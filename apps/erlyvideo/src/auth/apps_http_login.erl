%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        Go to HTTP auth backend and ask if it is ok to login with user/password, passed in RTMP params
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
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
-module(apps_http_login, [URL]).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("../log.hrl").

-include_lib("rtmp/include/rtmp.hrl").

-export([connect/2]).

connect(State, AMF) ->
  PlayerInfo = rtmp_session:get(State, player_info),
  {App, Args} = http_uri2:parse_path_query(proplists:get_value(app, PlayerInfo)),
  Login = proplists:get_value("login", Args),
  Password = proplists:get_value("password", Args),
  is_list(Login) orelse erlang:error(havent_passed_login),
  is_list(Password) orelse erlang:error(havent_passed_password),
  {ok, _Headers, _Body} = http_stream:get(URL ++ "?login="++Login++"&password="++Password, []),
  Info1 = lists:keystore(app, 1, PlayerInfo, {app, App}),
  State1 = rtmp_session:set(State, [{player_info, Info1},{user_id,list_to_binary(Login)}]),
  ?D({trying_to_login, Args}),
  {unhandled, State1, AMF}.
