%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        password protected publish
%%% @reference  
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
-module(password_publish, [Login, Password]).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("../log.hrl").

-include_lib("rtmp/include/rtmp.hrl").
-include("../rtmp/rtmp_session.hrl").

-export([publish/2]).


	
publish(#rtmp_session{host = Host} = State, #rtmp_funcall{args = [null, FullName, Spec]} = _AMF) when is_binary(Spec) ->

  {_RawName, Args} = http_uri2:parse_path_query(FullName),
  
  UserLogin = proplists:get_value("login", Args),
  UserPassword = proplists:get_value("password", Args),
  
  ?D({"Checking public login/password", UserLogin, UserPassword, Login, Password}),
  case {UserLogin,UserPassword} of
    {Login, Password} -> unhandled;
    _ ->
      ems_log:error(Host, "DENY_RECORD ~s ~s ~p ~s", [Spec, State#rtmp_session.addr, State#rtmp_session.user_id, FullName]),
      rtmp_session:reject_connection(State)
  end;

publish(_State, _AMF) ->
  unhandled.
  
