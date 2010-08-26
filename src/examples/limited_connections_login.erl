%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        trusted login
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
-module(limited_connections_login).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("../../include/ems.hrl").

-include("../../include/rtmp_session.hrl").

-export([connect/2, play/2, auth/3]).

auth(_Host, http, undefined) ->
  [];
  
auth(_Host, http, Header) ->
  [Method, Token] = string:tokens(Header, " "),
  case Method of
    "Basic" ->
      [UserId, Password] = string:tokens(binary_to_list(base64:decode(Token)), ":"),
      [{user_id, UserId}, {password, Password}]
  end;    

% auth(_Host, rtsp, Header) ->
%   case Header of
%     undefined -> undefined;
%     _ ->
%       Session = auth(_Host, http, binary_to_list(Header)),
%       ?D({"TRUSTED LOGIN", Header, Session}),
%       Session
%   end;
% 
auth(_Host, _Protocol, _Session) ->
  [].


play(#rtmp_session{host = Host} = State, Funcall) ->
  case ems:get_var(connection_timer, Host, undefined) of
    undefined -> ok;
    Timer -> ?D({"Timer started", Timer}), {ok, _Ref} = timer:send_after(Timer, self(), exit)
  end,
  apps_streaming:play(State, Funcall).

%%-------------------------------------------------------------------------
%% @spec connect(Session::rtmp_session(), Funcall::rtmp_funcall()) -> NewState::rtmp_session()
%% @doc Function always accept client, trusting all client information. 
%% It can even subscribe to channels
%% @end
%%-------------------------------------------------------------------------
	
connect(#rtmp_session{host = Host, addr = Address, player_info = PlayerInfo} = State, _AMF) ->
  case ems:get_var(connection_limit, Host, undefined) of
    undefined -> ok;
    Limit ->
      Count = length(supervisor:which_children(rtmp_session_sup)),
      true = (Count =< Limit)
  end,
  
  ems_log:access(Host, "CONNECT ~s ~s ~p ~s ~p ~s", [Address, Host, undefined, proplists:get_value(pageUrl, PlayerInfo), [], ?MODULE]),
	rtmp_session:accept_connection(State),
  State.
