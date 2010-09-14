%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Red5 fitc demo
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
-module(fitc_demo).
-author('Max Lapshin <max@maxidoors.ru>').

-include("../../include/rtmp_session.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-export([connect/2]).

-export([getStreams/2, publish/2]).

-export(['WAIT_FOR_DATA'/2]).

'WAIT_FOR_DATA'({newStreamRegistered, StreamName}, #rtmp_session{socket = Socket} = State) ->
  io:format("New stream ~p~n", [StreamName]),
  rtmp_socket:invoke(Socket, #rtmp_funcall{command = 'newStream', args = [null, StreamName]}),
  {next_state, 'WAIT_FOR_DATA', State};


'WAIT_FOR_DATA'(_Message, #rtmp_session{} =_State) -> {unhandled}.


connect(#rtmp_session{host = Host, addr = Address, socket = Socket, player_info = PlayerInfo} = State, _AMF) ->
  UserId = random:uniform(10000000),
  Channels = [10],
  {ok, SessionId} = ems_users:login(Host, UserId, Channels),
	NewState = State#rtmp_session{session_id = SessionId},

	ems_log:access(Host, "CONNECT ~p ~s ~p ~s ~p", [Address, Host, UserId, proplists:get_value(pageUrl, PlayerInfo), self()]),
  rtmp_session:accept_connection(NewState),
  rtmp_socket:invoke(Socket, #rtmp_funcall{command = 'setId', args = [null, UserId]}),
  NewState.
	
	
getStreams(#rtmp_session{host = Host} = State, AMF) ->
  Streams = [Name || {Name, _} <- media_provider:entries(Host)],
  io:format("getStreams() -> ~p~n", [Streams]),
  rtmp_session:reply(State,AMF#rtmp_funcall{args = [null, Streams]}),
  State.

publish(#rtmp_session{host = Host} = State, #rtmp_funcall{args = [null,URL,_]} = AMF) ->
  io:format("Publish videoConf ~p~n", [URL]),
  State1 = apps_recording:publish(State, AMF),
  {ok, Clients} = ems_users:clients(Host),
  io:format("Notifying ~p~n", [Clients]),
  [gen_fsm:send_event(Client, {newStreamRegistered, URL}) || Client <- Clients, Client /= self()],
  State1.
  
  
