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
-module(protected_play).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("rtmp/include/rtmp.hrl").
-include("../../include/rtmp_session.hrl").


-export([connect/2, play/2]).


connect(#rtmp_session{host = Host, addr = Address, player_info = PlayerInfo} = State, AMF) ->
  try perform_login(State, AMF) of
    NewState -> 
      rtmp_session:accept_connection(NewState),
      NewState
  catch
    _:_ ->
    	ems_log:access(Host, "REJECT ~s ~s ~p ~s json_session", [Address, Host, undefined, proplists:get_value(pageUrl, PlayerInfo)]),
      rtmp_session:reject_connection(State),
      State
  end.


perform_login(#rtmp_session{host = Host, addr = Address, player_info = PlayerInfo} = State, #rtmp_funcall{args = [_, Cookie | _]}) ->
  Secret = ems:get_var(secret_key, Host, undefined),
  Session = json_session:decode(Cookie, Secret),
  UserId = proplists:get_value(user_id, Session),
  Channels = proplists:get_value(channels, Session, []),
  PermitedUrl = proplists:get_value(url, Session),
  {ok, SessionId} = ems_users:login(Host, UserId, Channels),
	NewState = State#rtmp_session{user_id = UserId, session_id = SessionId, plugin_data = PermitedUrl},
	ems_log:access(Host, "CONNECT ~s ~s ~p ~s ~w ~s ~s", [Address, Host, UserId, proplists:get_value(pageUrl, PlayerInfo), Channels, PermitedUrl, ?MODULE]),
	NewState.


play(#rtmp_session{host = Host, streams = Streams, plugin_data = Name} = State, #rtmp_funcall{args = [null, Name | Args], stream_id = StreamId}) ->
  Stream = ems:element(StreamId, Streams),

  Options = apps_streaming:extract_play_args(Args),
  Stream ! {play, Name, Options},
  ems_log:access(Host, "PLAY ~s ~p ~s ~p", [State#rtmp_session.addr, State#rtmp_session.user_id, Name, StreamId]),  
  % gen_fsm:send_event(self(), {play, Name, Options}),
  State.
