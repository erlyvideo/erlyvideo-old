%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTMP functions, that support pushing messages to client
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org</a> for more information
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
-module(apps_push).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../../include/ems.hrl").
-include("../../include/rtmp_session.hrl").

-export(['WAIT_FOR_DATA'/2]).

-export([sendMessage/2]).

'WAIT_FOR_DATA'({message, Message}, #rtmp_session{socket = Socket} = State) ->
  % io:format("NetConnection.Message ~s~n", [Message]),
  rtmp_socket:status(Socket, 0, <<"NetConnection.Message">>, Message),
  {next_state, 'WAIT_FOR_DATA', State};



'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.


sendMessage(#rtmp_session{host = Host} = State, #rtmp_funcall{args = [null, ChannelId, Message]}) -> 
  Channel = round(ChannelId),
  ems_users:send_to_channel(Host, Channel, Message),
  ems_log:access(Host, "MESSAGE ~p ~p ~p ~p~n", [Host, State#rtmp_session.addr, Channel, Message]),
  State.
