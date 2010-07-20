%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Shared objects for RTMP
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
-module(apps_shared_objects).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../../include/ems.hrl").

-export(['WAIT_FOR_DATA'/2]).
-export([getServiceList/2]).




'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.



getServiceList(State, AMF) -> 
  rtmp_session:reply(State,AMF#rtmp_funcall{args = [null, [<<"hello">>, <<"setData">>, <<"getData">>]]}),
  % gen_fsm:send_event(self(), {publish, record, Name}),
  State.

