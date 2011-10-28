%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        check connected users limit
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
-module(auth_users_limit, [Limit]).
-author('Max Lapshin <max@maxidoors.ru>').

-include("../log.hrl").

-export([connect/2]).

connect(State, _AMF) ->
  ?D({"Checking limit", Limit,length(supervisor:which_children(rtmp_session_sup)) }),
  case length(supervisor:which_children(rtmp_session_sup)) of
    Count when Count =< Limit ->
      unhandled;
    Count ->
      ems_log:error("Connection limit ~p~n", [Count]),  
    	rtmp_session:reject_connection(State),
      State
  end.
  