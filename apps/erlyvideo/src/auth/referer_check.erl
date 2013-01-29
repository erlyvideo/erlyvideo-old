%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Authorization with checking allowed referer
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
-module(referer_check).
-export([connect/2]).

connect(State, _Funcall) ->
  Host = rtmp_session:get(State, host),
  PlayerInfo = rtmp_session:get(State, player_info),
  PageUrl = proplists:get_value(pageUrl, PlayerInfo),
  {ok, {http,_,Hostname,_Port,_Path,_QueryString}} = http_uri:parse(binary_to_list(PageUrl)),
  case lists:member(Hostname, ems:get_var(hostname, Host, [])) of
    true ->
      unhandled;
    false -> 
      ems_log:access(Host, "REJECT ~s ~s referer_check", [Host, PageUrl]),
      rtmp_session:reject_connection(State),
      State
  end.

