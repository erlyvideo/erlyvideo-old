%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        erlyvideo virtual hosts
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
-module(ems_vhosts).
-author('Max Lapshin <max@maxidoors.ru>').
-include("ems.hrl").
-export([start/0, stop/0]).


start() ->
  case ets:info(vhosts) of
    undefined -> vhosts = ets:new(vhosts, [set, named_table, public]);
    _ -> ets:delete_all_objects(vhosts)
  end,
  case application:get_env(erlyvideo, vhosts) of
    {ok, Hosts} when is_list(Hosts) -> init_vhosts(Hosts);
    _ -> ok
  end.
  

init_vhosts([]) ->
  ok;

init_vhosts([{Name, Host} | Hosts]) ->
  lists:foreach(fun({Key, Value}) ->
    ets:insert(vhosts, {{Name, Key}, Value})
  end, Host),
  lists:foreach(fun(Hostname) ->
    true = ets:insert(vhosts, {Hostname, Name})
  end, proplists:get_value(hostname, Host, [])),
  init_vhosts(Hosts).


stop() ->
  (catch ets:delete(vhosts)).
