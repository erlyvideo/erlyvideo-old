%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        runtime debugging module
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
-module(ems_debug).
-author('Max Lapshin <max@maxidoors.ru>').

-compile(export_all).

full_memory(Pid) ->
  lists:sum([Mem || {_, Mem, _} <- element(2,process_info(Pid, binary))]).
  
proc_info(Pid, binary) ->
  full_memory(Pid);

proc_info(Pid, Sort) ->
  element(2, process_info(Pid, Sort)).

top(Sort) ->
  DirtyList = [{Pid,(catch proc_info(Pid,Sort))} || Pid <- processes()],
  lists:reverse(lists:keysort(2, [{Pid,Count} || {Pid,Count} <- DirtyList, is_number(Count)] )).

top(Sort, Limit) ->
  lists:sublist(top(Sort), Limit).

limited(Sort, Limit) ->
  [{Pid,Count} || {Pid,Count} <- top(Sort), Count >= Limit].

kill(Sort, Limit) ->
  [erlang:exit(Pid,kill) || {Pid, _Count} <- limited(Sort, Limit)].

full_info(Sort) -> full_info(Sort, 10).
full_info(Sort, Limit) ->
  [{Pid, process_info(Pid)} || {Pid, _Count} <- top(Sort, Limit)].


get_state(Name) when is_atom(Name) -> get_state(whereis(Name));

get_state(Server) when is_pid(Server) ->
  Stat1 = fun(Pid) -> {status, _, _, Items} = sys:get_status(Pid), lists:nth(5,Items) end,
  Stat2 = fun(Pid) -> element(2, lists:nth(3,Stat1(Pid))) end,
  Stat3 = fun(Pid) -> proplists:get_value("State", Stat2(Pid)) end,
  Stat3(Server).
  
