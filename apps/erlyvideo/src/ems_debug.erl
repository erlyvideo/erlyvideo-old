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

-export([sorted_procs/1]).

sorted_procs(Sort) ->
  Info = [ case process_info(Pid, [Sort,memory,message_queue_len,total_heap_size,heap_size,stack_size,dictionary,current_function]) of
    undefined -> undefined;
    Else -> [{pid,Pid}|Else]
  end || Pid <- processes()],
  ClearedInfo = [I||I <- Info, I =/= undefined],
  Sorted = lists:sort(fun(Info1, Info2) ->
    Val1 = proplists:get_value(Sort, Info1),
    Val2 = proplists:get_value(Sort, Info2),
    Val1 > Val2
  end, ClearedInfo),
  lists:sublist(Sorted, 10).
