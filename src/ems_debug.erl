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
-include("ems.hrl").

-compile(export_all).

sorted_procs(Sort) ->
  Pids = lists:sort(fun(Pid1, Pid2) ->
    {Sort, Val1} = process_info(Pid1, Sort),
    {Sort, Val2} = process_info(Pid2, Sort),
    Val1 > Val2
  end, processes()),
  lists:sublist([ [{pid,Pid}|process_info(Pid, [memory,message_queue_len,total_heap_size,heap_size,stack_size])] || Pid <- Pids], 10).