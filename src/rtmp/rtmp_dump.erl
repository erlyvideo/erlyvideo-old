%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Can parse dump commands to debug rtmp scenarios
%%% Required only for console tool ``contrib/rtmp_dump''
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%% @hidden
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
-module(rtmp_dump).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/flv.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-include("../log.hrl").

-export([run/1]).

-record(dumper, {
  commands,
  rtmp
}).


run(Filename) when is_list(Filename) ->
  {ok, Commands} = file:consult(Filename),
  run(#dumper{commands = Commands});
  
run(#dumper{commands = [Command|Commands]} = Dumper) ->
  [Function|Args] = erlang:tuple_to_list(Command),
  ?D(Command),
  case erlang:apply(?MODULE, Function, [Dumper#dumper{commands = Commands}|Args]) of
    {ok, Dumper1} ->
      run(Dumper1);
    stop ->
      ?D(stop)
  end.


  