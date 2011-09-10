%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        export media as mp4 file
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
-module(ems_http_mp4_export).
-author('Max Lapshin <max@maxidoors.ru>').

-export([http/4]).

http(Host, 'GET', ["mp4" | PathSpec], Req) ->
  Path = ems:pathjoin(PathSpec),
  
  Req:stream(head, [{"Content-Type", "video/mp4"}, {"Connection", "close"}, {"Content-Disposition", "attachment; filename="++Path++".mp4"}]),
  {ok, Pid} = media_provider:open(Host, Path),
  Query = Req:parse_qs(),
  Options = case proplists:get_value("start", Query) of
    undefined -> [];
    Start -> [{start, list_to_integer(Start)*1000}]
  end ++ case proplists:get_value("duration", Query) of
    undefined -> [];
    Duration -> [{duration, list_to_integer(Duration)*1000}]
  end,
  mp4_writer:dump_media(Pid, [{writer, fun(_Offset, Bin) ->
    Req:stream(Bin)
  end}|Options]),
  Req:stream(close);


http(_Host, _Method, _Path, _Req) ->
  unhandled.
