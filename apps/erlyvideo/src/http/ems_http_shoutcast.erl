%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTMPT support
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
-module(ems_http_shoutcast).
-author('Ilya Shcherbak <tthread@gmail.com>').
-include("../log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-export([http/4]).

http(Host, 'GET', ["shoutcast"|Name], Req) ->
  case media_provider:play(Host,string:join([P || P <- Name, P =/= ".."], "/"),[]) of
    {ok,Stream} ->
      shoutcast_writer:write(Stream,Req),
      Req:stream(close);
    {notfound,Reason} -> Reason
  end;

http(_Host,_Method,_Path,_Req)->
  unhandled.

