%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2011 Max Lapshin
%%% @doc        Erlyvideo API for switching stream source
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
-module(ems_http_switch_source).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../log.hrl").

-export([http/4]).



http(Host, 'GET', ["erlyvideo", "api", "switch_source"], Req) ->
  Query = Req:parse_qs(),
  Stream = proplists:get_value("stream", Query),
  To = proplists:get_value("to", Query),
  proxy_media:switch_to(Host, Stream, Host, To),
  Req:ok([{'Content-Type', "application/json"}], "true\n");

http(_, _, _, _) ->
  unhandled.
  
  
