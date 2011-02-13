%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Templates from http
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
-module(ems_http_templates).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../log.hrl").

-export([http/4]).

http(Host, 'GET', ["admin"], Req) ->
  ok = erlydtl:compile(ems_http:wwwroot(Host) ++ "/admin.html", admin_template),
  Entries = [{Name, proplists:get_value(client_count, Options)} || {Name, _Pid, Options} <- media_provider:entries(Host)],
  {ok, Index} = admin_template:render([{entries, Entries}]),
  Req:ok([{'Content-Type', "text/html; charset=utf8"}], Index);


http(_Host, _Method, _Path, _Req) ->
  unhandled.
