%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        HTTP interface for turn-on/turn-off writing
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
-module(ems_http_write_control).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../ems.hrl").
-include("../log.hrl").

-export([http/4]).

http(Host, _Method, ["write_control", "enable" | Path], Req) ->
  Name = string:join(Path, "/"),
  ?D({write_enable, Host, Name}),
  ems_recorder:start_recorder(Host, Name, atomize(Req:parse_qs())),
  Req:ok([{'Content-Type', "application/json"}], "true\n");

http(Host, _Method, ["write_control", "disable" | Path], Req) ->
  Name = string:join(Path, "/"),
  ?D({write_disable, Host, Name}),
  ems_recorder:stop(Host, Name),
  Req:ok([{'Content-Type', "application/json"}], "true\n");

http(_Host, _Method, _Path, _Req) ->
  unhandled.

atomize(Query) ->
  atomize(Query, [{type,write}]).

atomize([{"url", Url}|Query], Acc) -> atomize(Query, [{url, list_to_binary(Url)}|Acc]);
atomize([{"type", "record"}|Query], Acc) -> atomize(Query, [{type, write}|Acc]);
atomize([{"type", "write"}|Query], Acc) -> atomize(Query, [{type, write}|Acc]);
atomize([{"type", "append"}|Query], Acc) -> atomize(Query, [{type, append}|Acc]);
atomize([{"sort_buffer", Buffer}|Query], Acc) -> atomize(Query, [{sort_buffer, list_to_integer(Buffer)}|Acc]);
atomize([{_K, _V}|Query], Acc) -> atomize(Query, Acc);
atomize([], Acc) -> Acc.










