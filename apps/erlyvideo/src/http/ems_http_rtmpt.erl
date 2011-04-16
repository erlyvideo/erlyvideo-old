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
-module(ems_http_rtmpt).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../log.hrl").

-define(SERVER_HEADER, {"Server", "FlashCom/3.5.4"}).
-define(CONTENT_TYPE, {'Content-Type', "application/x-fcs"}).

-export([http/4]).

http(Host, 'POST', ["open", ChunkNumber], Req) ->
  <<_Timeout>> = Req:get(body),
  {ok, Pid, SessionId} = rtmpt:open(Req:get(peer_addr), rtmp_session),
  ems_log:access(Host, "RTMPT OPEN ~p ~p ~p", [SessionId, ChunkNumber, Pid]),
  Req:ok([?CONTENT_TYPE, ?SERVER_HEADER, {'Cache-Control', 'no-cache'},{'Connection','Keep-Alive'}], [SessionId, "\n"]);
  
http(Host, 'POST', ["idle", SessionId, SequenceNumber], Req) ->
  case rtmpt:idle(SessionId, Req:get(peer_addr), list_to_integer(SequenceNumber)) of
    {ok, Data} ->
      Req:ok([?CONTENT_TYPE, ?SERVER_HEADER], [33, Data]);
    {error, _} ->
      ems_log:error(Host, "RTMPT IDLE to closed session ~p", [SessionId]),
      Req:stream(<<0>>),
      Req:stream(close)
  end;

http(Host, 'POST', ["send", SessionId, SequenceNumber], Req) ->
  case rtmpt:send(SessionId, Req:get(peer_addr), list_to_integer(SequenceNumber), Req:get(body)) of
    {ok, Data} ->
      Req:ok([?CONTENT_TYPE, ?SERVER_HEADER], [33, Data]);
    {error, _} ->
      ems_log:error(Host, "RTMPT SEND to closed session ~p", [SessionId]),
      Req:stream(<<0>>),
      Req:stream(close)
  end;
  
  
http(Host, 'POST', ["close", SessionId, _ChunkNumber], Req) ->
  ems_log:error(Host, "RTMPT CLOSE ~p", [SessionId]),
  rtmpt:close(SessionId, Req:get(peer_addr)),
  Req:stream(<<0>>),
  Req:stream(close);
    
http(_Host, 'POST', ["fcs", "ident", _ChunkNumber], Req) ->
  ems_log:access(_Host, "RTMPT ident/~p", [_ChunkNumber]),
  Req:ok([?CONTENT_TYPE, ?SERVER_HEADER], "<fcs><Company>Erlyvideo</Company><Team>Erlyvideo</Team></fcs>");
    
http(_Host, 'POST', ["fcs", "ident2"], Req) ->
  ems_log:access(_Host, "RTMPT ident2", []),
  Req:ok([?CONTENT_TYPE, ?SERVER_HEADER], "0.1");

http(_Host, _Method, _Path, _Req) ->
  unhandled.
