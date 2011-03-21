%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2011 Max Lapshin
%%% @doc        RTP module
%%% @end
%%% @reference  See <a href="http://erlyvideo.org/ertp" target="_top">http://erlyvideo.org</a> for common information.
%%% @end
%%%
%%% This file is part of erlang-rtp.
%%%
%%% erlang-rtp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtp).
-behaviour(application).
-include("log.hrl").
-include("../include/rtp.hrl").
-author('Max Lapshin <max@maxidoors.ru>').


-export([start/0, start/2, stop/1, config_change/3]).
-export([start_server/1, test/0]).
-export([open_ports/1]).


start() ->
  application:start(rtp).

%%--------------------------------------------------------------------
%% @spec (Type::any(), Args::list()) -> any()
%% @doc Starts RTP library
%% @end
%%--------------------------------------------------------------------
start(_Type, _Args) ->
  rtp_sup:start_link().



%%--------------------------------------------------------------------
%% @spec (Any::any()) -> ok()
%% @doc Stop RTP library
%% @end
%%--------------------------------------------------------------------
stop(_S) ->
  ok.


%%--------------------------------------------------------------------
%% @spec (Any::any(),Any::any(),Any::any()) -> any()
%% @doc Reload RTP config
%% @end
%%--------------------------------------------------------------------
config_change(_Changed, _New, _Remove) ->
  ok.

%%
start_server(Args) ->
  rtp_sup:start_server(Args).


test() ->
  eunit:test([rtp_decoder]).


open_ports(Type) ->
  {RTP, RTPSocket, RTCP, RTCPSocket} = try_ports(Type),
  gen_udp:controlling_process(RTPSocket, self()),
  gen_udp:controlling_process(RTCPSocket, self()),
  {ok, {Addr, _}} = inet:sockname(RTPSocket),
  Source = lists:flatten(io_lib:format("~p.~p.~p.~p", tuple_to_list(Addr))),
  #rtp_udp{
    server_rtp_port = RTP,
    rtp_socket = RTPSocket,
    server_rtcp_port = RTCP,
    rtcp_socket = RTCPSocket,
    source = Source
  }.
  

try_ports(audio) ->
  try_rtp(8000);

try_ports(video) ->
  try_rtp(5000).

try_rtp(40000) ->
  error;

try_rtp(Port) ->
  case gen_udp:open(Port, [binary, {active, true}, {recbuf, 1048576}]) of
    {ok, RTPSocket} ->
      try_rtcp(Port, RTPSocket);
    {error, _} ->
      try_rtp(Port + 2)
  end.

try_rtcp(RTP, RTPSocket) ->
  RTCP = RTP+1,
  case gen_udp:open(RTCP, [binary, {active, true}]) of
    {ok, RTCPSocket} ->
      {RTP, RTPSocket, RTCP, RTCPSocket};
    {error, _} ->
      gen_udp:close(RTPSocket),
      try_rtp(RTP + 2)
  end.


