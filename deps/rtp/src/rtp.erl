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
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-author('Max Lapshin <max@maxidoors.ru>').


-export([start/0, start/2, stop/1, config_change/3]).
-export([start_server/1, test/0]).
-export([open_ports/1]).


-export([init/2, setup_channel/3, handle_frame/2, handle_data/3]).

init(Direction, #media_info{audio = Audio, video = Video} = _MediaInfo) when Direction == in orelse Direction == out ->
  Streams = lists:sort(fun(#stream_info{stream_id = Id1}, #stream_info{stream_id = Id2}) ->
    Id1 =< Id2
  end, Audio ++ Video),
  ContentMap = [{Content,Id} || #stream_info{content = Content, stream_id = Id} <- Streams],
  #rtp_state{streams = Streams, direction = Direction, content_map = ContentMap}.


setup_channel(#rtp_state{streams = Streams, direction = Direction, channels = Channels, udp = UDPMap} = State, StreamId, Transport) ->
  Stream = #stream_info{content = Content} = lists:nth(StreamId, Streams),
  Channel = case Direction of
    in -> rtp_decoder:init(Stream);
    out -> rtp_encoder:init(Stream)
  end,
  State1 = State#rtp_state{channels = setelement(StreamId, Channels, Channel)},
  case proplists:get_value(proto, Transport, udp) of
    tcp ->
      {ok, State1#rtp_state{transport = tcp, tcp_socket = proplists:get_value(tcp_socket, Transport)}, []};
    udp -> 
      UDP = #rtp_udp{local_rtp_port = SPort1, local_rtcp_port = SPort2, local_addr = Source} = rtp:open_ports(Content),
      UDP1 = UDP#rtp_udp{
        remote_rtp_port = proplists:get_value(remote_rtp_port, Transport),
        remote_rtcp_port = proplists:get_value(remote_rtcp_port, Transport),
        remote_addr = proplists:get_value(remote_addr, Transport)
      },
      UDPMap1 = setelement(StreamId, UDPMap, UDP1),
      {ok, State1#rtp_state{transport = udp, udp = UDPMap1}, [{local_rtp_port, SPort1}, {local_rtcp_port, SPort2}, {local_addr, Source}]}
  end.

handle_frame(#rtp_state{transport = Transport, content_map = ContentMap, channels = Channels, direction = out} = State, #video_frame{content = Content} = Frame) ->
  Num = proplists:get_value(Content, ContentMap),
  Channel = element(Num, Channels),
  {ok, Channel1, Packets} = rtp_encoder:encode(Frame, Channel),
  case Transport of
    tcp ->
      RTPData = [packet_codec:encode({rtp, (Num-1)*2, Packet}) || Packet <- Packets],
      gen_tcp:send(State#rtp_state.tcp_socket, RTPData);
    udp ->
      #rtp_udp{rtp_socket = Socket, remote_addr = Addr, remote_rtp_port = Port} = element(Num, State#rtp_state.udp),
      [gen_udp:send(Socket, Addr, Port, Packet) || Packet <- Packets]
  end,      
  {ok, State#rtp_state{channels = setelement(Num, Channels, Channel1)}}.


handle_data(#rtp_state{} = State, Addr, Data) ->
  {ok, State, []}.



% make_invite_call() ->
%   MediaInfo1 = #media_info{audio = [#stream_info{codec = speex, content = audio, stream_id = 1}]},
%   Rtp1 = rtp:init(MediaInfo1),
%   {ok, Rtp2, SetupReply} = rtp:setup(Rtp1, 1, [{proto,udp}]),
%   MediaInfo2 = inject_setup_reply(MediaInfo1, SetupReply),
%   SDP = sdp:encode(MediaInfo2).
% 
% reply_invite(SDP) ->
%   MediaInfo1 = sdp:decode(SDP),
%   MediaInfo2 = select_proper_codecs(MediaInfo1),
%   SetupOptions = extract_setup_options(MediaInfo2, 1),
%   Rtp1 = rtp:init(MediaInfo2),
%   {ok, Rtp2, SetupReply} = rtp:setup(Rtp1, 1, SetupOptions),
%   MediaInfo3 = inject_setup_reply(MediaInfo2, SetupReply),
%   sdp:encode(MediaInfo3).



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
  % Source = lists:flatten(io_lib:format("~p.~p.~p.~p", tuple_to_list(Addr))),
  Source = {127,0,0,1},
  #rtp_udp{
    local_rtp_port = RTP,
    rtp_socket = RTPSocket,
    local_rtcp_port = RTCP,
    rtcp_socket = RTCPSocket,
    local_addr = Source
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


