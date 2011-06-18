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


-export([init/2, setup_channel/3, handle_frame/2, handle_data/3, sync/3, rtp_info/1, send_rtcp/3]).
-export([rtcp/2, rtcp_sr/1]).


%%--------------------------------------------------------------------
%% @spec (Direction::in|out, MediaInfo::media_info()) -> rtp_state()
%% @doc Initializes non-clean RTP object.
%% Should pass #media_info{} record with not more than one video and not more than one audio stream
%% Also mention about stream_id in #stream_info{} records, it will be used to indicate number of stream
%%
%% @end
%%--------------------------------------------------------------------
init(Location, #media_info{audio = Audio, video = Video} = _MediaInfo)
  when Location == local orelse
       Location == remote ->
  Streams = lists:sort(fun(#stream_info{stream_id = Id1}, #stream_info{stream_id = Id2}) ->
    Id1 =< Id2
  end, Audio ++ Video),
  ContentMap = [{Content,Id} || #stream_info{content = Content, stream_id = Id} <- Streams],
  #rtp_state{streams = Streams, location = Location, content_map = ContentMap}.

%%--------------------------------------------------------------------
%% @spec (RtpState::rtp_state(), StreamId::integer(), Headers::proplist) -> rtp_state()
%% @doc Synchronize stream StreamId from RTSP Rtp-Info header
%% @end
%%--------------------------------------------------------------------
sync(#rtp_state{channels = Channels} = State, Id, Headers) ->
  case element(Id,Channels) of
    undefined -> State;
    Channel ->
      Channel1 = rtp_decoder:sync(Channel, Headers),
      State#rtp_state{channels = setelement(Id, Channels, Channel1)}
  end.

%%--------------------------------------------------------------------
%% @spec (RtpState::rtp_state()) -> Rtp-Info Header
%% @doc Returns Rtp-Info header
%% @end
%%--------------------------------------------------------------------
rtp_info(#rtp_state{channels = Channels} = _State) ->
  string:join([rtp_encoder:rtp_info(Chan) || Chan <- tuple_to_list(Channels), Chan =/= undefined], ",").


%%--------------------------------------------------------------------
%% @spec (RtpState::rtp_state(), StreamId::integer(), Transport::proplist) -> {ok, rtp_state(), Reply::proplist()}
%% @doc Initialize stream StreamId with transport specification Transport.
%% There are two options: UDP and TCP interleaved:
%%
%% 1. UDP
%% {ok, RTP1, Reply} = rtp:setup_channel(RTP, 1, [{proto,udp},{remote_rtp_port,RPort1},{remote_rtcp_port,RPort2},{remote_addr,RAddr}])
%% Reply = [{local_rtp_port, LPort1}, {local_rtcp_port, LPort2}, {local_addr, LAddr}]
%%
%% Mention that remote_addr should be in erlang notation: {IP1,IP2,IP4,IP4}
%% setup_channel will open required ports itself, remember inside and reply with Reply
%%
%%
%% 2. TCP
%% {ok, RTP1, Reply} = rtp:setup_channel(RTP, 1, [{proto,tcp},{tcp_socket,Socket}])
%% in this case RTP will work with interleaved RTSP notation and will write bytes to tcpsocket itself
%%
%% @end
%%--------------------------------------------------------------------
setup_channel(#rtp_state{streams = Streams, location = Location, channels = Channels, udp = UDPMap, tcp_socket = OldSocket} = State, StreamId, Transport) ->
  Stream = #stream_info{content = Content} = lists:nth(StreamId, Streams),
  Channel = case Location of
    local -> rtp_decoder:init(Stream);
    remote -> rtp_encoder:init(Stream)
  end,
  State1 = State#rtp_state{channels = setelement(StreamId, Channels, Channel)},
  case proplists:get_value(proto, Transport, udp) of
    tcp ->
      {ok, State1#rtp_state{transport = tcp, tcp_socket = proplists:get_value(tcp_socket, Transport, OldSocket)}, []};
    udp ->
      UDP = #rtp_udp{local_rtp_port = SPort1, local_rtcp_port = SPort2, local_addr = Source} =
        case element(StreamId, UDPMap) of
          undefined ->
            ?DBG("OPEN PORTS: StreamId: ~p, UDPMap: ~p", [StreamId, UDPMap]),
            rtp:open_ports(Content);
          Else -> Else
        end,
      UDP1 = UDP#rtp_udp{
        remote_rtp_port = proplists:get_value(remote_rtp_port, Transport),
        remote_rtcp_port = proplists:get_value(remote_rtcp_port, Transport),
        remote_addr = proplists:get_value(remote_addr, Transport)
      },
      UDPMap1 = setelement(StreamId, UDPMap, UDP1),
      {ok, State1#rtp_state{transport = udp, udp = UDPMap1}, [{local_rtp_port, SPort1}, {local_rtcp_port, SPort2}, {local_addr, Source}]}
  end.

%%--------------------------------------------------------------------
%% @spec (RtpState::rtp_state(), VideoFrame::video_frame()) -> {ok, rtp_state()}
%% @doc Encode and send video_frame
%% @end
%%--------------------------------------------------------------------
handle_frame(#rtp_state{transport = Transport, content_map = ContentMap, channels = Channels, location = remote} = State, #video_frame{content = Content} = Frame) ->
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


%%--------------------------------------------------------------------
%% @spec (RtpState::rtp_state(), Channel, Data) -> {ok, rtp_state(), [video_frame()]}
%% @doc Decode network data. If you are working with tcp interleaved mode, second argument should be channel
%% In case of UDP it must be {Addr, Port}
%% Third argument is network data.
%%
%% Replies with new RTP state and possible list of decoded frames
%% @end
%%--------------------------------------------------------------------
handle_data(#rtp_state{transport = udp, udp = {#rtp_udp{remote_rtp_port = Port},_}} = State, {_Addr, Port}, Packet) ->
  handle_data(State, 0, Packet);

handle_data(#rtp_state{transport = udp, udp = {#rtp_udp{remote_rtcp_port = Port},_}} = State, {_Addr, Port}, Packet) ->
  handle_data(State, 1, Packet);

handle_data(#rtp_state{transport = udp, udp = {_,#rtp_udp{remote_rtp_port = Port}}} = State, {_Addr, Port}, Packet) ->
  handle_data(State, 2, Packet);

handle_data(#rtp_state{transport = udp, udp = {_,#rtp_udp{remote_rtcp_port = Port}}} = State, {_Addr, Port}, Packet) ->
  handle_data(State, 3, Packet);

handle_data(#rtp_state{channels = Channels} = State, Num, Packet) when Num rem 2 == 1 -> % RTCP
  Id = (Num - 1) div 2 + 1,
  Channel1 = rtcp(Packet, element(Id, Channels)),

  State1 = State#rtp_state{channels = setelement(Id, Channels, Channel1)},
  {ok, State2} = send_rtcp(State1, receiver_report, [{channel, Id}]),

  {ok, State2, []};

handle_data(#rtp_state{channels = Channels} = State, Num, Packet) when Num rem 2 == 0 -> % RTP
  Id = Num div 2 + 1,
  {ok, Channel1, NewFrames} = rtp_decoder:decode(Packet, element(Id, Channels)),
  % ?D({rtp,Num, size(Packet), length(NewFrames)}),
  reorder_frames(State#rtp_state{channels = setelement(Id, Channels, Channel1)}, NewFrames);

handle_data(State, _Num, _Packet) ->
  ?DBG("SKIP (~p):~n~p~n~p", [_Num, State, _Packet]),
  {ok, State, []}.

send_rtcp_data(#rtp_state{transport = Transport} = State, Id, Packet) ->
  Num = Id,
  %%%%%%%%%%%%%%%%%%%%%%%%%%  WARNING %%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%% FIXME!!!!!!!!!!
  %%%%
  %%%% Суть в том, что камеры бевард требуют обратный RTCP по тем же каналам, что и RTP
  %%%% Т.е. не по 1,3 а по 0,2
  %%%% По идее надо так:
  %%%% Num = (Id - 1)*2 + 1
  case Transport of
    tcp -> gen_tcp:send(State#rtp_state.tcp_socket, packet_codec:encode({rtcp, Num, Packet}));
    udp ->
      UDP = element(Id, State#rtp_state.udp),
      gen_udp:send(UDP#rtp_udp.rtcp_socket, UDP#rtp_udp.remote_addr, UDP#rtp_udp.remote_rtcp_port, Packet)
  end.


%%--------------------------------------------------------------------
%% @spec (RtpState::rtp_state(), Type, Options) -> {ok, rtp_state()}
%% @doc Sends requested RTCP
%%
%% Replies with new RTP state
%% @end
%%--------------------------------------------------------------------
send_rtcp(#rtp_state{channels = Channels} = State, sender_report, Options) ->
  EncodeAndSend = fun(Num) when element(Num, Channels) == undefined -> ok;
    (Num) ->
      Channel = element(Num, Channels),
      Packet = rtp_encoder:encode_rtcp(Channel, sender_report, Options),
      send_rtcp_data(State, Num, Packet)
  end,
  EncodeAndSend(1),
  EncodeAndSend(2),
  {ok, State};


send_rtcp(#rtp_state{channels = Channels} = State, receiver_report, Options) ->
  EncodeAndSend = fun(Num) when element(Num, Channels) == undefined -> ok;
    (Num) ->
      Channel = element(Num, Channels),
      case Channel#rtp_channel.stream_id of
        undefined -> ok;
        _ ->
          Packet = rtcp_rr(Channel),
          send_rtcp_data(State, Num, Packet)
      end
  end,
  EncodeAndSend(proplists:get_value(channel, Options)),
  {ok, State}.



stream(#rtp_state{streams = Streams}, C) -> hd([Stream || #stream_info{content = Content} = Stream <- Streams, C == Content]).


reorder_frames(#rtp_state{frames = OldFrames, reorder_length = ReorderLength} = RTP, NewFrames) ->
  Ordered = lists:sort(fun frame_sort/2, OldFrames++NewFrames),
  {Store, Send} = lists:split(ReorderLength, Ordered),
  {RTP1, ClientFrames} = add_configs(RTP#rtp_state{frames = Store}, Send, []),
  % [?D({F#video_frame.codec,F#video_frame.flavor,F#video_frame.dts, d(F#video_frame.body)}) || F <- ClientFrames],
  {ok, RTP1, ClientFrames}.

add_configs(#rtp_state{sent_audio_config = false} = Socket, [#video_frame{codec = aac, dts = DTS} = Frame|Frames], Acc) ->
  Config = (video_frame:config_frame(stream(Socket, audio)))#video_frame{dts = DTS, pts = DTS},
  add_configs(Socket#rtp_state{sent_audio_config = true}, Frames, [Frame,Config|Acc]);

add_configs(Socket, [#video_frame{codec = h264, flavor = keyframe, dts = DTS} = Frame|Frames], Acc) ->
  case video_frame:config_frame(stream(Socket, video)) of
    #video_frame{} = Config ->
      add_configs(Socket, Frames, [Frame,Config#video_frame{dts = DTS, pts = DTS}|Acc]);
    undefined ->
      add_configs(Socket, Frames, [Frame|Acc])
  end;

add_configs(Socket, [], Acc) ->
  {Socket, lists:reverse(Acc)};

add_configs(Socket, [Frame|Frames], Acc) ->
  add_configs(Socket, Frames, [Frame|Acc]).


frame_sort(#video_frame{dts = DTS1}, #video_frame{dts = DTS2}) -> DTS1 =< DTS2.




rtcp_sr(<<2:2, 0:1, _Count:5, ?RTCP_SR, _Length:16, StreamId:32, NTP:64, Timecode:32, PacketCount:32, OctetCount:32, _Rest/binary>>) ->
  % ?D({rtcp_sr, StreamId, NTP, Timecode, PacketCount, OctetCount}),
  #rtcp{ntp = NTP, stream_id = StreamId, timecode = Timecode, packet_count = PacketCount, octet_count = OctetCount}.


rtcp(<<_, ?RTCP_SR, _/binary>> = SR, #rtp_channel{timecode = TC} = RTP) when TC =/= undefined->
  #rtcp{ntp = NTP, stream_id = StreamId} = rtcp_sr(SR),
  RTP#rtp_channel{last_sr = NTP, stream_id = StreamId};

rtcp(<<_, ?RTCP_SR, _/binary>> = SR, #rtp_channel{} = RTP) ->
  #rtcp{ntp = NTP, stream_id = StreamId, timecode = Timecode} = rtcp_sr(SR),
  WallClock = round((NTP / 16#100000000 - ?YEARS_70) * 1000),
  RTP#rtp_channel{wall_clock = WallClock, timecode = Timecode, last_sr = NTP, stream_id = StreamId};

rtcp(<<_, ?RTCP_RR, _/binary>>, #rtp_channel{} = RTP) ->
  RTP.





rtcp_rr(#rtp_channel{last_sr = undefined} = RTP) ->
  rtcp_rr(RTP#rtp_channel{last_sr = 0});

rtcp_rr(#rtp_channel{stream_id = StreamId, sequence = Seq, last_sr = LSR} = _RTP) ->
  Count = 0,
  Length = 16,
  FractionLost = 0,
  LostPackets = 0,
  MaxSeq = case Seq of
    undefined -> 0;
    MS -> MS
  end,
  Jitter = 0,
  DLSR = 0,
  % ?D({send_rr, StreamId, Seq, LSR, MaxSeq}),
  <<1:2, 0:1, Count:5, ?RTCP_RR, Length:16, StreamId:32, FractionLost, LostPackets:24, MaxSeq:32, Jitter:32, LSR:32, DLSR:32>>.



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
  {ok, {_Addr, _}} = inet:sockname(RTPSocket),
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
  try_rtp(5050);

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


