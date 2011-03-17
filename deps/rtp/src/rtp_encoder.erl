%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2011 Max Lapshin
%%% @doc        RTP decoder module
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
-module(rtp_encoder).
-author('Max Lapshin <max@maxidoors.ru>').
-author('Maxim Treskin <zerthud@gmail.com>').

-include_lib("erlmedia/include/h264.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("erlmedia/include/sdp.hrl").
-include("rtp.hrl").
-include("log.hrl").


-define(RTP_SIZE, 1100).


-export([init/1, rtp_info/1, encode/2]).

init(#stream_info{codec = Codec, timescale = Scale} = Stream) ->
  #rtp_state{codec = Codec, stream_info = Stream, timescale = Scale, sequence = 0, wall_clock = 0, timecode = 0}.


rtp_info(#rtp_state{stream_info = #stream_info{stream_id = Id}, sequence = Sequence, timecode = Timecode}) ->
  io_lib:format("url=trackID=~p;seq=~p;rtptime=~p", [Id, Sequence, Timecode]).



encode(#video_frame{body = Data}, #rtp_state{codec = pcm_le} = RTP) ->
  compose_rtp(RTP, l2b(Data), ?RTP_SIZE);

encode(#video_frame{body = Data}, #rtp_state{codec = mp3} = RTP) ->
  Size = size(Data),
  %% Add support of frames with size more than 14 bit
  %% (set continuating flag, split to several RTP: http://tools.ietf.org/html/rfc5219#section-4.2)
  ADU =
    if Size < 16#40 ->                           % more than 6 bit
        <<0:1,0:1,Size:6>>;
       Size < 16#4000 ->
        <<0:1,1:1,Size:14>>;
       true ->
        ?DBG("Error: big frame", []),
        <<>>
    end,
  MP3 = <<ADU/binary, Data/binary>>,
  compose_rtp(RTP, MP3);
  
encode(#video_frame{body = Data}, #rtp_state{codec = aac} = RTP) ->
  AH = 16#00,
  ASsize = 16#10,                           % TODO: size of > 16#ff
  DataSize = bit_size(Data),
  Size = <<DataSize:2/big-integer-unit:8>>,
  AS = <<ASsize:8, Size/binary>>,
  Header = <<AH:8,AS/binary>>,
  AAC = <<Header/binary,Data/binary>>,
  compose_rtp(RTP#rtp_state{marker = true}, AAC);
  

encode(#video_frame{body = Data}, #rtp_state{codec = speex} = RTP) ->
  SPEEX = <<Data/binary, 16#7f:8 >>,        % Padding?
  compose_rtp(RTP#rtp_state{marker = false}, SPEEX);
  

encode(#video_frame{body = Data}, #rtp_state{codec = h264} = RTP) ->
  Fun =
    fun({config, [SPS, PPS]}, {BRtp, Accum}) ->
        {BR1, [Pack1]} = compose_rtp(BRtp, SPS),
        {BR2, [Pack2]} = compose_rtp(BR1#base_rtp{marker = false}, PPS),
        {BR2, [Pack2, Pack1 | Accum]};
       ({KF, Frame}, {BRtp, Accum}) when KF =:= frame;
                                       KF =:= keyframe ->
        {BaseRTP1, RevPacks} =
          lists:foldl(fun({M, F}, {BR, Acc}) ->
                          {NewBR, Ps} = compose_rtp(BR#base_rtp{marker = M}, F, 1387),
                          {NewBR, [Ps | Acc]}
                      end, {BRtp, Accum},
                      split_h264_frame(RTP#base_rtp.framelens, Frame)),
        {BaseRTP1#base_rtp{marker = false}, RevPacks}
    end,
  if is_list(Data) ->
      {NewBaseRTP, RevPacks} = lists:foldl(Fun, {RTP, []}, Data),
      Packs = lists:reverse(RevPacks);
     true ->
      {NewBaseRTP, RevPacks} = Fun(Data, {RTP, []}),
      Packs = lists:reverse(RevPacks)
  end,
  {NewBaseRTP, Packs};
  
encode(#video_frame{body = Data}, #rtp_state{codec = mpeg4} = RTP) ->
  compose_rtp(RTP, Data, 1388);

encode(#video_frame{body = Data}, #rtp_state{} = RTP) ->
  compose_rtp(RTP, Data).


split_h264_frame(FLS, Frame) ->
  split_h264_frame(FLS, Frame, []).

split_h264_frame(_FLS, <<>>, Acc) ->
  lists:reverse(Acc);
split_h264_frame(FLS, Frame, Acc) ->
  Len = (8*FLS),
  <<Size:Len, FrameRest/binary>> = Frame,
  <<D:Size/binary-unit:8, Rest/binary>> = FrameRest,
  M = (Rest =:= <<>>),
  split_h264_frame(FLS, Rest, [{M, D} | Acc]).


make_rtp_pack(#base_rtp{codec = PayloadType,
                        sequence = Sequence,
                        timecode = Timestamp,
                        stream_id = SSRC}, Marker, Payload) ->
  Version = 2,
  Padding = 0,
  Extension = 0,
  CSRC = 0,
  % ?D({rtp,Sequence,PayloadType,Timestamp}),
  <<Version:2, Padding:1, Extension:1, CSRC:4, Marker:1, PayloadType:7, Sequence:16, Timestamp:32, SSRC:32, Payload/binary>>.


%% Compose one RTP-packet from whole Data
compose_rtp(Base, Data) ->
  compose_rtp(Base, Data, undefined, [], undefined).

%% Compose number of RTP-packets from splitten Data to Size
compose_rtp(Base, Data, Size)
  when is_integer(Size) ->
  compose_rtp(Base, Data, Size, [], undefined).

compose_rtp(Base, <<>>, _, Acc, _) -> % Return new Sequence ID and list of RTP-binaries
  %%?DBG("New Sequence: ~p", [Sequence]),
  {Base#base_rtp{marker = false}, lists:reverse(Acc)};
compose_rtp(#base_rtp{sequence = Sequence, marker = _Marker,
                      packets = Packets, bytes = Bytes} = Base, Data, Size, Acc, Nal)
  when (is_integer(Size) andalso (size(Data) > Size)) ->
  <<P:Size/binary,Rest/binary>> = Data,
  Start = if Acc == [] -> 1; true -> 0 end,
  End = 0,
  {PFrag, NewNal} = fragment_nal(P, Nal, Start, End),
  M = 0,
  Pack = make_rtp_pack(Base, M, PFrag),
  compose_rtp(Base#base_rtp{sequence = inc_seq(Sequence),
                            packets = inc_packets(Packets, 1),
                            bytes = inc_bytes(Bytes, size(Pack))}, Rest, Size, [Pack | Acc], NewNal);
compose_rtp(#base_rtp{sequence = Sequence, marker = Marker,
                      packets = Packets, bytes = Bytes} = Base, Data, Size, Acc, Nal) ->
  if Marker -> M = 1; true -> M = 0 end,
  ResData =
    if ((Acc == []) or (Nal == undefined)) ->
        Data;
       true ->
        Start = 0, End = 1,
        {FN, _Nal} = fragment_nal(Data, Nal, Start, End),
        FN
    end,
  Pack = make_rtp_pack(Base, M, ResData),
  compose_rtp(Base#base_rtp{sequence = inc_seq(Sequence),
                            packets = inc_packets(Packets, 1),
                            bytes = inc_bytes(Bytes, size(Pack))}, <<>>, Size, [Pack | Acc], Nal).

fragment_nal(Data, Nal, S, E) ->
  if (S == 1) orelse (Nal == undefined) ->
      <<_:1, NRI:2, Type:5, Payload/binary>> = Data;
     true ->
      {NRI, Type} = Nal,
      Payload = Data
  end,
  FUInd = <<0:1, NRI:2, 28:5>>,
  R = 0,
  FUHeader = <<S:1, E:1, R:1, Type:5>>,
  PFrag = <<FUInd/binary, FUHeader/binary, Payload/binary>>,
  {PFrag, {NRI, Type}}.

l2b(List) when is_list(List) ->
  [l2b(B) || B <- List];
l2b(Bin) when is_binary(Bin) ->
    l2b(Bin, []).

l2b(<<>>, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
l2b(<<A:2/little-unit:8,Rest/binary>>, Acc) ->
    l2b(Rest, [<<A:2/big-unit:8>> | Acc]).



inc_seq(S) ->
  (S+1) band 16#FFFF.

inc_packets(S, V) ->
  (S+V) band 16#FFFFFFFF.

inc_bytes(S, V) ->
  (S+V) band 16#FFFFFFFF.

