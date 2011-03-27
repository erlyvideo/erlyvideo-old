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


-export([init/1, rtp_info/1, encode/2, encode_rtcp/3]).

init(#stream_info{codec = Codec, timescale = Scale, stream_id = StreamId, config = Config} = Stream) ->
  LengthSize = case Codec of
    h264 -> proplists:get_value(length_size, h264:metadata(Config));
    _ -> undefined
  end,
  #rtp_channel{codec = Codec, stream_info = Stream, stream_id = StreamId, timescale = Scale, length_size = LengthSize,
             payload_type = sdp_encoder:payload_type(Codec), sequence = 1, wall_clock = 0, timecode = 1}.


rtp_info(#rtp_channel{stream_info = #stream_info{stream_id = Id}, sequence = Sequence, timecode = Timecode}) ->
  io_lib:format("url=trackID=~p;seq=~p;rtptime=~p", [Id, Sequence, Timecode]).



dts_to_timecode(#rtp_channel{timescale = Scale, timecode = BaseTimecode, wall_clock = WallClock}, DTS) ->
  % ?D({dtst_, WallClock, BaseTimecode, Scale, DTS, round((DTS - WallClock)*Scale + BaseTimecode)}),
  round((DTS - WallClock)*Scale + BaseTimecode).


encode(#video_frame{flavor = config}, #rtp_channel{} = RTP) ->
  {ok, RTP, []};

encode(#video_frame{dts = DTS, body = Data} = _F, #rtp_channel{} = RTP) ->
  % ?D({dts,_F#video_frame.codec,_F#video_frame.flavor, DTS,dts_to_timecode(RTP, DTS)}),
  {ok, RTP1, Packets} = encode_data(Data, RTP, dts_to_timecode(RTP, DTS)),
  RTP2 = RTP1#rtp_channel{
    packet_count = RTP1#rtp_channel.packet_count + length(Packets),
    octet_count = RTP1#rtp_channel.octet_count + iolist_size(Packets)
  },
  {ok, RTP2, Packets}.


encode_rtcp(#rtp_channel{stream_id = StreamId, packet_count = PacketCount, octet_count = OctetCount, timescale = Scale}, sender_report, _) ->
  Count = 0,
  Length = 6, % StreamId, 2*NTP, Timecode, Packet, Octet words
  {Mega, Sec, Micro} = erlang:now(),
  MSW = (Mega*1000000 + Sec + ?YEARS_70) band 16#FFFFFFFF,
  LSW = Micro * 1000,
  NTP = MSW + Micro / 1000000,
  Timecode = round((NTP - ?YEARS_100)*1000*Scale),
  <<2:2, 0:1, Count:5, ?RTCP_SR, Length:16, StreamId:32, MSW:32, LSW:32, Timecode:32, PacketCount:32, OctetCount:32>>.
  
  

% encode_data(Data, #rtp_channel{codec = pcm_le} = RTP, Timecode) ->
%   compose_rtp(RTP, l2b(Data), ?RTP_SIZE, Timecode);

encode_data(Data, #rtp_channel{codec = mp3} = RTP, Timecode) ->
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
  compose_rtp(RTP, MP3, Timecode);

encode_data(Data, #rtp_channel{codec = aac, buffer = undefined} = RTP, _Timecode) ->
  {ok, RTP#rtp_channel{buffer = Data}, []};

encode_data(Data, #rtp_channel{codec = aac, buffer = Bin} = RTP, Timecode) when is_binary(Bin) ->
  encode_data(<<Bin/binary, Data/binary>>, RTP#rtp_channel{buffer = done}, Timecode);

encode_data(Data, #rtp_channel{codec = aac, buffer = done} = RTP, Timecode) ->
  AUHeader = <<(size(Data)):13, 0:3>>,
  AULength = bit_size(AUHeader),
  AAC = <<AULength:16, AUHeader/binary, Data/binary>>,
  compose_rtp(RTP, AAC, Timecode);
  

encode_data(Data, #rtp_channel{codec = speex} = RTP, Timecode) ->
  compose_rtp(RTP, <<Data/binary, 16#7f:8 >>, Timecode);
  

encode_data(Data, #rtp_channel{codec = h264, length_size = LengthSize} = RTP, Timecode) ->
  FUA_NALS = lists:flatten([h264:fua_split(NAL, 1387) || NAL <- split_h264_frame(Data, LengthSize)]),
  compose_rtp(RTP, FUA_NALS, Timecode);
  
% encode_data(Data, #rtp_channel{codec = mpeg4} = RTP, Timecode) ->
%   compose_rtp(RTP, Data, 1388, Timecode);

encode_data(Data, #rtp_channel{} = RTP, Timecode) ->
  compose_rtp(RTP, Data, Timecode).


split_h264_frame(Frame, LengthSize) ->
  split_h264_frame(Frame, LengthSize, []).

split_h264_frame(<<>>, _LengthSize, Acc) ->
  lists:reverse(Acc);
split_h264_frame(<<Size:16, NAL:Size/binary, Rest/binary>>, 2, Acc) ->
  split_h264_frame(Rest, 2, [NAL|Acc]);
split_h264_frame(<<Size:32, NAL:Size/binary, Rest/binary>>, 4, Acc) ->
  split_h264_frame(Rest, 4, [NAL|Acc]).

compose_rtp(RTP, Bin, Timecode) when is_binary(Bin) ->
  compose_rtp(RTP, [Bin], Timecode);

compose_rtp(RTP, Parts, Timecode) ->
  compose_rtp(RTP, Parts, [], Timecode).

compose_rtp(RTP, [], Acc, _Timecode) ->
  {ok, RTP, lists:reverse(Acc)};
  
compose_rtp(#rtp_channel{sequence = Sequence} = RTP, [Part|Parts], Acc, Timecode) ->
  Pack = make_rtp_pack(RTP, case length(Parts) of 0 -> 1; _ -> 0 end, Part, Timecode),
  compose_rtp(RTP#rtp_channel{sequence = inc_seq(Sequence)}, Parts, [Pack|Acc], Timecode). 
  

make_rtp_pack(#rtp_channel{payload_type = PayloadType,
                        sequence = Sequence,
                        stream_id = SSRC}, Marker, Payload, Timecode) ->
  Version = 2,
  Padding = 0,
  Extension = 0,
  CSRC = 0,
  % ?D({rtp,Sequence,PayloadType,Timecode,SSRC}),
  <<Version:2, Padding:1, Extension:1, CSRC:4, Marker:1, PayloadType:7, Sequence:16, Timecode:32, SSRC:32, Payload/binary>>.


% %% Compose number of RTP-packets from splitten Data to Size
% compose_rtp(Base, Data, Size)
%   when is_integer(Size) ->
%   compose_rtp(Base, Data, Size, [], undefined).
% 
% compose_rtp(Base, <<>>, _, Acc, _) -> % Return new Sequence ID and list of RTP-binaries
%   %%?DBG("New Sequence: ~p", [Sequence]),
%   {Base#rtp_channel{marker = false}, lists:reverse(Acc)};
% compose_rtp(#rtp_channel{sequence = Sequence, marker = _Marker,
%                       packets = Packets, bytes = Bytes} = Base, Data, Size, Acc, Nal)
%   when (is_integer(Size) andalso (size(Data) > Size)) ->
%   <<P:Size/binary,Rest/binary>> = Data,
%   Start = if Acc == [] -> 1; true -> 0 end,
%   End = 0,
%   {PFrag, NewNal} = fragment_nal(P, Nal, Start, End),
%   M = 0,
%   Pack = make_rtp_pack(Base, M, PFrag),
%   compose_rtp(Base#rtp_channel{sequence = inc_seq(Sequence),
%                             packets = inc_packets(Packets, 1),
%                             bytes = inc_bytes(Bytes, size(Pack))}, Rest, Size, [Pack | Acc], NewNal);
% compose_rtp(#rtp_channel{sequence = Sequence, marker = Marker,
%                       packets = Packets, bytes = Bytes} = Base, Data, Size, Acc, Nal) ->
%   if Marker -> M = 1; true -> M = 0 end,
%   ResData =
%     if ((Acc == []) or (Nal == undefined)) ->
%         Data;
%        true ->
%         Start = 0, End = 1,
%         {FN, _Nal} = fragment_nal(Data, Nal, Start, End),
%         FN
%     end,
%   Pack = make_rtp_pack(Base, M, ResData),
%   compose_rtp(Base#rtp_channel{sequence = inc_seq(Sequence),
%                             packets = inc_packets(Packets, 1),
%                             bytes = inc_bytes(Bytes, size(Pack))}, <<>>, Size, [Pack | Acc], Nal).


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

