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
-module(rtp_decoder).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("erlmedia/include/h264.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("erlmedia/include/sdp.hrl").
-include("rtp.hrl").
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(h264_buffer, {
  time,
  h264,
  buffer,
  flavor
}).

-export([init/1, decode/2, sync/2]).

init(#stream_info{codec = Codec, timescale = Scale} = Stream) ->
  #rtp_channel{codec = Codec, stream_info = Stream, timescale = Scale}.

sync(#rtp_channel{} = RTP, Headers) ->
  Seq = proplists:get_value(seq, Headers),
  Time = proplists:get_value(rtptime, Headers),
  % ?D({sync, Headers}),
  RTP#rtp_channel{wall_clock = 0, timecode = Time, sequence = Seq}.

decode(_, #rtp_channel{timecode = TC, wall_clock = Clock} = RTP) when TC == undefined orelse Clock == undefined ->
  %% ?D({unsynced, RTP}),
  {ok, RTP#rtp_channel{timecode = 0, wall_clock = 0, sequence = 0}, []};

decode(<<_:16, Sequence:16, _/binary>> = Data, #rtp_channel{sequence = undefined} = RTP) ->
  decode(Data, RTP#rtp_channel{sequence = Sequence});

decode(<<_:16, OldSeq:16, _/binary>>, #rtp_channel{sequence = Sequence} = RTP) when OldSeq < Sequence ->
  ?D({drop_sequence, OldSeq, Sequence}),
  {ok, RTP, []};

decode(<<2:2, 0:1, _Extension:1, 0:4, _Marker:1, _PayloadType:7, Sequence:16, Timecode:32, _StreamId:32, Data/binary>>, #rtp_channel{} = RTP) ->
  decode(Data, RTP#rtp_channel{sequence = (Sequence + 1) rem 65536}, Timecode).


decode(<<AULength:16, AUHeaders:AULength/bitstring, AudioData/binary>>, #rtp_channel{codec = aac} = RTP, Timecode) ->
  decode_aac(AudioData, AUHeaders, RTP, Timecode, []);

decode(Body, #rtp_channel{codec = h264, buffer = Buffer} = RTP, Timecode) ->
  DTS = timecode_to_dts(RTP, Timecode),
  {ok, Buffer1, Frames} = decode_h264(Body, Buffer, DTS),
  % ?D({decode,h264,Timecode,DTS, length(Frames), size(Body), size(Buffer1#h264_buffer.buffer)}),
  {ok, RTP#rtp_channel{buffer = Buffer1}, Frames};

decode(Body, #rtp_channel{codec = mpegts, buffer = Decoder} = RTP, _Timecode) ->
  {ok, Decoder1, Frames} = rtp_mpegts:decode(Body, Decoder),
  {ok, RTP#rtp_channel{buffer = Decoder1}, Frames};

decode(Body, #rtp_channel{stream_info = #stream_info{codec = Codec, content = Content} = Info} = RTP, Timecode) ->
  DTS = timecode_to_dts(RTP, Timecode),
  Frame = #video_frame{
    content = Content,
    dts     = DTS,
    pts     = DTS,
    body    = Body,
	  codec	  = Codec,
	  flavor  = frame,
	  sound	  = video_frame:frame_sound(Info)
  },
  {ok, RTP, [Frame]}.


% FIXME:
% Тут надо по-другому. 
% Надо аккумулировать все RTP-пейлоады с одним Timecode
% Потом проходить по ним депакетизатором FUA. Отрефакторить это в h264 в depacketize
% Потом взять результирующие NAL-юниты, склеить их в один или два кадра.
% SPS, PPS оформить в конфиг, остальное положить в один кадр. Бевард зачем-то шлет два полукадра.
%
% decode_h264(Body, OldDts, OldDts) -> accumulate
% decode_h264(NewBody, OldDts, NewDts) ->
%   depacketize(Accum)
%   split_into_frames(NALS)
%   flush_buffer
%   accumulate(Body, NewDts)
%   return_frames_and_new_buffer
%
decode_h264(Body, #h264_buffer{time = OldDTS} = RTP, DTS) when OldDTS > DTS ->
  Reply = case h264:decode_nal(Body, h264:init()) of
    {#h264{buffer = undefined}, Frames} -> [F#video_frame{dts = DTS, pts = DTS} || F <- Frames];
    _ ->
      ?D({drop_late_h264, DTS, OldDTS, h264:type(Body), size(Body)}),
      []
  end,
  {ok, RTP, Reply};

decode_h264(Body, undefined, DTS) ->
%   decode_h264(Body, #h264_buffer{}, DTS);
%
% decode_h264(_Body, #h264_buffer{time = undefined} = RTP, DTS) ->
  % {ok, RTP#h264_buffer{time = DTS}, []}; % Here we are entering sync-wait state which will last till current inteleaved frame is over
  % ?D(init_h264_buffer),
  decode_h264(Body, #h264_buffer{h264 = h264:init(), time = DTS, buffer = <<>>}, DTS);

% decode_h264(_Body, #h264_buffer{time = OldDTS, h264 = undefined} = RTP, DTS) when OldDTS =/= DTS ->
%   {ok, RTP#h264_buffer{time = DTS, h264 = h264:init(), buffer = <<>>}, []};
%
% decode_h264(_Body, #h264_buffer{time = DTS, h264 = undefined} = RTP, DTS) ->
%   {ok, RTP, []};

decode_h264(Body, #h264_buffer{h264 = H264, time = DTS, buffer = Buffer, flavor = Flavor} = RTP, DTS) ->
  {H264_1, Frames} = h264:decode_nal(Body, H264),
  Buf1 = lists:foldl(fun(#video_frame{body = AVC}, Buf) -> <<Buf/binary, AVC/binary>> end, Buffer, Frames),
  % ?D({avc, h264:type(Body), [{F#video_frame.flavor, size(F#video_frame.body)} || F <- Frames], size(Buf1)}),
  [F#video_frame.flavor =/= undefined orelse erlang:error(h264_decoder_flavor_undefined) || F <- Frames],
  Flavor1 = case Frames of
    [#video_frame{flavor = Fl}|_] -> Fl;
    [] -> Flavor
  end,
  % ?D({h264_decode_fragment, DTS, Flavor1, size(Buf1)}),
  {ok, RTP#h264_buffer{h264 = H264_1, buffer = Buf1, flavor = Flavor1}, []};


% decode_h264(Body, #h264_buffer{time = OldDTS, buffer = <<>>} = RTP, DTS) when OldDTS < DTS ->
%   ?D(zerobuf),
%   decode_h264(Body, RTP#h264_buffer{h264 = h264:init(), time = DTS}, DTS);


decode_h264(Body, #h264_buffer{h264 = OldH264, time = OldDTS, buffer = Buffer, flavor = OldFlavor} = RTP, DTS) when OldDTS < DTS ->

   % orelse erlang:error({non_decoded_h264_left, OldH264, Buffer}),

  {H264, Flavor} = case {OldH264#h264.buffer, Buffer} of
    {<<>>, <<>>} -> {undefined, undefined};
    {undefined, <<>>} -> {undefined, undefined};
    {Bin, <<>>} ->
      {_, [#video_frame{flavor = Fl}]} = h264:decode_nal(Bin, OldH264),
      {<<(size(Bin)):32, Bin/binary>>, Fl};
    {_, Bin} when size(Bin) > 0 -> {Bin, OldFlavor}
  end,

  Frames = case H264 of
    undefined -> [];
    _ ->
      Flavor =/= undefined orelse erlang:error({h264_frame_flavor_undefined, size(Buffer)}),
      [#video_frame{
        content = video,
        codec = h264,
        body = H264,
        flavor = Flavor,
        dts = OldDTS,
        pts = OldDTS
      }]
  end,

  {ok, RTP1, []} = decode_h264(Body, RTP#h264_buffer{h264 = h264:init(), flavor = undefined, time = DTS, buffer = <<>>}, DTS),
  {ok, RTP1, Frames}.


decode_aac(<<>>, <<>>, RTP, _, Frames) ->
  {ok, RTP, lists:reverse(Frames)};

decode_aac(AudioData, <<AUSize:13, _Delta:3, AUHeaders/bitstring>>, RTP, Timecode, Frames) ->
  <<Body:AUSize/binary, Rest/binary>> = AudioData,
  DTS = timecode_to_dts(RTP, Timecode),
  Frame = #video_frame{
    content = audio,
    dts     = DTS,
    pts     = DTS,
    body    = Body,
	  codec	  = aac,
	  flavor  = frame,
	  sound	  = {stereo, bit16, rate44}
  },
  decode_aac(Rest, AUHeaders, RTP, Timecode + 1024, [Frame|Frames]).

timecode_to_dts(#rtp_channel{timescale = Scale, timecode = BaseTimecode, wall_clock = WallClock}, Timecode) ->
  % ?D({tdts, WallClock, BaseTimecode, Scale, WallClock + (Timecode - BaseTimecode)/Scale, Timecode}),
  WallClock + (Timecode - BaseTimecode)/Scale.










