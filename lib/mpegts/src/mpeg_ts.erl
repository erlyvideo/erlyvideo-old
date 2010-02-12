%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        MPEG TS stream module
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%%
%%% Copyright (c) 2009 Max Lapshin
%%%    This program is free software: you can redistribute it and/or modify
%%%    it under the terms of the GNU Affero General Public License as
%%%    published by the Free Software Foundation, either version 3 of the
%%%    License, or any later version.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------

-module(mpeg_ts).
-author('Max Lapshin <max@maxidoors.ru>').
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-define(TIMEOUT, 4000).
-include_lib("erlyvideo/include/video_frame.hrl").
-include("mpegts.hrl").

-export([play/3, play/1]).
-define(TS_PACKET, 184). % 188 - 4 bytes of header
-define(PAT_PID, 0).
-define(PMT_PID, 66).
-define(PCR_PID, 69).
-define(AUDIO_PID, 68).
-define(VIDEO_PID, 69).
-define(PAT_TABLEID, 0).
-define(PMT_TABLEID, 2).


-record(streamer, {
  player,
  req,
  pat_counter = 0,
  pmt_counter = 0,
  audio_counter = 0,
  video_counter = 0,
  length_size = 2,
  audio_config = undefined,
  video_config = undefined
}).

play(_Name, Player, Req) ->
  ?D({"Player starting", _Name, Player}),
  process_flag(trap_exit, true),
  link(Player),
  link(Req:socket_pid()),
  Player ! start,
  Streamer = #streamer{player = Player, req = Req},
  Streamer1 = send_pat(Streamer),
  ?MODULE:play(Streamer1),
  Req:stream(close),
  ok.

mux(Data, Streamer, Pid) ->
  Start = 1,
  mux_parts(Data, Streamer, Pid, Start).
  
increment_counter(#streamer{pat_counter = C} = Streamer, ?PAT_PID) ->
  {C, Streamer#streamer{pat_counter = (C + 1) rem 16}};
increment_counter(#streamer{pmt_counter = C} = Streamer, ?PMT_PID) ->
  {C, Streamer#streamer{pmt_counter = (C + 1) rem 16}};
increment_counter(#streamer{audio_counter = C} = Streamer, ?AUDIO_PID) ->
  {C, Streamer#streamer{audio_counter = (C + 1) rem 16}};
increment_counter(#streamer{video_counter = C} = Streamer, ?VIDEO_PID) ->
  {C, Streamer#streamer{video_counter = (C + 1) rem 16}}.
  
% 4 bytes header, 1 byte syncwork, 188 packet, so data is 183


adaptation_field(Data) when size(Data) >= ?TS_PACKET -> 
  {0, <<>>};
  
adaptation_field(Data) when is_binary(Data) ->
  Field = padding(<<0>>, ?TS_PACKET - size(Data) - 2),
  {1, <<(size(Field)), Field/binary>>};

adaptation_field({Timestamp, Data}) ->
  PCR = Timestamp * 27000,
  PCR1 = round(PCR / 300),
  PCR2 = PCR rem 300,
  AdaptationMinLength = 1 + 1 + 6,

  Adaptation = <<0:1, 0:1, 0:1, 1:1, 0:4, PCR1:33, PCR2:9, 0:6>>,
  Field = padding(Adaptation, ?TS_PACKET - AdaptationMinLength - size(Data)),
  {1, <<(size(Field)), Field/binary>>}.


mux_parts(Data, Streamer, Pid, Start) ->
  {Adaptation, Field} = adaptation_field(Data),
  {Counter, Streamer1} = increment_counter(Streamer, Pid),
  HasPayload = 1,
  Scrambling = 0,
  Priority = 0,
  TEI = 0,

  Header = <<16#47, TEI:1, Start:1, Priority:1, Pid:13, Scrambling:2, Adaptation:1, HasPayload:1, Counter:4, Field/binary>>,
  Payload = case Data of
    {_, Bin} -> Bin;
    _ -> Data
  end,
  send_ts(Header, Payload, Streamer1, Pid).
  
send_ts(Header, Data, #streamer{req = Req} = Streamer, _) when size(Data) == 188 - size(Header) ->
  Req:stream(<<Header/binary, Data/binary>>),
  Streamer;

send_ts(Header, Data, #streamer{req = Req} = Streamer, Pid) when size(Data) > 188 - size(Header) ->
  Length = 188 - size(Header),
  <<Packet:Length/binary, Rest/binary>> = Data,
  Req:stream(<<Header/binary, Packet/binary>>),
  mux_parts(Rest, Streamer, Pid, 0).
  

padding(Padding, Size) when Size =< 0 -> Padding;
padding(Padding, Size) when Size > 0 -> padding(<<Padding/binary, 255>>, Size - 1).
  
send_pat(Streamer) ->
  Programs = <<1:16, 111:3, ?PMT_PID:13>>,
  TSStream = 29998, % Just the same, as VLC does
  Reserved = 0,
  Version = 2,
  CNI = 1,
  Section = 0,
  LastSection = 0,
  Misc = <<Reserved:2, Version:5, CNI:1, Section, LastSection>>,
  Length = size(Programs)+5+4,
  PAT1 = <<?PAT_TABLEID, 2#1011:4, Length:12, TSStream:16, Misc/binary, Programs/binary>>,
  CRC32 = mpeg2_crc32:crc32(PAT1),
  PAT = <<0, PAT1/binary, CRC32:32>>,
  mux(PAT, Streamer, 0).

send_pmt(#streamer{video_config = _VideoConfig} = Streamer) ->
  SectionSyntaxInd = 1,
  ProgramNum = 1,
  Version = 0,
  CurrentNext = 1,
  _SectionNumber = 0,
  _LastSectionNumber = 0,
  
  % Some hardcoded output from VLC
  ProgramInfo = <<29,13,17,1,2,128,128,7,0,79,255,255,254,254,255>>,
  
  AudioES = <<>>,
  AudioStream = <<?TYPE_AUDIO_AAC, 2#111:3, ?AUDIO_PID:13, 0:4, (size(AudioES)):12, AudioES/binary>>,
  
  % MultipleFrameRate = 0,
  % FrameRateCode = 0,
  % MPEG1Only = 0,
  % ProfileLevel = 0,
  % Chroma = 0,
  % FrameRateExt = 0,
  % VideoES = <<2, (size(VideoConfig)+3), MultipleFrameRate:1, FrameRateCode:4, MPEG1Only:1,
  %             0:1, 0:1, ProfileLevel, Chroma:2, FrameRateExt:1, 0:5,    VideoConfig/binary>>,
  VideoES = <<>>,
  VideoStream = <<?TYPE_VIDEO_H264, 2#111:3, ?VIDEO_PID:13, 0:4, (size(VideoES)):12, VideoES/binary>>,
  Streams = iolist_to_binary([AudioStream, VideoStream]),
  PMT1 = <<ProgramNum:16, 
           11:2, Version:5, CurrentNext:1, 
           _SectionNumber,
           _LastSectionNumber, 
           0:3, ?PCR_PID:13, 
           0:4, (size(ProgramInfo)):12, 
           ProgramInfo/binary, 
           Streams/binary>>,

  SectionLength = size(PMT1) + 4, % Add CRC32
  PMT2 = <<?PMT_TABLEID, SectionSyntaxInd:1, 0:1, 2#11:2, SectionLength:12, PMT1/binary>>,

  CRC32 = mpeg2_crc32:crc32(PMT2),
  PMT = <<0, PMT2/binary, CRC32:32>>,
  mux(PMT, Streamer, ?PMT_PID).

  % <<_Pointer, 2, _SectionInd:1, 0:1, 2#11:2, SectionLength:12, 
  %     ProgramNum:16, _:2, _Version:5, _CurrentNext:1, _SectionNumber,
  %     _LastSectionNumber, _:3, _PCRPID:13, _:4, ProgramInfoLength:12, 
  %     _ProgramInfo:ProgramInfoLength/binary, Streams/binary>> =  <<0,2,176,50,0,1,217,0,0,224,69,240,15,29,13,
  %                               17,1,2,128,128,7,0,79,255,255,254,254,255,15,
  %                               224,68,240,6,10,4,101,110,103,0,27,224,69,240,6,
  %                               10,4,101,110,103,0,219,45,131,210>>.
  % 
  
  
send_video(Streamer, #video_frame{timestamp = Timestamp, body = Body}) ->
  PtsDts = 2#11,
  Marker = 2#10,
  Scrambling = 0,
  Alignment = 1,
  Pts = Timestamp * 90,
  <<Pts1:3, Pts2:15, Pts3:15>> = <<Pts:33>>,
  AddPesHeader = <<PtsDts:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1,
                   1:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1>>,
  PesHeader = <<Marker:2, Scrambling:2, 0:1,
                Alignment:1, 0:1, 0:1, PtsDts:2, 0:6, (size(AddPesHeader)):8, AddPesHeader/binary>>,
  % ?D({"Sending nal", Body}),
  PES = <<1:24, ?TYPE_VIDEO_H264, (size(PesHeader) + size(Body) + 4):16, PesHeader/binary, 1:24, Body/binary, 0>>,
  mux({Timestamp, PES}, Streamer, ?VIDEO_PID).


send_audio(#streamer{audio_config = AudioConfig} = Streamer, #video_frame{timestamp = Timestamp, body = Body}) ->
  PtsDts = 2#11,
  Marker = 2#10,
  Scrambling = 0,
  Alignment = 0,
  Pts = Timestamp * 90,
  <<Pts1:3, Pts2:15, Pts3:15>> = <<Pts:33>>,
  AddPesHeader = <<PtsDts:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1,
                   1:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1>>,
  PesHeader = <<Marker:2, Scrambling:2, 0:1,
                Alignment:1, 0:1, 0:1, PtsDts:2, 0:6, (size(AddPesHeader)):8, AddPesHeader/binary>>,
  % ?D({"Sending audio", Timestamp, Body}),
  ADTS = aac:encode(Body, AudioConfig),
  
  % PES = <<1:24, ?TYPE_AUDIO_AAC, (size(PesHeader) + size(ADTS)):16, PesHeader/binary, ADTS/binary>>,
  PES = <<1:24, ?TYPE_AUDIO_AAC, 0:16, PesHeader/binary, ADTS/binary>>,
  mux({Timestamp, PES}, Streamer, ?AUDIO_PID).


send_video_config(#streamer{video_config = Config} = Streamer) ->
  F = fun(NAL, S) ->
    send_video(S, #video_frame{type = video, timestamp = 0, decoder_config = true, body = NAL})
  end,
  {_LengthSize, NALS} = h264:unpack_config(Config),
  lists:foldl(F, Streamer, NALS).
  

play(#streamer{player = Player, video_config = undefined} = Streamer) ->
  receive
    #video_frame{type = video, decoder_config = true, body = Config} ->
      Streamer1 = send_pmt(Streamer#streamer{video_config = Config}),
      {LengthSize, _} = h264:unpack_config(Config),
      Streamer2 = send_video_config(Streamer1#streamer{length_size = LengthSize*8}),
      ?MODULE:play(Streamer2)
  after
    ?TIMEOUT ->
      ?D("No video decoder config received"),
      Player ! stop,
      ok
  end;

play(#streamer{player = Player, audio_config = undefined} = Streamer) ->
  receive
    #video_frame{type = audio, decoder_config = true, body = AudioConfig} ->
      Config = aac:decode_config(AudioConfig),
      ?D({"Audio config", Config}),
      ?MODULE:play(Streamer#streamer{audio_config = Config})
  after
    ?TIMEOUT ->
      ?D("No audio decoder config received"),
      Player ! stop,
      ok
  end;
  
play(#streamer{player = Player, length_size = LengthSize} = Streamer) ->
  receive
    #video_frame{type = video, frame_type = keyframe, body = <<Length:LengthSize, NAL:Length/binary>>} = Frame->
      % Streamer1 = send_video_config(Streamer),
      % <<Length:LengthSize, NAL:Length/binary>> = Body,
      Streamer2 = send_video(Streamer, Frame#video_frame{body = NAL}),
      ?MODULE:play(Streamer2);
    #video_frame{type = video, body = <<Length:LengthSize, NAL:Length/binary>>} = Frame ->
      % <<Length:LengthSize, NAL:Length/binary>> = Body,
      Streamer1 = send_video(Streamer, Frame#video_frame{body = NAL}),
      ?MODULE:play(Streamer1);
    #video_frame{type = audio} = Frame ->
      Streamer1 = send_audio(Streamer, Frame),
      ?MODULE:play(Streamer1);
    {'EXIT', _, _} ->
      ?D({"MPEG TS reader disconnected"}),
      Player ! stop,
      ok;
    Message -> 
      ?D(Message),
      ?MODULE:play(Streamer)
  after
    ?TIMEOUT ->
      ?D("MPEG TS player stopping"),
      Player ! stop,
      ok
  end.
  
  

  
  
  
  
  
  
  