%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        MPEG TS stream module
%%% Links:
%%%  http://dvd.sourceforge.net/dvdinfo/pes-hdr.html
%%%  http://en.wikipedia.org/wiki/MPEG-TS
%%%  http://en.wikipedia.org/wiki/Packetized_Elementary_Stream
%%%  http://en.wikipedia.org/wiki/Program_Specific_Information
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

-module(mpegts).
-author('Max Lapshin <max@maxidoors.ru>').
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-define(TIMEOUT, 4000).
-include_lib("erlmedia/include/video_frame.hrl").
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
  ?MODULE:play(Streamer),
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
  Field = padding(<<0>>, ?TS_PACKET - size(Data) - 1),
  {1, <<(size(Field)), Field/binary>>};

adaptation_field({Timestamp, Data}) ->
  adaptation_field({Timestamp, 0, Data});
  
adaptation_field({Timestamp, Keyframe, Data}) ->
  PCR = round(Timestamp * 27000),
  PCR1 = round(Timestamp * 90),
  PCR2 = PCR rem 300,
  % ?D({"PCR", PCR}),
  Discontinuity = 0,
  RandomAccess = Keyframe,
  Priority = 0,
  HasPCR = 1,
  HasOPCR = 0,
  Splice = 0,
  Private = 0,
  Ext = 0,

  Adaptation = <<Discontinuity:1, RandomAccess:1, Priority:1, HasPCR:1, HasOPCR:1, Splice:1, Private:1, Ext:1, PCR1:33, 2#111111:6, PCR2:9>>,
  Field = padding(Adaptation, ?TS_PACKET - 1 - size(Data)),
  % ?D({"Adaptation PCR", Timestamp}),
  % ?D({"Adapt", size(Adaptation), size(Field)+1, size(Data)}),
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
    {_, _, Bin} -> Bin;
    {_, Bin} -> Bin;
    _ -> Data
  end,
  send_ts(Header, Payload, Streamer1, Pid).
  
send_ts(Header, Data, #streamer{req = Req} = Streamer, _Pid) when size(Data) + size(Header) == 188 ->
  % ?D({"TS packet", _Pid, size(<<Header/binary, Data/binary>>)}),
  Req:stream(<<Header/binary, Data/binary>>),
  Streamer;

send_ts(Header, Data, #streamer{req = Req} = Streamer, Pid) when size(Data) + size(Header) > 188  ->
  Length = 188 - size(Header),
  <<Packet:Length/binary, Rest/binary>> = Data,
  % ?D({"TS packet", Pid, size(<<Header/binary, Packet/binary>>), size(Rest)}),
  Req:stream(<<Header/binary, Packet/binary>>),
  mux_parts(Rest, Streamer, Pid, 0).
  

padding(Padding, Size) when size(Padding) >= Size -> Padding;
padding(Padding, Size) when size(Padding) < Size -> 
  Padder = <<255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
             255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
             255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
             255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
             255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
             255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
             255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
             255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
             255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
             255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
             255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
             255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
             255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
             255,255,255,255,255,255,255,255>>,
  {Pad, _} = split_binary(Padder, Size - size(Padding)),
  padding(<<Padding/binary, Pad/binary>>, Size - size(Pad)).
  
send_pat(Streamer, _DTS) ->
  Programs = <<1:16, 111:3, ?PMT_PID:13>>,
  TSStream = 29998, % Just the same, as VLC does
  Version = 2,
  CNI = 1,
  Section = 0,
  LastSection = 0,
  Misc = <<2#11:2, Version:5, CNI:1, Section, LastSection>>,
  Length = size(Programs)+5+4,
  PAT1 = <<?PAT_TABLEID, 2#1011:4, Length:12, TSStream:16, Misc/binary, Programs/binary>>,
  CRC32 = mpeg2_crc32:crc32(PAT1),
  PAT = <<0, PAT1/binary, CRC32:32>>,
  PATBin = padding(PAT, ?TS_PACKET),
  % ?D({"Sending PAT", size(PAT), size(PATBin)}),
  mux(PATBin, Streamer, 0).

send_pmt(#streamer{video_config = _VideoConfig} = Streamer, _DTS) ->
  SectionSyntaxInd = 1,
  ProgramNum = 1,
  Version = 0,
  CurrentNext = 1,
  _SectionNumber = 0,
  _LastSectionNumber = 0,
  
  % Some hardcoded output from VLC
  IOD = <<17,1,2,128,128,7,0,79,255,255,254,254,255>>,
  
  
  %% FIXME: Program info is not just for IOD, but also for other descriptors
  %% Look at libdvbpsi/src/tables/pmt.c:468
  _ProgramInfo1 = <<?DESCRIPTOR_IOD, (size(IOD)), IOD/binary>>,
  ProgramInfo = <<>>,
  
  %% FIXME: Here also goes the same descriptor as in ProgramInfo
  %% libdvbpsi/src/tables/pmt.c:499
  %% Also, look at mp4:esds_tag, here goes the same content
  %%
  %% It is required to add audio config here, if we don't want to see 
  %% "MPEG-4 descriptor not found" from VLC
  %% Code, that read it is in vlc/modules/demux/ts.c:3177
  %%
  _AudioES1 = <<?DESCRIPTOR_SL, 2, 1:16>>, % means, 2 byte and ES ID = 1
  AudioES = <<>>,
  AudioStream = <<?TYPE_AUDIO_AAC, 2#111:3, ?AUDIO_PID:13, 2#1111:4, (size(AudioES)):12, AudioES/binary>>,
  
  % MultipleFrameRate = 0,
  % FrameRateCode = 0,
  % MPEG1Only = 0,
  % ProfileLevel = 0,
  % Chroma = 0,
  % FrameRateExt = 0,
  % VideoES = <<2, (size(VideoConfig)+3), MultipleFrameRate:1, FrameRateCode:4, MPEG1Only:1,
  %             0:1, 0:1, ProfileLevel, Chroma:2, FrameRateExt:1, 0:5,    VideoConfig/binary>>,
  VideoES = <<>>,
  VideoStream = <<?TYPE_VIDEO_H264, 2#111:3, ?VIDEO_PID:13, 2#1111:4, (size(VideoES)):12, VideoES/binary>>,
  Streams = iolist_to_binary([AudioStream, VideoStream]),
  Program = <<ProgramNum:16, 
           2#11:2, Version:5, CurrentNext:1, 
           _SectionNumber,
           _LastSectionNumber, 
           2#111:3, ?PCR_PID:13, 
           2#1111:4, (size(ProgramInfo)):12, 
           ProgramInfo/binary, 
           Streams/binary>>,
           
  Programs = Program, % Only one program for now
  SectionLength = size(Programs) + 4, % Add CRC32
  PMT = <<?PMT_TABLEID, SectionSyntaxInd:1, 0:1, 2#11:2, SectionLength:12, Programs/binary>>,

  CRC32 = mpeg2_crc32:crc32(PMT),
  PMTBin = <<0, PMT/binary, CRC32:32>>,
  mux(padding(PMTBin, ?TS_PACKET), Streamer, ?PMT_PID).

  % <<_Pointer, 2, _SectionInd:1, 0:1, 2#11:2, SectionLength:12, 
  %     ProgramNum:16, _:2, _Version:5, _CurrentNext:1, _SectionNumber,
  %     _LastSectionNumber, _:3, _PCRPID:13, _:4, ProgramInfoLength:12, 
  %     _ProgramInfo:ProgramInfoLength/binary, Streams/binary>> =  <<0,2,176,50,0,1,217,0,0,224,69,240,15,29,13,
  %                               17,1,2,128,128,7,0,79,255,255,254,254,255,15,
  %                               224,68,240,6,10,4,101,110,103,0,27,224,69,240,6,
  %                               10,4,101,110,103,0,219,45,131,210>>.
  % 
  
  
send_video(Streamer, #video_frame{dts = DTS, pts = PTS, body = Body, frame_type = FrameType}) ->
  Marker = 2#10,
  Scrambling = 0,
  Priority = 0,
  Alignment = 1,
  Copyright = 0,
  Original = 0,
  
  ESCR = 0,
  ESRate = 0,
  DSMTrick = 0,
  CopyInfo = 0,
  CRC = 0,
  Extension = 0,
  

  % ?D({"Video", PTS, DTS, "--", PTS*90, DTS*90}),
  DTS1 = round(DTS*90),
  PTS1 = round(PTS*90),
  <<Pts1:3, Pts2:15, Pts3:15>> = <<PTS1:33>>,
  <<Dts1:3, Dts2:15, Dts3:15>> = <<DTS1:33>>,

  case DTS1 of
    PTS1 ->
      PtsDts = 2#10,
      AddPesHeader = <<2#10:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1>>;
    _ ->
      PtsDts = 2#11,
      AddPesHeader = <<2#0011:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1, 
                       2#0001:4, Dts1:3, 1:1, Dts2:15, 1:1, Dts3:15, 1:1>>
  end,
  PesHeader = <<Marker:2, Scrambling:2, Priority:1, Alignment:1, Copyright:1, Original:1, PtsDts:2, 
                ESCR:1,ESRate:1,DSMTrick:1,CopyInfo:1,CRC:1,Extension:1,  % All these bits are usually zero
                (size(AddPesHeader)):8, AddPesHeader/binary>>,
  % ?D({"Sending nal", Body}),
  NALHeader = <<1:32>>,
  PES = <<1:24, ?MPEGTS_STREAMID_H264, (size(PesHeader) + size(Body) + size(NALHeader)):16, PesHeader/binary, NALHeader/binary, Body/binary>>,
  
  Keyframe = case FrameType of
    keyframe -> 1;
    _ -> 0
  end,
  mux({DTS, Keyframe, PES}, Streamer, ?VIDEO_PID).


send_audio(#streamer{audio_config = AudioConfig} = Streamer, #video_frame{dts = Timestamp, body = Body}) ->
  PtsDts = 2#10,
  Marker = 2#10,
  Scrambling = 0,
  Alignment = 0,
  % ?D({"Audio", Timestamp}),
  Pts = round(Timestamp * 90),
  <<Pts1:3, Pts2:15, Pts3:15>> = <<Pts:33>>,
  AddPesHeader = <<PtsDts:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1>>,
  PesHeader = <<Marker:2, Scrambling:2, 0:1,
                Alignment:1, 0:1, 0:1, PtsDts:2, 0:6, (size(AddPesHeader)):8, AddPesHeader/binary>>,
  % ?D({"Audio", Timestamp}),
  ADTS = aac:encode(Body, AudioConfig),
  
  PES = <<1:24, ?MPEGTS_STREAMID_AAC, (size(PesHeader) + size(ADTS)):16, PesHeader/binary, ADTS/binary>>,
  % PES = <<1:24, ?TYPE_AUDIO_AAC, 0:16, PesHeader/binary, ADTS/binary>>,
  mux({Timestamp, PES}, Streamer, ?AUDIO_PID).


send_video_config(#streamer{video_config = Config} = Streamer, DTS) ->
  F = fun(NAL, S) ->
    send_video(S, #video_frame{type = video, dts = DTS, pts = DTS, frame_type = keyframe, decoder_config = true, body = NAL})
  end,
  {_LengthSize, NALS} = h264:unpack_config(Config),
  lists:foldl(F, Streamer, NALS).
  

play(#streamer{player = Player, video_config = undefined} = Streamer) ->
  receive
    #video_frame{type = video, decoder_config = true, body = Config, dts = DTS} = Frame ->
      Streamer1 = send_pat(Streamer, DTS),

      Streamer2 = send_pmt(Streamer1#streamer{video_config = Config}, DTS),
      {LengthSize, _} = h264:unpack_config(Config),
      ?D({"Set length size", LengthSize}),
      Streamer3 = send_video(Streamer2, Frame#video_frame{body = <<9, 16#E0>>}), %H264 NAL_DELIM
      Streamer4 = send_video_config(Streamer3#streamer{length_size = LengthSize*8}, DTS),
      ?MODULE:play(Streamer4)
  after
    ?TIMEOUT ->
      ?D("No video decoder config received"),
      Player ! stop,
      ok
  end;

play(#streamer{audio_config = undefined} = Streamer) ->
  receive
    #video_frame{type = audio, decoder_config = true, body = AudioConfig} ->
      Config = aac:decode_config(AudioConfig),
      ?D({"Audio config", Config}),
      ?MODULE:play(Streamer#streamer{audio_config = Config})
  after
    ?TIMEOUT ->
      ?D("No audio decoder config received"),
      % Player ! stop,
      % ok
      ?MODULE:play(Streamer#streamer{audio_config = <<>>})
  end;
  
play(#streamer{player = Player, length_size = LengthSize} = Streamer) ->
  receive
    #video_frame{type = video, frame_type = keyframe, body = <<Length:LengthSize, NAL:Length/binary>>, dts = _DTS} = Frame->
      Streamer1 = send_video_config(Streamer, _DTS),
      % <<Length:LengthSize, NAL:Length/binary>> = Body,
      % Streamer1 = Streamer,
      Streamer2 = send_video(Streamer1, Frame#video_frame{body = NAL, frame_type = frame}),
      ?MODULE:play(Streamer2);
    #video_frame{type = video, frame_type = keyframe, dts = _DTS} = Frame->
      % Streamer1 = send_video_config(Streamer, DTS),
      % <<Length:LengthSize, NAL:Length/binary>> = Body,
      Streamer1 = Streamer,
      Streamer2 = send_video(Streamer1, Frame#video_frame{frame_type = frame}),
      ?MODULE:play(Streamer2);
    #video_frame{type = video, body = Body} = Frame ->
      <<Length:LengthSize, NAL:Length/binary, Rest/binary>> = Body,
      case size(Rest) of
        0 -> ok;
        _Remain -> 
          self() ! Frame#video_frame{body = Rest}
      end,
      Streamer1 = send_video(Streamer, Frame#video_frame{body = NAL}),
      ?MODULE:play(Streamer1);
    #video_frame{type = audio} = Frame ->
      Streamer1 = send_audio(Streamer, Frame),
      ?MODULE:play(Streamer1);
    #video_frame{type = metadata} ->
      ?MODULE:play(Streamer);
    {'EXIT', _, _} ->
      ?D({"MPEG TS reader disconnected"}),
      Player ! stop,
      ok;
    Message -> 
      ?D({LengthSize, Message}),
      ?MODULE:play(Streamer)
  after
    ?TIMEOUT ->
      ?D("MPEG TS player stopping"),
      Player ! stop,
      ok
  end.
  
  

  
  
  
  
  
  
  