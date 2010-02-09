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
-define(TS_PACKET, 183). % 188 - 4 bytes of header - 1 byte syncword
-define(PMT_PID, 66).
-define(PCR_PID, 69).
-define(AUDIO_PID, 68).
-define(VIDEO_PID, 68).
-define(PAT_TABLEID, 0).
-define(PMT_TABLEID, 2).


play(_Name, Player, Req) ->
  ?D({"Player starting", _Name, Player}),
  Req:stream(head, [{"Content-Type", "video/mpeg2"}, {"Connection", "close"}]),
  process_flag(trap_exit, true),
  link(Req:socket_pid()),
  Player ! start,
  send_pat(Req),
  send_pmt(Req),
  ?MODULE:play(Req),
  % ?D({"MPEG TS", Req}),
  Req:stream(close),
  ok.

mux(Data, Req, Pid) ->
  Start = 1,
  Counter = 0,
  mux_parts(Data, Req, Pid, Start, Counter).
  
  
% 4 bytes header, 1 byte syncwork, 188 packet, so data is 183
mux_parts(<<Data:?TS_PACKET/binary, Rest/binary>>, Req, Pid, Start, Counter) ->
  TEI = 0,
  Priority = 0,
  Scrambling = 0,
  Adapt = 0,
  HasPayload = 1,
  % Adaptation = <<>>,
  % (size(Adaptation)), Adaptation/binary
  Part = <<16#47, TEI:1, Start:1, Priority:1, Pid:13, Scrambling:2, Adapt:1, HasPayload:1, Counter:4, Data/binary>>,
  Req:stream(Part),
  mux_parts(Rest, Req, Pid, 0, (Counter+1) rem 16);
  
mux_parts(<<>>, _Req, _Pid, _Start, _Counter) ->
  ok;

mux_parts(Data, Req, Pid, Start, Counter) ->
  TEI = 0,
  Priority = 0,
  Scrambling = 0,
  Adapt = 1,
  HasPayload = 1,
  Adaptation = padding(<<0>>, ?TS_PACKET - size(Data) - 1),
  Part = <<16#47, TEI:1, Start:1, Priority:1, Pid:13, Scrambling:2, Adapt:1, HasPayload:1, Counter:4, (size(Adaptation)), Adaptation/binary, Data/binary>>,
  Req:stream(Part).


padding(Padding, 0) -> Padding;
padding(Padding, Size) -> padding(<<Padding/binary, 255>>, Size - 1).
  
send_pat(Req) ->
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
  CRC32 = erlang:crc32(PAT1),
  PAT = <<PAT1/binary, CRC32:32>>,
  mux(PAT, Req, 0).

send_pmt(Req) ->
  _Pointer = 0,
  SectionSyntaxInd = 1,
  CRC32 = 0,
  SectionLength = 50,  % Don't know why, just from vlc output
  ProgramNum = 1,
  _Version = 12,
  _CurrentNext = 1,
  _SectionNumber = 0,
  _LastSectionNumber = 0,
  ProgramInfo = <<29,13,17,1,2,128,128,7,0,79,255,255,254,254,255>>,
  AudioES = <<>>,
  AudioStream = <<?TYPE_AUDIO_AAC, 2#111:3, ?AUDIO_PID:13, 0:4, (size(AudioES)):12, AudioES/binary>>,
  VideoES = <<>>,
  VideoStream = <<?TYPE_VIDEO_H264, 2#111:3, ?VIDEO_PID:13, 0:4, (size(VideoES)):12, VideoES/binary>>,
  Streams = iolist_to_binary([AudioStream, VideoStream]),
  PMT = <<_Pointer, ?PMT_TABLEID, SectionSyntaxInd:1, 0:1, 2#11:2, SectionLength:12, 
      ProgramNum:16, 0:2, _Version:5, _CurrentNext:1, _SectionNumber,
      _LastSectionNumber, 0:3, ?PCR_PID:13, 0:4, (size(ProgramInfo)):12, 
      ProgramInfo/binary, Streams/binary, CRC32:32>>,
  mux(PMT, Req, ?PMT_PID).

  % <<_Pointer, 2, _SectionInd:1, 0:1, 2#11:2, SectionLength:12, 
  %     ProgramNum:16, _:2, _Version:5, _CurrentNext:1, _SectionNumber,
  %     _LastSectionNumber, _:3, _PCRPID:13, _:4, ProgramInfoLength:12, 
  %     _ProgramInfo:ProgramInfoLength/binary, Streams/binary>> =  <<0,2,176,50,0,1,217,0,0,224,69,240,15,29,13,
  %                               17,1,2,128,128,7,0,79,255,255,254,254,255,15,
  %                               224,68,240,6,10,4,101,110,103,0,27,224,69,240,6,
  %                               10,4,101,110,103,0,219,45,131,210>>.
  % 
  
  
send_video(#video_frame{timestamp = Timestamp, body = Body}, Req) ->
  PtsDts = 2#11,
  HeaderSize = 16#0A,
  NAL = [<<1:24>>, Body, <<1:24>>],
  ESsize = iolist_size(NAL),
  PesHeaderSize = HeaderSize + 3 + ESsize,
  Alignment = 1,
  Pts = Timestamp * 90,
  <<Pts1:3, Pts2:15, Pts3:15>> = <<Pts:33>>,
  PESHeader = <<1:24, ?TYPE_VIDEO_H264, PesHeaderSize:16, 2:2, 0:2, 0:1, 
                Alignment:1, 0:1, 0:1, PtsDts:2, 0:6, HeaderSize:8,
                PtsDts:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1,
                1:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1>>,
  PES = iolist_to_binary([PESHeader | NAL]),
  mux(PES, Req, ?VIDEO_PID).
  
play(Req) ->
  receive
    #video_frame{type = video} = Frame ->
      send_video(Frame, Req),
      ?MODULE:play(Req);
    #video_frame{} = _Frame ->
      % Req:stream(<<"frame\n">>),
      ?MODULE:play(Req);
    {'EXIT', _, _} ->
      ?D({"MPEG TS reader disconnected"}),
      ok;
    Message -> 
      ?D(Message),
      ?MODULE:play(Req)
  after
    ?TIMEOUT ->
      ?D("MPEG TS player stopping"),
      ok
  end.
  
  

  
  
  
  
  
  
  