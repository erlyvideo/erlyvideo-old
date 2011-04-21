%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2011 Max Lapshin
%%% @doc        MPEG TS stream module
%%% Links:
%%%  http://dvd.sourceforge.net/dvdinfo/pes-hdr.html
%%%  http://en.wikipedia.org/wiki/MPEG-TS
%%%  http://en.wikipedia.org/wiki/Packetized_Elementary_Stream
%%%  http://en.wikipedia.org/wiki/Program_Specific_Information
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlang-mpegts.
%%% 
%%% erlang-mpegts is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-mpegts is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-mpegts.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------

-module(mpegts).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/video_frame.hrl").
-include("mpegts.hrl").

-export([init/0, init/1, flush/1, encode/2, pad_continuity_counters/1]).
-export([continuity_counters/1, video_config/1, audio_config/1]).

-export([read/2]).


-define(TS_PACKET, 184). % 188 - 4 bytes of header
-define(PAT_PID, 0).
-define(PMT_PID, 256).
-define(AUDIO_PID, 258).
-define(VIDEO_PID, 257).
-define(PCR_PID, ?AUDIO_PID).
-define(PAT_TABLEID, 0).
-define(PMT_TABLEID, 2).


  
read(URL, Options) ->
  {ok, Reader} = mpegts_sup:start_reader([{consumer,self()},{url, URL}|Options]),
  case (catch gen_server:call(Reader, connect)) of
    ok -> {ok, Reader};
    {'EXIT', Error} -> {error, Error}
  end.

-record(streamer, {
  pat_counter = 0,
  pmt_counter = 0,
  audio_counter = 0,
  video_counter = 0,
  sent_pat = false,
  last_dts,
  length_size = 32,
  audio_config = undefined,
  video_config = undefined,
  audio_buffer = [],
  audio_dts,
  interleave
}).

init() -> init([]).

init(Options) -> 
  Interleave = case proplists:get_value(interleave, Options) of
    Num when is_number(Num) andalso Num > 0 -> Num;
    _ -> false
  end,
  #streamer{interleave = Interleave}.

flush(Streamer) ->
  {Streamer1, Audio} = flush_audio(Streamer),
  {Streamer2, Padding} = pad_continuity_counters(Streamer1),
  {Streamer2, [Audio, Padding]}.

mux(Data, Streamer, Pid) ->
  Start = 1,
  mux_parts(Data, Streamer, Pid, Start, <<>>).
  
increment_counter(#streamer{pat_counter = C} = Streamer, ?PAT_PID) ->
  {C, Streamer#streamer{pat_counter = (C + 1) rem 16}};
increment_counter(#streamer{pmt_counter = C} = Streamer, ?PMT_PID) ->
  {C, Streamer#streamer{pmt_counter = (C + 1) rem 16}};
increment_counter(#streamer{audio_counter = C} = Streamer, ?AUDIO_PID) ->
  {C, Streamer#streamer{audio_counter = (C + 1) rem 16}};
increment_counter(#streamer{video_counter = C} = Streamer, ?VIDEO_PID) ->
  {C, Streamer#streamer{video_counter = (C + 1) rem 16}}.
  
% 4 bytes header, 1 byte syncword, 188 packet, so data is 183


adaptation_field(Data, _Pid) when size(Data) >= ?TS_PACKET -> 
  {0, <<>>};
  
adaptation_field(Data, _Pid) when is_binary(Data) ->
  Field = padding(<<0>>, ?TS_PACKET - size(Data) - 1),
  {1, <<(size(Field)), Field/binary>>};

adaptation_field({Timestamp, Data}, Pid) ->
  adaptation_field({Timestamp, 0, Data}, Pid);
  
adaptation_field({Timestamp, _Keyframe, Data}, Pid) ->
  % ?D({"PCR", PCR}),
  Discontinuity = 0,
  RandomAccess = 0,
  Priority = 0,
  {HasPCR, PCR} = case Pid of
    ?PCR_PID ->
      FullPCR = round(Timestamp * 27000),
      PCR1 = FullPCR div 300,
      PCR2 = FullPCR rem 300,
      PCRBin = <<PCR1:33, 2#111111:6, PCR2:9>>,
      % ?D({pcr,Timestamp, PCR1}),
      {1, PCRBin};
    _ ->
      % ?D({dts,Timestamp}),
      {0, <<>>}
  end,
  HasOPCR = 0,
  Splice = 0,
  Private = 0,
  Ext = 0,

  Adaptation = <<Discontinuity:1, RandomAccess:1, Priority:1, HasPCR:1, HasOPCR:1, Splice:1, Private:1, Ext:1, PCR/bitstring>>,
  Field = padding(Adaptation, ?TS_PACKET - 1 - size(Data)),
  % ?D({"Adaptation PCR", Timestamp}),
  % ?D({"Adapt", size(Adaptation), size(Field)+1, size(Data)}),
  {1, <<(size(Field)), Field/binary>>}.


mux_parts(Data, Streamer, Pid, Start, Accumulator) ->
  {Adaptation, Field} = adaptation_field(Data, Pid),
  {Counter, Streamer1} = increment_counter(Streamer, Pid),
  % ?D({Pid,Counter}),
  Scrambling = 0,
  Priority = 0,
  TEI = 0,

  Payload = case Data of
    {_, _, Bin} -> Bin;
    {_, Bin} -> Bin;
    _ -> Data
  end,
  HasPayload = case size(Payload) of
    0 -> 0;
    _ -> 1
  end,
  Header = <<16#47, TEI:1, Start:1, Priority:1, Pid:13, Scrambling:2, Adaptation:1, HasPayload:1, Counter:4, Field/binary>>,
  send_ts(Header, Payload, Streamer1, Pid, Accumulator).
  
send_ts(Header, Data, Streamer, _Pid, Accumulator) when size(Data) + size(Header) == 188 ->
  % ?D({"TS packet", _Pid, size(<<Header/binary, Data/binary>>)}),
  {Streamer, <<Accumulator/binary, Header/binary, Data/binary>>};

send_ts(Header, Data, Streamer, Pid, Accumulator) when size(Data) + size(Header) > 188  ->
  Length = 188 - size(Header),
  <<Packet:Length/binary, Rest/binary>> = Data,
  % ?D({"TS packet", Pid, size(<<Header/binary, Packet/binary>>), size(Rest)}),
  mux_parts(Rest, Streamer, Pid, 0, <<Accumulator/binary, Header/binary, Packet/binary>>).
  
  
padder() ->
  <<255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
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
  255,255,255,255,255,255,255,255>>.

padding(Padding, Size) when size(Padding) >= Size -> Padding;
padding(Padding, Size) when size(Padding) < Size -> 
  {Pad, _} = split_binary(padder(), Size - size(Padding)),
  padding(<<Padding/binary, Pad/binary>>, Size - size(Pad)).
  
send_pat(Streamer, _DTS) ->
  Programs = <<1:16, 111:3, ?PMT_PID:13>>,
  TSStream = 1, % Just the same, as VLC does
  Version = 0,
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
  mux(PATBin, Streamer, ?PAT_PID).

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
  Streams = iolist_to_binary([VideoStream, AudioStream]),
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

timestamps(DTS, PTS) ->
  timestamps(DTS, PTS, undefined).

timestamps(DTS, PTS, Force) when DTS == PTS andalso Force =/= different ->
  PTS1 = round(PTS*90),
  <<Pts1:3, Pts2:15, Pts3:15>> = <<PTS1:33>>,
  AddPesHeader = <<2#0010:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1>>,
  {2#10, AddPesHeader};
  
timestamps(DTS, PTS, _Force) ->
  DTS1 = round(DTS*90),
  PTS1 = round(PTS*90),
  % ?D({"Video", PTS, DTS, "--", DTS1, PTS1}),
  % ?D({video, round(DTS), round(PTS), FrameType}),
  <<Pts1:3, Pts2:15, Pts3:15>> = <<PTS1:33>>,
  <<Dts1:3, Dts2:15, Dts3:15>> = <<DTS1:33>>,
  AddPesHeader = <<2#0011:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1, 
                   2#0001:4, Dts1:3, 1:1, Dts2:15, 1:1, Dts3:15, 1:1>>,
  {2#11, AddPesHeader}.

  
send_video(Streamer, #video_frame{dts = DTS, pts = PTS, body = Body, flavor = Flavor} = _F) ->
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
  
  {PtsDts, AddPesHeader} = timestamps(DTS, PTS, different),
  PesHeader = <<Marker:2, Scrambling:2, Priority:1, Alignment:1, Copyright:1, Original:1, PtsDts:2, 
                ESCR:1,ESRate:1,DSMTrick:1,CopyInfo:1,CRC:1,Extension:1,  % All these bits are usually zero
                (size(AddPesHeader)):8, AddPesHeader/binary>>,
  % ?D({"Sending nal", Body}),
  % NALHeader = <<1:32>>,
  % PES = <<1:24, ?MPEGTS_STREAMID_H264, (size(PesHeader) + size(Body) + size(NALHeader) + 1):16, PesHeader/binary, NALHeader/binary, Body/binary, 0>>,
  % no PES size should be provided for video
  PES = <<1:24, ?MPEGTS_STREAMID_H264, 0:16, PesHeader/binary, 1:32, Body/binary>>,
  
  Keyframe = case Flavor of
    config -> 1;
    keyframe -> 1;
    _ -> 0
  end,
  mux({PTS, Keyframe, PES}, Streamer, ?VIDEO_PID).

send_audio(#streamer{audio_config = AudioConfig} = Streamer, #video_frame{codec = Codec, dts = DTS, body = Body}) ->
  Marker = 2#10,
  Scrambling = 0,
  Alignment = 1,
  {PtsDts, AddPesHeader} = timestamps(DTS, DTS),
  PesHeader = <<Marker:2, Scrambling:2, 0:1,
                Alignment:1, 0:1, 0:1, PtsDts:2, 0:6, (size(AddPesHeader)):8, AddPesHeader/binary>>,
  % ?D({"Audio", round(DTS), size(Body)}),
  ADTS = case Codec of
    aac -> aac:pack_adts(Body, AudioConfig);
    adts -> Body;
    mpeg2audio -> Body
  end,
  PES = <<1:24, ?MPEGTS_STREAMID_AAC, (size(PesHeader) + size(ADTS)):16, PesHeader/binary, ADTS/binary>>,
  % PES = <<1:24, ?TYPE_AUDIO_AAC, 0:16, PesHeader/binary, ADTS/binary>>,
  mux({DTS, PES}, Streamer, ?AUDIO_PID).



unpack_nals(Body, LengthSize) ->
  unpack_nals(Body, LengthSize, []).
  
unpack_nals(<<>>, _LengthSize, NALS) ->
  lists:reverse(NALS);

unpack_nals(Body, LengthSize, NALS) ->
  <<Length:LengthSize, NAL:Length/binary, Rest/binary>> = Body,
  unpack_nals(Rest, LengthSize, [NAL|NALS]).



encode(#streamer{sent_pat = false} = Streamer, #video_frame{dts = DTS} = Frame) ->
  {Streamer1, PATBin} = send_pat(Streamer, DTS),
  {Streamer2, PMTBin} = send_pmt(Streamer1, DTS),
  case encode(Streamer2#streamer{sent_pat = true, last_dts = DTS}, Frame) of
    {Streamer3, none} ->
      {Streamer3, <<PATBin/binary, PMTBin/binary>>};
    {Streamer3, Bin} ->
      {Streamer3, <<PATBin/binary, PMTBin/binary, Bin/binary>>}
  end;

encode(#streamer{} = Streamer, #video_frame{content = video, flavor = config, body = Config, dts = DTS}) ->
  {NewLengthSize, _} = h264:unpack_config(Config),
  {Streamer#streamer{video_config = Config, length_size = NewLengthSize*8, last_dts = DTS}, none};

encode(#streamer{length_size = LengthSize, video_config = VideoConfig} = Streamer, #video_frame{content = video, flavor = keyframe, body = Body, dts = DTS} = Frame) when VideoConfig =/= undefined ->
  {Streamer1, PATBin} = send_pat(Streamer, DTS),
  {Streamer2, PMTBin} = send_pmt(Streamer1, DTS),

  BodyNALS = unpack_nals(Body, LengthSize),
  {_, ConfigNALS} = h264:unpack_config(VideoConfig),
  F = fun(NAL, S) ->
    <<S/binary, 1:24, NAL/binary>>
  end,
  Packed = lists:foldl(F, <<9, 16#F0>>, ConfigNALS ++ BodyNALS),
  % Packed = lists:foldl(F, undefined, ConfigNALS ++ BodyNALS),

  {Streamer3, Video} = send_video(Streamer2, Frame#video_frame{body = Packed}),
  {Streamer3#streamer{last_dts = DTS}, <<PATBin/binary, PMTBin/binary, Video/binary>>};

encode(#streamer{length_size = LengthSize} = Streamer, #video_frame{content = video, codec = h264, body = Body, dts = DTS} = Frame) ->
  BodyNALS = unpack_nals(Body, LengthSize),
  F = fun(NAL, S) ->
    <<S/binary, 1:24, NAL/binary>>
  end,
  Packed = lists:foldl(F, <<9, 16#F0>>, BodyNALS),
  send_video(Streamer#streamer{last_dts = DTS}, Frame#video_frame{body = Packed});

encode(#streamer{} = Streamer, #video_frame{content = video, dts = DTS} = Frame) ->
  send_video(Streamer#streamer{last_dts = DTS}, Frame);
  
encode(#streamer{} = Streamer, #video_frame{content = audio, flavor = config, codec = aac, body = AudioConfig, dts = DTS}) ->
  Config = aac:decode_config(AudioConfig),
  {Streamer#streamer{audio_config = Config, last_dts = DTS}, none};

%%% From here goes interleave
encode(#streamer{audio_dts = undefined} = Streamer, #video_frame{content = audio, dts = DTS} = Frame) ->
  encode(Streamer#streamer{audio_dts = DTS}, Frame);

encode(#streamer{audio_config = undefined} = Streamer, #video_frame{content = audio, codec = aac, dts = DTS}) ->
  {Streamer#streamer{last_dts = DTS}, none};

encode(#streamer{interleave = false} = Streamer, #video_frame{content = audio, dts = DTS} = Frame) ->
  send_audio(Streamer#streamer{last_dts = DTS}, Frame);

encode(#streamer{interleave = Interleave, audio_buffer = Audio, audio_dts = OldDTS, audio_config = Config} = Streamer, 
       #video_frame{content = audio, codec = aac, body = Body, dts = DTS}) when DTS - OldDTS < Interleave ->
  ADTS = aac:pack_adts(Body, Config),
  {Streamer#streamer{audio_buffer = [ADTS|Audio]}, none};

encode(#streamer{} = Streamer, #video_frame{content = audio, codec = aac, dts = DTS} = Frame) ->
  {Streamer1, Reply} = flush_audio(Streamer),
  {Streamer2, none} = encode(Streamer1#streamer{audio_dts = DTS, audio_buffer = []}, Frame),
  {Streamer2, Reply};

encode(#streamer{} = Streamer, #video_frame{content = metadata}) ->
  {Streamer, none}.


flush_audio(#streamer{audio_buffer = [], audio_dts = undefined} = Streamer) ->
  {Streamer, <<>>};

flush_audio(#streamer{audio_buffer = Audio, audio_dts = DTS} = Streamer) ->
  % ?D({flush_adts, length(Audio), round(DTS)}),
  ADTS = #video_frame{
    content = audio,
    codec = adts,
    flavor = frame,
    dts = DTS,
    pts = DTS,
    body = iolist_to_binary(lists:reverse(Audio))
  },
  send_audio(Streamer, ADTS).

video_config(#streamer{video_config = V}) -> V.
audio_config(#streamer{audio_config = A}) -> A.

-define(END_COUNTER, 15).

continuity_counters(#streamer{video_counter = Video, audio_counter = Audio, pmt_counter = PMT, pat_counter = PAT}) ->
  {Video, Audio, PMT, PAT}.

pad_continuity_counters(Streamer) ->
  % ?D({padding, continuity_counters(Streamer)}),
  pad_continuity_counters(Streamer, <<>>).

pad_continuity_counters(#streamer{audio_counter = Counter} = Streamer, Accum) when Counter > 0 ->
  % ?D({pad, audio, Streamer#streamer.audio_counter}),
  {Streamer1, Bin} = mux_parts(<<0,0,1>>, Streamer, ?AUDIO_PID, 0, <<>>),
  pad_continuity_counters(Streamer1, <<Accum/binary, Bin/binary>>);

pad_continuity_counters(#streamer{video_counter = Counter} = Streamer, Accum) when Counter > 0 ->
  % ?D({pad, video, Streamer#streamer.video_counter}),
  {Streamer1, Bin} = mux_parts(<<0,0,1>>, Streamer, ?VIDEO_PID, 0, <<>>),
  pad_continuity_counters(Streamer1, <<Accum/binary, Bin/binary>>);

pad_continuity_counters(#streamer{pat_counter = Counter, last_dts = DTS} = Streamer, Accum) when Counter > 0 ->
  % ?D({pad, pat, Streamer#streamer.pat_counter}),
  {Streamer1, Bin} = send_pat(Streamer, DTS),
  pad_continuity_counters(Streamer1, <<Accum/binary, Bin/binary>>);

pad_continuity_counters(#streamer{pmt_counter = Counter, last_dts = DTS} = Streamer, Accum) when Counter > 0 ->
  % ?D({pad, pmt, Streamer#streamer.pmt_counter}),
  {Streamer1, Bin} = send_pmt(Streamer, DTS),
  pad_continuity_counters(Streamer1, <<Accum/binary, Bin/binary>>);

pad_continuity_counters(Streamer, Accum) ->
  {Streamer, Accum}.


  
  

  
  
  
  
  
  
  