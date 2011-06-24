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
-include("log.hrl").
-include("../include/mpegts.hrl").

-export([init/0, init/1, flush/1, encode/2, pad_continuity_counters/1, counters/1]).
-export([continuity_counters/1, video_config/1, audio_config/1]).

-export([read/2]).


-define(TS_PACKET, 184). % 188 - 4 bytes of header
-define(PMT_PID, 4095).
-define(AUDIO_PID, 257).
-define(VIDEO_PID, 256).
-define(PCR_PID, ?VIDEO_PID).


  
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
  pad_counters = true,
  sent_pat = false,
  last_dts,
  length_size = 32,
  audio_codec,
  video_codec,
  audio_config = undefined,
  video_config = undefined,
  video_buffer = [],
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
  Streamer = #streamer{interleave = Interleave, pad_counters = proplists:get_value(pad_counters, Options, true)},
  init_counters(Streamer, proplists:get_value(counters, Options)).

init_counters(Streamer, undefined) ->
  Streamer;
init_counters(Streamer, [PAT, PMT, Audio, Video]) ->
  Streamer#streamer{pat_counter = PAT, pmt_counter = PMT, audio_counter = Audio, video_counter = Video}.

counters(#streamer{pat_counter = PAT, pmt_counter = PMT, audio_counter = Audio, video_counter = Video}) ->
  [PAT, PMT, Audio, Video].

encode(Streamer, #video_frame{content = Content} = Frame) when Content == audio orelse Content == video ->
  % ?D({Frame#video_frame.codec, Frame#video_frame.flavor, round(Frame#video_frame.dts)}),
  Streamer0 = set_stream_codec(Streamer, Frame),
  {Streamer1, Bin1} = send_program_tables(Streamer0, Frame),
  Streamer2 = enqueue_frame(Streamer1, Frame),
  case need_to_flush(Streamer2) of
    true ->
      {Streamer3, Audio} = flush_audio(Streamer2),
      {Streamer4, Video} = flush_video(Streamer3),
      case Bin1 of
        <<>> -> {Streamer4, interleave(Audio,Video)};
        _ -> {Streamer4, [Bin1, interleave(Audio,Video)]}
      end;
    false ->
      {Streamer2, Bin1}
  end;

encode(Streamer, #video_frame{} = _Frame) ->
  {Streamer, []}.


flush(#streamer{} = Streamer) ->
  {Streamer1, Audio} = flush_audio(Streamer),
  {Streamer2, Video} = flush_video(Streamer1),
  {Streamer3, Padding} = pad_continuity_counters(Streamer2),
  {Streamer3, [Audio, Video, Padding]}.

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
  
adaptation_field({Timestamp, Keyframe, Data}, Pid) ->
  % ?D({"PCR", PCR}),
  Discontinuity = 0,
  RandomAccess = Keyframe,
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
  PAT = mpegts_psi:encode(pat, [{programs, [{1, ?PMT_PID}]}]),
  PATBin = padding(PAT, ?TS_PACKET),
  % ?D({"Sending PAT", size(PAT), size(PATBin)}),
  mux(PATBin, Streamer, ?PAT_PID).

send_pmt(#streamer{video_config = _VideoConfig, audio_codec = AudioCodec, video_codec = VideoCodec} = Streamer, _DTS) ->
  PMT = mpegts_psi:encode(pmt, [{program,1},{pcr_pid,?PCR_PID},{streams, [{AudioCodec, ?AUDIO_PID, []}, {VideoCodec, ?VIDEO_PID, []}]}]),
  mux(padding(PMT, ?TS_PACKET), Streamer, ?PMT_PID).

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

  
send_video(Streamer, #video_frame{dts = DTS, pts = PTS, body = Body, flavor = Flavor, codec = Codec} = _F) ->
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
  Code = case Codec of
    h264 -> ?MPEGTS_STREAMID_H264;
    mpeg2video -> ?TYPE_VIDEO_MPEG2
  end,
  PES = <<1:24, Code, 0:16, PesHeader/binary, 1:32, Body/binary>>,
  
  Keyframe = case Flavor of
    config -> 1;
    keyframe -> 1;
    _ -> 0
  end,
  % ?D({mux,h264,round(DTS), PTS}),
  mux({DTS, Keyframe, PES}, Streamer, ?VIDEO_PID).

send_audio(#streamer{} = Streamer, #video_frame{codec = empty}) ->
  {Streamer, <<>>};

send_audio(#streamer{audio_config = AudioConfig} = Streamer, #video_frame{codec = Codec, dts = DTS, body = Body}) ->
  Marker = 2#10,
  Scrambling = 0,
  Alignment = 1,
  {PtsDts, AddPesHeader} = timestamps(DTS, DTS),
  PesHeader = <<Marker:2, Scrambling:2, 0:1,
                Alignment:1, 0:1, 0:1, PtsDts:2, 0:6, (size(AddPesHeader)):8, AddPesHeader/binary>>,
  % ?D({"Audio", round(DTS), size(Body)}),
  Packed = case Codec of
    aac -> aac:pack_adts(Body, AudioConfig);
    adts -> Body;
    mpeg2audio -> Body;
    mp3 -> Body
  end,
  Code = case Codec of
    aac -> ?MPEGTS_STREAMID_AAC;
    adts -> ?MPEGTS_STREAMID_AAC;
    mp3 -> ?TYPE_AUDIO_MPEG1;
    mpeg2audio -> ?TYPE_AUDIO_MPEG2
  end,
  PES = <<1:24, Code, (size(PesHeader) + size(Packed)):16, PesHeader/binary, Packed/binary>>,
  % PES = <<1:24, ?TYPE_AUDIO_AAC, 0:16, PesHeader/binary, ADTS/binary>>,
  % ?D({mux,Codec,round(DTS)}),
  mux({DTS, PES}, Streamer, ?AUDIO_PID).



unpack_nals(Body, LengthSize) ->
  unpack_nals(Body, LengthSize, []).
  
unpack_nals(<<>>, _LengthSize, NALS) ->
  lists:reverse(NALS);

unpack_nals(Body, LengthSize, NALS) ->
  case Body of
    <<Length1:LengthSize, NAL:Length1/binary, Rest/binary>> ->
      unpack_nals(Rest, LengthSize, [NAL|NALS]);
    _ ->
      ?D({"Warning!!!! Broken file, cannot unpack NAL units from H264", Body, NALS}),
      []
  end.





send_program_tables(#streamer{sent_pat = SentPat, audio_codec = A, video_codec = V} = Streamer, #video_frame{content = Content, flavor = Flavor, dts = DTS} = _Frame) 
  when (SentPat == false orelse (Content == video andalso Flavor == keyframe)) andalso A =/= undefined andalso V =/= undefined ->
  {Streamer1, PATBin} = send_pat(Streamer, DTS),
  {Streamer2, PMTBin} = send_pmt(Streamer1, DTS),
  {Streamer2#streamer{sent_pat = true, last_dts = DTS}, <<PATBin/binary, PMTBin/binary>>};

send_program_tables(Streamer, _Frame) ->
  {Streamer, <<>>}.


set_stream_codec(#streamer{audio_codec = undefined} = Streamer, #video_frame{content = audio, codec = Codec}) ->
  Streamer#streamer{audio_codec = Codec};

set_stream_codec(#streamer{video_codec = undefined} = Streamer, #video_frame{content = video, codec = Codec}) ->
  Streamer#streamer{video_codec = Codec};

set_stream_codec(Streamer, _) ->
  Streamer.


enqueue_frame(#streamer{} = Streamer, #video_frame{content = video, flavor = config, codec = h264, body = Config, dts = DTS}) ->
  {NewLengthSize, _} = h264:unpack_config(Config),
  % ?D({new_length_size,NewLengthSize}),
  Streamer#streamer{video_config = Config, length_size = NewLengthSize*8, last_dts = DTS};

enqueue_frame(#streamer{} = Streamer, #video_frame{content = audio, flavor = config, codec = aac, body = AudioConfig, dts = DTS}) ->
  Config = aac:decode_config(AudioConfig),
  Streamer#streamer{audio_config = Config, last_dts = DTS};

enqueue_frame(#streamer{audio_config = undefined} = Streamer, #video_frame{content = audio, codec = aac}) ->
  Streamer;

enqueue_frame(#streamer{audio_dts = undefined} = Streamer, #video_frame{content = audio, dts = DTS} = Frame) ->
  enqueue_frame(Streamer#streamer{audio_dts = DTS}, Frame);

enqueue_frame(#streamer{audio_buffer = Audio, audio_config = Config} = Streamer, #video_frame{content = audio, codec = aac, body = Body}) ->
  ADTS = aac:pack_adts(Body, Config),
  Streamer#streamer{audio_buffer = [ADTS|Audio]};

enqueue_frame(#streamer{length_size = LengthSize, video_config = VideoConfig, video_buffer = Buffer} = Streamer, 
              #video_frame{content = video, codec = h264, flavor = Flavor, body = Body, dts = DTS} = Frame) ->
  BodyNALS = unpack_nals(Body, LengthSize),
  ConfigNALS = case Flavor of
    keyframe ->
      {_, CfgNALS} = h264:unpack_config(VideoConfig),
      CfgNALS;
    _ ->
      []
  end,
  F = fun(NAL, S) ->
    <<S/binary, 1:32, NAL/binary>>
  end,
  Packed = lists:foldl(F, <<9, 16#F0>>, ConfigNALS ++ BodyNALS), % Need to add AU (9 NAL) for HLS
  {Streamer2, Video} = send_video(Streamer#streamer{last_dts = DTS}, Frame#video_frame{body = Packed}),
  Streamer2#streamer{video_buffer = [Video|Buffer]};

enqueue_frame(#streamer{audio_buffer = Buffer} = Streamer, #video_frame{content = audio, dts = DTS} = Frame) ->
  {Streamer2, Audio} = send_audio(Streamer#streamer{last_dts = DTS}, Frame),
  Streamer2#streamer{audio_buffer = [Audio|Buffer]};

enqueue_frame(#streamer{video_buffer = Buffer} = Streamer, #video_frame{content = video, dts = DTS} = Frame) ->
  {Streamer2, Video} = send_video(Streamer#streamer{last_dts = DTS}, Frame),
  Streamer2#streamer{video_buffer = [Video|Buffer]}.



need_to_flush(#streamer{interleave = false}) -> true;
need_to_flush(#streamer{interleave = Interleave, audio_buffer = Buffer}) when Interleave =< length(Buffer) -> true;
need_to_flush(#streamer{interleave = Interleave, video_buffer = Buffer}) when Interleave =< length(Buffer) -> true;
need_to_flush(#streamer{}) -> false.


flush_audio(#streamer{audio_buffer = [], audio_dts = undefined} = Streamer) ->
  {Streamer, <<>>};

flush_audio(#streamer{audio_buffer = Audio, audio_dts = DTS, audio_codec = aac} = Streamer) ->
  % ?D({flush_adts, length(Audio), round(DTS)}),
  ADTS = #video_frame{
    content = audio,
    codec = adts,
    flavor = frame,
    dts = DTS,
    pts = DTS,
    body = iolist_to_binary(lists:reverse(Audio))
  },
  send_audio(Streamer#streamer{audio_buffer = [], audio_dts = undefined}, ADTS);

flush_audio(#streamer{audio_buffer = Audio} = Streamer) ->
  {Streamer#streamer{audio_buffer = []}, lists:reverse(Audio)}.

flush_video(#streamer{video_buffer = Buffer} = Streamer) ->
  {Streamer#streamer{video_buffer = []}, lists:reverse(Buffer)}.


interleave(Audio, Video) ->
  [Video, Audio].
%   A = iolist_to_binary(Audio),
%   V = iolist_to_binary(Video),
%   interleave(A, V, size(A), size(V), <<>>).
% 
% interleave(<<>>, V, _, _, Accum) -> <<Accum/binary, V/binary>>;
% interleave(A, <<>>, _, _, Accum) -> <<Accum/binary, A/binary>>;
% interleave(<<Packet:188/binary, Rest/binary>> = A, V, C1, C2, Accum) when size(A)*C2 > size(V)*C1 ->
%   interleave(Rest, V, C1, C2, <<Accum/binary, Packet/binary>>);
% interleave(A, <<Packet:188/binary, Rest/binary>>, C1, C2, Accum) ->
%   interleave(A, Rest, C1, C2, <<Accum/binary, Packet/binary>>).


video_config(#streamer{video_config = V}) -> V.
audio_config(#streamer{audio_config = A}) -> A.

-define(END_COUNTER, 15).

continuity_counters(#streamer{video_counter = Video, audio_counter = Audio, pmt_counter = PMT, pat_counter = PAT}) ->
  {Video, Audio, PMT, PAT}.


pad_continuity_counters(#streamer{pad_counters = false} = Streamer) ->
  {Streamer, <<>>};

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


  
  

  
  
  
  
  
  
  