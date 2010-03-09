-module(mpegts_reader).
-author('Max Lapshin <max@maxidoors.ru>').

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-include_lib("h264/include/h264.hrl").
-include("mpegts.hrl").
-include_lib("erlyvideo/include/video_frame.hrl").


-export([benchmark/0]).

-export([ts/1]).



-record(ts_lander, {
  buffer = <<>>,
  pids,
  consumer,
  byte_counter = 0
}).



-record(stream_out, {
  pid,
  handler
}).

-record(stream, {
  pid,
  program_num,
  handler,
  consumer,
  type,
  synced = false,
  ts_buffer = [],
  es_buffer = <<>>,
  counter = 0,
  pcr,
  start_dts,
  dts,
  pts,
  video_config = undefined,
  send_audio_config = false,
  h264
}).

-record(ts_header, {
  payload_start,
  pid,
  pcr = undefined,
  opcr = undefined,
  payload
}).

-export([handle_pat/4, pmt/4, pes/1]).

-export([pat/1]).


-export([start_link/1, init/1, synchronizer/1]).


start_link(Consumer) ->
  {ok, spawn_link(?MODULE, init, [[Consumer]])}.


init([Consumer]) ->
  erlang:monitor(process, Consumer),
  synchronizer(#ts_lander{consumer = Consumer, pids = [#stream{pid = 0, handler = handle_pat}]}).

synchronizer(#ts_lander{consumer = Consumer, buffer = Buffer} = TSLander) ->
  receive
    {'DOWN', _Ref, process, Consumer, _Reason} ->
      ?D({"MPEG TS reader lost consumer"}),
      ok;
    % {data, Bin} when size(Buffer) == 0 ->
    %   ?D({"Rece"})
    %   synchronizer(Bin, TSLander),
    %   ?MODULE:synchronizer(TSLander);
    {data, Bin} ->
      TSLander1 = synchronizer(<<Buffer/binary, Bin/binary>>, TSLander),
      ?MODULE:synchronizer(TSLander1);
    Else ->
      ?D({"MPEG TS reader", Else}),
      ok
  end.    
    
    

synchronizer(<<16#47, _:187/binary, 16#47, _/binary>> = Bin, TSLander) ->
  {Packet, Rest} = split_binary(Bin, 188),
  Lander = demux(TSLander, Packet),
  synchronizer(Rest, Lander);

synchronizer(<<_, Bin/binary>>, TSLander) when size(Bin) >= 374 ->
  synchronizer(Bin, TSLander);

synchronizer(Bin, TSLander) ->
  TSLander#ts_lander{buffer = Bin}.


ts(<<16#47, _TEI:1, PayloadStart:1, _:1, Pid:13, _Opt:4, _Counter:4, _/binary>> = Packet) ->
  Header = adaptation_field(Packet, #ts_header{payload_start = PayloadStart, pid = Pid}),
  Header#ts_header{pid = Pid, payload = ts_payload(Packet)}.


demux(#ts_lander{pids = Pids} = TSLander, <<16#47, _:1, PayloadStart:1, _:1, Pid:13, _:4, Counter:4, _/binary>> = Packet) ->
  Header = adaptation_field(Packet, #ts_header{payload_start = PayloadStart, pid = Pid}),
  case lists:keyfind(Pid, #stream.pid, Pids) of
    #stream{handler = Handler, counter = _OldCounter} = Stream ->
      % Counter = (OldCounter + 1) rem 15,
      % ?D({Handler, Packet}),
      ?MODULE:Handler(ts_payload(Packet), TSLander, Stream#stream{counter = Counter}, Header);
    #stream_out{handler = Handler} ->
      Handler ! {ts_packet, Header, ts_payload(Packet)},
      TSLander;
    false ->
      TSLander
  end.
  
      

ts_payload(<<16#47, _TEI:1, _Start:1, _Priority:1, _Pid:13, _Scrambling:2, 0:1, 1:1, _Counter:4, Payload/binary>>)  -> 
  Payload;

ts_payload(<<16#47, _TEI:1, _Start:1, _Priority:1, _Pid:13, _Scrambling:2, 1:1, 1:1, _Counter:4, 
              AdaptationLength, _AdaptationField:AdaptationLength/binary, Payload/binary>>) -> 
  Payload;

ts_payload(<<16#47, _TEI:1, _Start:1, _Priority:1, _Pid:13, _Scrambling:2, 
              _Adaptation:1, 0:1, _Counter:4, _Payload/binary>>)  ->
  ?D({"Empty payload on pid", _Pid}),
  <<>>.

adaptation_field(<<16#47, _:18, 0:1, _:5, _/binary>>, Header) -> Header;
adaptation_field(<<16#47, _:18, 1:1, _:5, AdaptationLength, AdaptationField:AdaptationLength/binary, _/binary>>, Header) when AdaptationLength > 0 -> 
  parse_adaptation_field(AdaptationField, Header);
  
adaptation_field(_, Header) -> Header.


parse_adaptation_field(<<_Discontinuity:1, _RandomAccess:1, _Priority:1, PCR:1, OPCR:1, _Splice:1, _Private:1, _Ext:1, Data/binary>>, Header) ->
  parse_adaptation_field(Data, PCR, OPCR, Header).

parse_adaptation_field(<<Pcr1:33, Pcr2:9, Rest/bitstring>>, 1, OPCR, Header) ->
  % ?D({Header#ts_header.pid, round(Pcr1/90 + Pcr2 / 27000)}),
  parse_adaptation_field(Rest, 0, OPCR, Header#ts_header{pcr = Pcr1 / 90 + Pcr2 / 27000});

parse_adaptation_field(<<OPcr1:33, OPcr2:9, _Rest/bitstring>>, 0, 1, Header) ->
  Header#ts_header{opcr = OPcr1 / 90 + OPcr2 / 27000};
  
parse_adaptation_field(_, 0, 0, Field) -> Field.



%%%%%%%%%%%%%%%   Program access table  %%%%%%%%%%%%%%

handle_pat(PATBin, #ts_lander{pids = Pids} = TSLander, _, _) ->
  % ?D({"Full PAT", size(PATBin), PATBin}),
  PAT = pat(PATBin),
  #mpegts_pat{descriptors = Descriptors} = PAT,
  TSLander#ts_lander{pids = lists:ukeymerge(#stream.pid, Pids, Descriptors)}.
  

pat(<<_PtField, 0, 2#10:2, 2#11:2, Length:12, _Misc:5/binary, PAT/binary>> = _PATBin) -> % PAT
  ProgramCount = round((Length - 5)/4) - 1,
  % io:format("PAT: ~p programs (~p)~n", [ProgramCount, size(PAT)]),
  % ?D({"PAT descriptors", ProgramCount, PAT}),
  Descriptors = extract_pat(PAT, ProgramCount, []),
  #mpegts_pat{descriptors = Descriptors}.



extract_pat(<<_CRC32/binary>>, 0, Descriptors) ->
  lists:keysort(#stream.pid, Descriptors);
  
extract_pat(<<ProgramNum:16, _:3, Pid:13, PAT/binary>>, ProgramCount, Descriptors) ->
  extract_pat(PAT, ProgramCount - 1, [#stream{handler = pmt, pid = Pid, counter = 0, program_num = ProgramNum} | Descriptors]).



pmt(<<_Pointer, 2, _SectionInd:1, 0:1, 2#11:2, SectionLength:12, 
    ProgramNum:16, _:2, _Version:5, _CurrentNext:1, _SectionNumber,
    _LastSectionNumber, _Some:3, _PCRPID:13, _Some2:4, ProgramInfoLength:12, 
    _ProgramInfo:ProgramInfoLength/binary, PMT/binary>> = _PMTBin, #ts_lander{pids = Pids, consumer = Consumer} = TSLander, _, _) ->
  % ?D({"PMT", size(PMTBin), PMTBin, SectionLength - 13, size(PMT), PMT}),
  PMTLength = round(SectionLength - 13 - ProgramInfoLength),
  io:format("Program ~p v~p. PCR: ~p~n", [ProgramNum, _Version, _PCRPID]),
  % io:format("Program info: ~p~n", [ProgramInfo]),
  ?D({"PMT", size(PMT), PMTLength, _ProgramInfo}),
  Descriptors = extract_pmt(PMT, PMTLength, []),
  % io:format("Streams: ~p~n", [Descriptors]),
  Descriptors1 = lists:map(fun(#stream{pid = Pid} = Stream) ->
    case lists:keyfind(Pid, #stream.pid, Pids) of
      false ->
        Handler = spawn_link(?MODULE, pes, [Stream#stream{program_num = ProgramNum, consumer = Consumer, h264 = #h264{}}]),
        ?D({"Starting PID", Pid, Handler}),
        erlang:monitor(process, Handler),
        #stream_out{pid = Pid, handler = Handler};
      Other ->
        Other
    end
  end, Descriptors),
  % AllPids = [self() | lists:map(fun(A) -> element(#stream_out.handler, A) end, Descriptors1)],
  % eprof:start(),
  % eprof:start_profiling(AllPids),
  % TSLander#ts_lander{pids = lists:keymerge(#stream.pid, Pids, Descriptors1)}.
  TSLander#ts_lander{pids = Descriptors1}.

extract_pmt(_CRC32, 0, Descriptors) ->
  % ?D({"Left CRC32", _CRC32}),
  % io:format("Unknown PMT: ~p~n", [PMT]),
  lists:keysort(#stream.pid, Descriptors);

extract_pmt(<<StreamType, 2#111:3, Pid:13, _:4, ESLength:12, _ES:ESLength/binary, Rest/binary>>, PMTLength, Descriptors) ->
  ?D({"Pid -> Type", Pid, StreamType, _ES, PMTLength}),
  extract_pmt(Rest, PMTLength - 5 - ESLength, [#stream{handler = pes, counter = 0, pid = Pid, type = stream_type(StreamType)}|Descriptors]).
  


stream_type(?TYPE_VIDEO_H264) -> video;
stream_type(?TYPE_AUDIO_AAC) -> audio;
stream_type(?TYPE_AUDIO_AAC2) -> audio;
stream_type(Type) -> ?D({"Unknown TS PID type", Type}), unhandled.

pes(#stream{synced = false, pid = Pid} = Stream) ->
  receive
    {ts_packet, #ts_header{payload_start = 0}, _} ->
      ?D({"Not synced pes", Pid}),
      ?MODULE:pes(Stream);
    {ts_packet, #ts_header{payload_start = 1}, Packet} ->
      ?D({"Synced PES", Pid}),
      Stream1 = Stream#stream{synced = true, ts_buffer = [Packet]},
      ?MODULE:pes(Stream1);
    {ts_packet, #ts_header{}, _} ->
      % ?D({"Not synced pes", Pid}),
      ?MODULE:pes(Stream);
    Other ->
      ?D({"Undefined message to pid", Pid, Other})
  end;
  
pes(#stream{synced = true, pid = Pid, ts_buffer = Buf} = Stream) ->
  receive
    {ts_packet, #ts_header{payload_start = 0} = Header, Packet} ->
      Stream1 = copy_pcr(Header, Stream#stream{synced = true, ts_buffer = [Packet | Buf]}),
      ?MODULE:pes(Stream1);
    {ts_packet, #ts_header{payload_start = 1} = Header, Packet} ->
      PES = iolist_to_binary(lists:reverse(Buf)),
      Stream1 = stream_timestamp(Packet, copy_pcr(Header, Stream)),
      % ?D({Stream1#stream.type, Stream1#stream.pcr, Stream1#stream.dts}),
      Stream2 = pes_packet(PES, Stream1),
      ?MODULE:pes(Stream2#stream{ts_buffer = [Packet]});
    Other ->
      ?D({"Undefined message to pid", Pid, Other})
  end.

copy_pcr(#ts_header{pcr = undefined}, Stream) -> Stream;
copy_pcr(#ts_header{pcr = PCR}, Stream) -> Stream#stream{pcr = PCR}.
      
pes_packet(_, #stream{type = unhandled} = Stream) -> Stream#stream{ts_buffer = []};

pes_packet(_, #stream{dts = undefiend} = Stream) ->
  ?D({"No PCR or DTS yes"}),
  Stream#stream{ts_buffer = []};

pes_packet(<<1:24, _:5/binary, Length, _PESHeader:Length/binary, Data/binary>>, #stream{type = audio, es_buffer = Buffer} = Stream) ->
  % ?D({"Audio", Stream1#stream.pcr, Stream1#stream.dts}),
  % Stream1;
  decode_aac(Stream#stream{es_buffer = <<Buffer/binary, Data/binary>>});
  
pes_packet(<<1:24, _:5/binary, Length, _PESHeader:Length/binary, Rest/binary>>, #stream{es_buffer = Buffer, type = video} = Stream) ->
  % ?D({"Timestamp1", Stream#stream.timestamp, Stream#stream.start_time}),
  % ?D({"Video", Stream1#stream.pcr, Stream1#stream.dts}),
  % ?D({"Video", Stream1#stream.timestamp, _PESHeader}),
  decode_avc(Stream#stream{es_buffer = <<Buffer/binary, Rest/binary>>}).


stream_timestamp(<<_:7/binary, 2#11:2, _:6, PESHeaderLength, PESHeader:PESHeaderLength/binary, _/binary>>, Stream) ->
  <<2#0011:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1, 
    2#0001:4, Dts1:3, 1:1, Dts2:15, 1:1, Dts3:15, 1:1, _Rest/binary>> = PESHeader,
  <<PTS1:33>> = <<Pts1:3, Pts2:15, Pts3:15>>,
  <<DTS1:33>> = <<Dts1:3, Dts2:15, Dts3:15>>,
  PTS = PTS1/90,
  DTS = DTS1/90,
  % ?D({"Have DTS & PTS", round(DTS), round(PTS)}),
  normalize_timestamp(Stream#stream{dts = DTS, pts = PTS});
  

stream_timestamp(<<_:7/binary, 2#10:2, _:6, PESHeaderLength, PESHeader:PESHeaderLength/binary, _/binary>>, Stream) ->
  <<2#0010:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1, _Rest/binary>> = PESHeader,
  <<PTS1:33>> = <<Pts1:3, Pts2:15, Pts3:15>>,
  PTS = PTS1/90,
  % ?D({"Have pts", Stream#stream.pid, round(PTS)}),
  normalize_timestamp(Stream#stream{dts = PTS, pts = PTS});

% FIXME!!!
% Here is a HUGE hack. VLC give me stream, where are no DTS or PTS, only OPCR, once a second,
% thus I increment timestamp counter on each NAL, assuming, that there is 25 FPS.
% This is very, very wrong, but I really don't know how to calculate it in other way.
% stream_timestamp(_, #stream{pcr = PCR} = Stream, _) when is_number(PCR) ->
%   % ?D({"Set DTS to PCR", PCR}),
%   normalize_timestamp(Stream#stream{dts = PCR, pts = PCR});
stream_timestamp(_, #stream{dts = DTS, pts = _PTS, pcr = PCR, start_dts = Start} = Stream) when is_number(PCR) andalso is_number(DTS) andalso is_number(Start) andalso PCR == DTS + Start ->
  ?D({"Increasing", DTS}),
  % Stream#stream{dts = DTS + 40, pts = PTS + 40};
  Stream;

stream_timestamp(_, #stream{dts = DTS, pts = PTS, pcr = undefined} = Stream) when is_number(DTS) andalso is_number(PTS) ->
  ?D({"Have no timestamps", DTS}),
  Stream#stream{dts = DTS + 40, pts = PTS + 40};

stream_timestamp(_,  #stream{pcr = PCR} = Stream) when is_number(PCR) ->
  ?D({"No DTS, taking", PCR - (Stream#stream.dts + Stream#stream.start_dts), PCR - (Stream#stream.pts + Stream#stream.start_dts)}),
  normalize_timestamp(Stream#stream{pcr = PCR, dts = PCR, pts = PCR});
  
stream_timestamp(_, #stream{pcr = undefined, dts = undefined} = Stream) ->
  Stream.


% normalize_timestamp(Stream) -> Stream;
% normalize_timestamp(#stream{start_dts = undefined, dts = DTS} = Stream) when is_number(DTS) -> 
%   normalize_timestamp(Stream#stream{start_dts = DTS});
% normalize_timestamp(#stream{start_dts = undefined, pts = PTS} = Stream) when is_number(PTS) -> 
%   normalize_timestamp(Stream#stream{start_dts = PTS});

normalize_timestamp(#stream{start_dts = undefined, pcr = PCR} = Stream) when is_number(PCR) -> 
  normalize_timestamp(Stream#stream{start_dts = PCR});

normalize_timestamp(#stream{start_dts = undefined, dts = DTS} = Stream) when is_number(DTS) andalso DTS > 0 -> 
  normalize_timestamp(Stream#stream{start_dts = DTS});

normalize_timestamp(#stream{start_dts = Start, dts = DTS, pts = PTS} = Stream) when is_number(Start) andalso Start > 0 -> 
  % ?D({"Normalize", Stream#stream.pid, round(DTS - Start), round(PTS - Start)}),
  Stream#stream{dts = DTS - Start, pts = PTS - Start};
normalize_timestamp(Stream) ->
  Stream.
  
% normalize_timestamp(#stream{start_pcr = 0, pcr = PCR} = Stream) when is_integer(PCR) andalso PCR > 0 -> 
%   Stream#stream{start_pcr = PCR, pcr = 0};
% normalize_timestamp(#stream{start_pcr = Start, pcr = PCR} = Stream) -> 
%   Stream#stream{pcr = PCR - Start}.
% normalize_timestamp(Stream) -> Stream.



% <<18,16,6>>
decode_aac(#stream{send_audio_config = false, dts = DTS, pts = PTS, consumer = Consumer} = Stream) ->
  % Config = <<16#A:4, 3:2, 1:1, 1:1, 0>>,
  Config = <<18,16>>,
  AudioConfig = #video_frame{       
   	type          = audio,
   	decoder_config = true,
		dts           = DTS,
		pts           = PTS,
		body          = Config,
	  codec_id	    = aac,
	  sound_type	  = stereo,
	  sound_size	  = bit16,
	  sound_rate	  = rate44
	},
	Consumer ! AudioConfig,
  % ?D({"Send audio config", AudioConfig}),
	decode_aac(Stream#stream{send_audio_config = true});
  

decode_aac(#stream{es_buffer = <<_Syncword:12, _ID:1, _Layer:2, 0:1, _Profile:2, _Sampling:4,
                                 _Private:1, _Channel:3, _Original:1, _Home:1, _Copyright:1, _CopyrightStart:1,
                                 _FrameLength:13, _ADTS:11, _Count:2, _CRC:16, Rest/binary>>} = Stream) ->
  send_aac(Stream#stream{es_buffer = Rest});

decode_aac(#stream{es_buffer = <<_Syncword:12, _ID:1, _Layer:2, _ProtectionAbsent:1, _Profile:2, _Sampling:4,
                                 _Private:1, _Channel:3, _Original:1, _Home:1, _Copyright:1, _CopyrightStart:1,
                                 _FrameLength:13, _ADTS:11, _Count:2, Rest/binary>>} = Stream) ->
  % ?D({"AAC", Syncword, ID, Layer, ProtectionAbsent, Profile, Sampling, Private, Channel, Original, Home,
  % Copyright, CopyrightStart, FrameLength, ADTS, Count}),
  % ?D({"AAC", Rest}),
  send_aac(Stream#stream{es_buffer = Rest}).

send_aac(#stream{es_buffer = Data, consumer = Consumer, dts = DTS, pts = PTS} = Stream) ->
  % ?D({audio, }),
  AudioFrame = #video_frame{       
    type          = audio,
    dts           = DTS,
    pts           = PTS,
    body          = Data,
	  codec_id	    = aac,
	  sound_type	  = stereo,
	  sound_size	  = bit16,
	  sound_rate	  = rate44
  },
  % ?D({audio, Stream#stream.pcr, DTS}),
  Consumer ! AudioFrame,
  Stream#stream{es_buffer = <<>>}.
  

decode_avc(#stream{es_buffer = <<16#000001:24, _/binary>>} = Stream) ->
  find_nal_end(Stream, 3);
  
decode_avc(#stream{es_buffer = Data} = Stream) ->
  % io:format("PES ~p ~p ~p ~p, ~p, ~p~n", [StreamId, _DataAlignmentIndicator, _PesPacketLength, PESHeaderLength, PESHeader, Rest]),
  % io:format("PES ~p ~p ~p ~p, ~p, ~p~n", [StreamId, _DataAlignmentIndicator, _PesPacketLength, PESHeaderLength, PESHeader, Rest]),
  Offset1 = nal_unit_start_code_finder(Data, 0) + 3,
  find_nal_end(Stream, Offset1).
  
find_nal_end(Stream, false) ->  
  Stream;
  
find_nal_end(#stream{es_buffer = Data} = Stream, Offset1) ->
  Offset2 = nal_unit_start_code_finder(Data, Offset1+3),
  extract_nal(Stream, Offset1, Offset2).

extract_nal(Stream, _, false) ->
  Stream;
  
extract_nal(#stream{es_buffer = Data, consumer = Consumer, dts = DTS, pts = PTS, h264 = H264} = Stream, Offset1, Offset2) ->
  Length = Offset2-Offset1,
  <<_:Offset1/binary, NAL:Length/binary, Rest1/binary>> = Data,
  % ?D({"Found NAL", Offset1, Offset2, NAL}),
  {H264_1, Frames} = h264:decode_nal(NAL, H264),
  % ?D({video, Stream#stream.pcr, DTS}),
  lists:foreach(fun(Frame) ->
    Consumer ! Frame#video_frame{dts = DTS, pts = PTS}
    % Consumer ! Frame#video_frame{dts = TS, pts = TS}
  end, Frames),
  decode_avc(Stream#stream{es_buffer = Rest1, h264 = H264_1}).

nal_unit_start_code_finder(Bin, Offset) ->
  case Bin of
    <<_:Offset/binary, Rest/binary>> -> find_nal_start_code(Rest, Offset);
    _ -> false
  end.

find_nal_start_code(Bin, Offset) -> find_nal_start_code_erl(Bin, Offset).

find_nal_start_code_erl(<<16#000001:24, _/binary>>, Offset) -> Offset;
find_nal_start_code_erl(<<_:1/binary, 16#000001:24, _/binary>>, Offset) -> Offset + 1;
find_nal_start_code_erl(<<_:2/binary, 16#000001:24, _/binary>>, Offset) -> Offset + 2;
find_nal_start_code_erl(<<_:3/binary, 16#000001:24, _/binary>>, Offset) -> Offset + 3;
find_nal_start_code_erl(<<_:4/binary, 16#000001:24, _/binary>>, Offset) -> Offset + 4;
find_nal_start_code_erl(<<_:5/binary, 16#000001:24, _/binary>>, Offset) -> Offset + 5;
find_nal_start_code_erl(<<_:6/binary, 16#000001:24, _/binary>>, Offset) -> Offset + 6;
find_nal_start_code_erl(<<_:7/binary, 16#000001:24, _/binary>>, Offset) -> Offset + 7;
find_nal_start_code_erl(<<_:8/binary, 16#000001:24, _/binary>>, Offset) -> Offset + 8;
find_nal_start_code_erl(<<_:9/binary, 16#000001:24, _/binary>>, Offset) -> Offset + 9;
find_nal_start_code_erl(<<_:10/binary, 16#000001:24, _/binary>>, Offset) -> Offset + 10;
find_nal_start_code_erl(<<_:11/binary, 16#000001:24, _/binary>>, Offset) -> Offset + 11;
find_nal_start_code_erl(<<_:12/binary, 16#000001:24, _/binary>>, Offset) -> Offset + 12;
find_nal_start_code_erl(<<_:13/binary, 16#000001:24, _/binary>>, Offset) -> Offset + 13;
find_nal_start_code_erl(<<_:14/binary, 16#000001:24, _/binary>>, Offset) -> Offset + 14;
find_nal_start_code_erl(<<_:15/binary, 16#000001:24, _/binary>>, Offset) -> Offset + 15;
find_nal_start_code_erl(<<_:16/binary, Rest/binary>>, Offset) -> find_nal_start_code_erl(Rest, Offset+16);
% find_nal_start_code(<<_, Rest/binary>>, Offset) -> find_nal_start_code(Rest, Offset+1);
find_nal_start_code_erl(_, _) -> false.


-include_lib("eunit/include/eunit.hrl").

benchmark() ->
  find_nal_start_code_bm().

find_nal_code_bin() ->
  Bin = <<0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,  %50
          0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,  %100
          0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,  %150
          0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,  %200
          0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,  %250
          0,0,1,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9, %303
          0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,  %353
          0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,  %403
          0,0,1>>,
  Bin.

find_nal_start_code_test() ->
  Bin = find_nal_code_bin(),
  ?assertEqual(250, find_nal_start_code(Bin, 0)),
  {_, Bin1} = split_binary(Bin, 253),
  ?assertEqual(403, find_nal_start_code(Bin1, 253)).

find_nal_start_code_bm() ->
  Bin = find_nal_code_bin(),
  erlang:statistics(wall_clock),
  N = 1000000,
  lists:foreach(fun(_) ->
    find_nal_start_code(Bin, 0)
  end, lists:seq(1,N)),
  {_, Timer} = erlang:statistics(wall_clock),
  ?D({"Timer", N, Timer}).





