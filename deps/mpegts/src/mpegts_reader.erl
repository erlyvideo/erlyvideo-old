%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        MPEG TS demuxer module
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
-module(mpegts_reader).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("erlmedia/include/h264.hrl").
-include("mpegts.hrl").
-include_lib("erlmedia/include/video_frame.hrl").


-export([benchmark/0]).

-export([ts/1]).

% -on_load(load_nif/0).



-record(ts_lander, {
  buffer = <<>>,
  pids,
  consumer,
  socket,
  options,
  byte_counter = 0
}).



-record(stream_out, {
  pid,
  handler
}).

-record(stream, {
  pid,
  program_num,
  demuxer,
  handler,
  consumer,
  codec,
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
-export([extract_nal/1]).

-export([start_link/1, set_socket/2]).
-export([init/1, synchronizer/1]).


% load_nif() ->
%   Load = erlang:load_nif(code:lib_dir(mpegts,ebin)++ "/mpegts_reader", 0),
%   io:format("Load mpegts_reader: ~p~n", [Load]),
%   ok.


start_link(Options) ->
  {ok, proc_lib:spawn_link(?MODULE, init, [[Options]])}.

set_socket(Reader, Socket) when is_pid(Reader) andalso is_port(Socket) ->
  gen_tcp:controlling_process(Socket, Reader),
  gen_server:call(Reader, {set_socket, Socket}).

init([Options]) ->
  Consumer = proplists:get_value(consumer, Options),
  erlang:monitor(process, Consumer),
  synchronizer(#ts_lander{consumer = Consumer, options = Options, pids = [#stream{pid = 0, handler = handle_pat}]}).

synchronizer(#ts_lander{} = TSLander) ->
  receive
    Message ->
      case (catch handle_message(Message, TSLander)) of
        {ok, TSLander1} -> synchronizer(TSLander1);
        ok -> ok;
        {'EXIT', Reason} -> 
          error_logger:error_msg("MPEGTS reader died: ~p~n", [Reason]),
          {error, Reason}
      end
  end.
  
handle_message({'DOWN', _Ref, process, Consumer, normal}, #ts_lander{consumer = Consumer}) ->
  ok;
handle_message({'DOWN', _Ref, process, _Pid, normal}, #ts_lander{}) ->
  ?D({"MPEG TS reader lost pid handler", _Pid}),
  ok;

handle_message({'$gen_call', From, {set_socket, Socket}}, #ts_lander{} = TSLander) ->
  inet:setopts(Socket, [{packet,raw},{active,once}]),
  % ?D({passive_accepted, Socket}),
  gen:reply(From, ok),
  {ok, TSLander#ts_lander{socket = Socket}};
  

handle_message({'$gen_call', From, connect}, #ts_lander{options = Options} = TSLander) ->
  URL = proplists:get_value(url, Options),
  Timeout = proplists:get_value(timeout, Options, 2000),
  {Schema, _, _Host, _Port, _Path, _Query} = http_uri2:parse(URL),
  {ok, Socket} = case Schema of
    udp -> 
      connect_udp(URL);
    _ ->
      {ok, _Headers, Sock} = http_stream:get(URL, [{timeout,Timeout}]),
      inet:setopts(Sock, [{packet,raw},{active,once}]),
      {ok, Sock}
  end,
  ?D({connected, URL, Socket}),
  gen:reply(From, ok),
  {ok, TSLander#ts_lander{socket = Socket}};

handle_message({udp, Socket, _IP, _InPortNo, Bin}, #ts_lander{buffer = Buffer} = TSLander) ->
  inet:setopts(Socket, [{active,once}]),
  TSLander1 = case Buffer of
    <<>> -> synchronizer(Bin, TSLander);
    _ -> synchronizer(<<Buffer/binary, Bin/binary>>, TSLander)
  end,
  {ok, TSLander1};
  
handle_message({tcp, Socket, Bin}, #ts_lander{buffer = Buffer} = TSLander) ->
  inet:setopts(Socket, [{active,once}]),
  TSLander1 = case Buffer of
    <<>> -> synchronizer(Bin, TSLander);
    _ -> synchronizer(<<Buffer/binary, Bin/binary>>, TSLander)
  end,
  {ok, TSLander1};
  
handle_message({tcp_closed, _Socket}, _TSLander) ->
  ok;

handle_message({data, Bin}, #ts_lander{buffer = Buffer} = TSLander) ->
  TSLander1 = case Buffer of
    <<>> -> synchronizer(Bin, TSLander);
    _ -> synchronizer(<<Buffer/binary, Bin/binary>>, TSLander)
  end,
  {ok, TSLander1};

handle_message(Else, _TSLander) ->
  ?D({"MPEG TS reader", Else}),
  ok.
    
    
connect_udp(URL) ->
  {_, _, Host, Port, _Path, _Query} = http_uri2:parse(URL),
  {ok, Addr} = inet_parse:address(Host),
  {ok, Socket} = gen_udp:open(Port, [binary,{active,once},{recbuf,65536},inet,{ip,Addr}]),
  {ok, Socket}.
  
    

synchronizer(<<16#47, _:187/binary, 16#47, _/binary>> = Bin, TSLander) ->
  {Packet, Rest} = split_binary(Bin, 188),
  Lander = demux(TSLander, Packet),
  synchronizer(Rest, Lander);

synchronizer(<<_, Bin/binary>>, TSLander) when size(Bin) >= 374 ->
  ?D(desync),
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
      % ?D("ZZZ"),
      Handler ! {ts_packet, Header, ts_payload(Packet)},
      receive
        {ok, Pid} -> ok
      after
        1000 -> 
          error_logger:error_msg("Pid ~p failed to reply", [Pid]),
          erlang:exit({pid_timeout,Pid})
      end,
      TSLander;
    false ->
      % ?D({none,Pid,Pids}),
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
  ?D({"Selecting MPEG-TS program", ProgramNum}),
  % io:format("Program info: ~p~n", [ProgramInfo]),
  % ?D({"PMT", size(PMT), PMTLength, _ProgramInfo}),
  Descriptors = extract_pmt(PMT, PMTLength, []),
  % io:format("Streams: ~p~n", [Descriptors]),
  Descriptors1 = lists:map(fun(#stream{pid = Pid} = Stream) ->
    case lists:keyfind(Pid, #stream.pid, Pids) of
      false ->
        Handler = proc_lib:spawn_link(?MODULE, pes, [Stream#stream{demuxer = self(), program_num = ProgramNum, consumer = Consumer, h264 = #h264{}}]),
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
  extract_pmt(Rest, PMTLength - 5 - ESLength, [#stream{handler = pes, counter = 0, pid = Pid, codec = stream_codec(StreamType)}|Descriptors]).
  


stream_codec(?TYPE_VIDEO_H264) -> h264;
stream_codec(?TYPE_VIDEO_MPEG2) -> mpeg2video;
stream_codec(?TYPE_AUDIO_AAC) -> aac;
stream_codec(?TYPE_AUDIO_AAC2) -> aac;
stream_codec(?TYPE_AUDIO_MPEG2) -> mpeg2audio;
stream_codec(Type) -> ?D({"Unknown TS PID type", Type}), unhandled.

pes(#stream{demuxer = Demuxer, synced = false, pid = Pid} = Stream) ->
  receive
    {ts_packet, #ts_header{payload_start = 0}, _} ->
      ?D({"Not synced pes", Pid}),
      Demuxer ! {ok, Pid},
      ?MODULE:pes(Stream);
    {ts_packet, #ts_header{payload_start = 1}, Packet} ->
      ?D({"Synced PES", Pid}),
      stream_timestamp(Packet, Stream),
      Stream1 = Stream#stream{synced = true, ts_buffer = [Packet]},
      Demuxer ! {ok, Pid},
      ?MODULE:pes(Stream1);
    {ts_packet, #ts_header{}, _} ->
      % ?D({"Not synced pes", Pid}),
      Demuxer ! {ok, Pid},
      ?MODULE:pes(Stream);
    Other ->
      ?D({"Undefined message to unsynced pid", Pid, Other})
  end;
  
pes(#stream{demuxer = Demuxer, synced = true, pid = Pid, ts_buffer = Buf} = Stream) ->
  receive
    {ts_packet, #ts_header{payload_start = 0} = Header, Packet} ->
      Stream1 = copy_pcr(Header, Stream#stream{synced = true, ts_buffer = [Packet | Buf]}),
      Demuxer ! {ok, Pid},
      ?MODULE:pes(Stream1);
    {ts_packet, #ts_header{payload_start = 1} = Header, Packet} ->
      PES = iolist_to_binary(lists:reverse(Buf)),
      Stream1 = stream_timestamp(Packet, copy_pcr(Header, Stream)),
      % ?D({Stream1#stream.type, Stream1#stream.pcr, Stream1#stream.dts}),
      Stream2 = pes_packet(PES, Stream1),
      Demuxer ! {ok, Pid},
      ?MODULE:pes(Stream2#stream{ts_buffer = [Packet]});
    Other ->
      ?D({"Undefined message to synced pid", Pid, Other})
  end.

copy_pcr(#ts_header{pcr = undefined}, Stream) -> Stream;
copy_pcr(#ts_header{pcr = PCR}, Stream) -> Stream#stream{pcr = PCR}.
      
pes_packet(_, #stream{codec = unhandled} = Stream) -> Stream#stream{ts_buffer = []};

pes_packet(_, #stream{dts = undefined} = Stream) ->
  ?D({"No PCR or DTS yes"}),
  Stream#stream{ts_buffer = []};

pes_packet(<<1:24, _:5/binary, Length, _PESHeader:Length/binary, Data/binary>>, #stream{codec = aac, es_buffer = Buffer} = Stream) ->
  % ?D({"Audio", Stream1#stream.pcr, Stream1#stream.dts}),
  % Stream1;
  decode_aac(Stream#stream{es_buffer = <<Buffer/binary, Data/binary>>});
  
pes_packet(<<1:24, _:5/binary, Length, _PESHeader:Length/binary, Rest/binary>>, #stream{es_buffer = Buffer, codec = h264} = Stream) ->
  % ?D({"Timestamp1", Stream#stream.timestamp, Stream#stream.start_time}),
  % ?D({"Video", Stream1#stream.pcr, Stream1#stream.dts}),
  % ?D({avc, Stream#stream.dts, <<Buffer/binary, Rest/binary>>}),
  decode_avc(Stream#stream{es_buffer = <<Buffer/binary, Rest/binary>>});


pes_packet(<<1:24, _:5/binary, Length, _PESHeader:Length/binary, Data/binary>>, #stream{codec = mpeg2audio, dts = DTS, pts = PTS, consumer = Consumer} = Stream) ->
  AudioFrame = #video_frame{       
    content = audio,
    flavor  = frame,
    dts     = DTS,
    pts     = PTS,
    body    = Data,
	  codec	  = mpeg2audio,
	  sound	  = {stereo, bit16, rate44}
  },
  % ?D({audio, Stream#stream.pcr, DTS}),
  Consumer ! AudioFrame,
  Stream;


pes_packet(<<1:24, _:5/binary, Length, _PESHeader:Length/binary, Data/binary>>, #stream{codec = mpeg2video, dts = DTS, pts = PTS, consumer = Consumer} = Stream) ->
  AudioFrame = #video_frame{       
    content = video,
    flavor  = frame,
    dts     = DTS,
    pts     = PTS,
    body    = Data,
	  codec	  = mpeg2video
  },
  % ?D({audio, Stream#stream.pcr, DTS}),
  Consumer ! AudioFrame,
  Stream.

pes_timestamp(<<_:7/binary, 2#11:2, _:6, PESHeaderLength, PESHeader:PESHeaderLength/binary, _/binary>>) ->
  <<2#0011:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1, 
    2#0001:4, Dts1:3, 1:1, Dts2:15, 1:1, Dts3:15, 1:1, _Rest/binary>> = PESHeader,
  <<PTS1:33>> = <<Pts1:3, Pts2:15, Pts3:15>>,
  <<DTS1:33>> = <<Dts1:3, Dts2:15, Dts3:15>>,
  {DTS1 / 90, PTS1 / 90};

pes_timestamp(<<_:7/binary, 2#10:2, _:6, PESHeaderLength, PESHeader:PESHeaderLength/binary, _/binary>>) ->
  <<2#0010:4, Pts1:3, 1:1, Pts2:15, 1:1, Pts3:15, 1:1, _Rest/binary>> = PESHeader,
  <<PTS1:33>> = <<Pts1:3, Pts2:15, Pts3:15>>,
  % ?D({pts, PTS1}),
  {undefined, PTS1/90};

pes_timestamp(_) ->
  {undefined, undefined}.
  
stream_timestamp(PES, Stream) ->
  {DTS, PTS} = pes_timestamp(PES),
  % ?D({Stream#stream.pid, DTS, PTS}),
  guess_timestamp(DTS, PTS, Stream).
  
  
guess_timestamp(DTS, PTS, Stream) when is_number(DTS) andalso is_number(PTS) ->
  normalize_timestamp(Stream#stream{dts = DTS, pts = PTS});
  
guess_timestamp(undefined, PTS, Stream) when is_number(PTS) ->
  normalize_timestamp(Stream#stream{dts = PTS, pts = PTS});

% FIXME!!!
% Here is a HUGE hack. VLC give me stream, where are no DTS or PTS, only PCR, once a second,
% thus I increment timestamp counter on each NAL, assuming, that there is 25 FPS.
% This is very, very wrong, but I really don't know how to calculate it in other way.
% stream_timestamp(_, #stream{pcr = PCR} = Stream, _) when is_number(PCR) ->
%   % ?D({"Set DTS to PCR", PCR}),
%   normalize_timestamp(Stream#stream{dts = PCR, pts = PCR});
guess_timestamp(undefined, undefined, #stream{dts = DTS, pts = PTS, pcr = PCR, start_dts = Start} = Stream) when is_number(PCR) andalso is_number(DTS) andalso is_number(Start) andalso PCR == DTS + Start ->
  % ?D({"Increasing", DTS}),
  Stream#stream{dts = DTS + 40, pts = PTS + 40};
  % Stream;

guess_timestamp(undefined, undefined, #stream{dts = DTS, pts = PTS, pcr = undefined} = Stream) when is_number(DTS) andalso is_number(PTS) ->
  ?D({none, PTS, DTS}),
  % ?D({"Have no timestamps", DTS}),
  Stream#stream{dts = DTS + 40, pts = PTS + 40};

guess_timestamp(undefined, undefined,  #stream{pcr = PCR, start_dts = undefined} = Stream) when is_number(PCR) ->
  guess_timestamp(undefined, undefined,  Stream#stream{start_dts = 0});

guess_timestamp(undefined, undefined,  #stream{pcr = PCR} = Stream) when is_number(PCR) ->
  % ?D({no_dts, PCR, Stream#stream.dts, Stream#stream.start_dts, Stream#stream.pts}),
  % ?D({"No DTS, taking", PCR - (Stream#stream.dts + Stream#stream.start_dts), PCR - (Stream#stream.pts + Stream#stream.start_dts)}),
  normalize_timestamp(Stream#stream{pcr = PCR, dts = PCR, pts = PCR});
  
guess_timestamp(undefined, undefined, #stream{pcr = undefined, dts = undefined} = Stream) ->
  ?D({"Not timestamps at all"}),
  Stream.


% normalize_timestamp(Stream) -> Stream;
% normalize_timestamp(#stream{start_dts = undefined, dts = DTS} = Stream) when is_number(DTS) -> 
%   normalize_timestamp(Stream#stream{start_dts = DTS});
% normalize_timestamp(#stream{start_dts = undefined, pts = PTS} = Stream) when is_number(PTS) -> 
%   normalize_timestamp(Stream#stream{start_dts = PTS});

% normalize_timestamp(#stream{start_dts = undefined, pcr = PCR} = Stream) when is_number(PCR) -> 
%   normalize_timestamp(Stream#stream{start_dts = PCR});
% 
% normalize_timestamp(#stream{start_dts = undefined, dts = DTS} = Stream) when is_number(DTS) andalso DTS > 0 -> 
%   normalize_timestamp(Stream#stream{start_dts = DTS});
% 
% normalize_timestamp(#stream{start_dts = Start, dts = DTS, pts = PTS} = Stream) when is_number(Start) andalso Start > 0 -> 
%   % ?D({"Normalize", Stream#stream.pid, round(DTS - Start), round(PTS - Start)}),
%   Stream#stream{dts = DTS - Start, pts = PTS - Start};
normalize_timestamp(Stream) ->
  Stream.
  
% normalize_timestamp(#stream{start_pcr = 0, pcr = PCR} = Stream) when is_integer(PCR) andalso PCR > 0 -> 
%   Stream#stream{start_pcr = PCR, pcr = 0};
% normalize_timestamp(#stream{start_pcr = Start, pcr = PCR} = Stream) -> 
%   Stream#stream{pcr = PCR - Start}.
% normalize_timestamp(Stream) -> Stream.



% <<18,16,6>>
decode_aac(#stream{send_audio_config = false, es_buffer = AAC, dts = DTS, pts = PTS, consumer = Consumer} = Stream) ->
  Config = aac:adts_to_config(AAC),
  AudioConfig = #video_frame{       
   	content = audio,
   	flavor  = config,
		dts     = DTS,
		pts     = PTS,
		body    = Config,
	  codec	  = aac,
	  sound	  = {stereo, bit16, rate44}
	},
	Consumer ! AudioConfig,
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
    content = audio,
    flavor  = frame,
    dts     = DTS,
    pts     = PTS,
    body    = Data,
	  codec	  = aac,
	  sound	  = {stereo, bit16, rate44}
  },
  % ?D({audio, Stream#stream.pcr, DTS}),
  Consumer ! AudioFrame,
  Stream#stream{es_buffer = <<>>}.
  

decode_avc(#stream{es_buffer = Data} = Stream) ->
  case extract_nal(Data) of
    undefined ->
      Stream;
    {ok, NAL, Rest} ->
      % ?D(NAL),
      Stream1 = handle_nal(Stream#stream{es_buffer = Rest}, NAL),
      decode_avc(Stream1)
  end.

handle_nal(Stream, <<_:3, 9:5, _/binary>>) ->
  Stream;

handle_nal(#stream{consumer = Consumer, dts = DTS, pts = PTS, h264 = H264} = Stream, NAL) ->
  {H264_1, Frames} = h264:decode_nal(NAL, H264),
  case {h264:has_config(H264), h264:has_config(H264_1)} of
    {false, true} -> 
      Config = h264:video_config(H264_1),
      Consumer ! Config#video_frame{dts = DTS, pts = DTS};
    _ -> ok
  end,
  lists:foreach(fun(Frame) ->
    % ?D({Frame#video_frame.flavor, round(DTS)}),
    Consumer ! Frame#video_frame{dts = DTS, pts = PTS}
  end, Frames),
  Stream#stream{h264 = H264_1}.


extract_nal(Data) -> extract_nal_erl(Data).

extract_nal_erl(Data) ->
  find_nal_start_erl(Data).

find_nal_start_erl(<<1:32, Rest/binary>>) ->
  find_and_extract_nal(Rest);

find_nal_start_erl(<<1:24, Rest/binary>>) ->
  find_and_extract_nal(Rest);

find_nal_start_erl(<<>>) ->
  undefined;
  
find_nal_start_erl(<<_, Rest/binary>>) ->
  find_nal_start_erl(Rest).

find_and_extract_nal(Bin) ->
  Length = find_nal_end_erl(Bin, 0),
  <<NAL:Length/binary, Rest/binary>> = Bin,
  {ok, NAL, Rest}.
  
  
find_nal_end_erl(<<1:32, _/binary>>, Len) -> Len;
find_nal_end_erl(<<1:24, _/binary>>, Len) -> Len;
find_nal_end_erl(<<>>, Len) -> Len;
find_nal_end_erl(<<_, Rest/binary>>, Len) -> find_nal_end_erl(Rest, Len+1).


      


-include_lib("eunit/include/eunit.hrl").

benchmark() ->
  N = 100000,
  extract_nal_erl_bm(N),
  extract_nal_c_bm(N).

nal_test_bin(large) ->
  <<0,0,0,1,
    0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,  %54
    0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,  %104
    0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,  %154
    0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,  %204
    0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,  %254
    0,0,0,1,
    0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9, %308
    0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,  %358
    0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,  %408
    0,0,0,1>>;

nal_test_bin(filler) ->
  <<0,0,0,1,9,80,
    0,0,0,1,6,0,1,192,128,
    0,0,0,1,6,1,1,36,128,
    0,0,0,1,1,174,15,3,234,95,253,83,176,
              187,255,13,246,196,189,93,100,111,80,30,30,167,
              220,41,236,119,135,93,159,204,2,57,132,207,28,
              91,54,128,228,85,112,81,129,18,140,99,90,53,128,
    0,0,0,1,12,255,255,255,255,255,255,255,255,255,255,255,255,255,128,
    0,0,0,1,12,255,255,255,255,255,255,255,255,255,255,255,255,255,255>>;                                                                                            
  
nal_test_bin(small) ->
  <<0,0,0,1,9,224,0,0,1,104,206,50,200>>.

extract_nal_test() ->
  ?assertEqual({ok, <<9,224>>, <<>>}, extract_nal(<<0,0,1,9,224>>)),
  ?assertEqual({ok, <<9,224>>, <<0,0,1,104,206,50,200>>}, extract_nal(nal_test_bin(small))),
  ?assertEqual({ok, <<104,206,50,200>>, <<>>}, extract_nal(<<0,0,1,104,206,50,200>>)),
  ?assertEqual(undefined, extract_nal(<<>>)).
  
extract_nal_erl_test() ->  
  ?assertEqual({ok, <<9,224>>, <<0,0,1,104,206,50,200>>}, extract_nal_erl(nal_test_bin(small))),
  ?assertEqual({ok, <<104,206,50,200>>, <<>>}, extract_nal_erl(<<0,0,0,1,104,206,50,200>>)),
  ?assertEqual(undefined, extract_nal_erl(<<>>)).

extract_real_nal_test() ->
  Bin = nal_test_bin(filler),
  {ok, <<9,80>>, Bin1} = extract_nal(Bin),
  {ok, <<6,0,1,192,128>>, Bin2} = extract_nal(Bin1),
  {ok, <<6,1,1,36,128>>, Bin3} = extract_nal(Bin2),
  {ok, <<1,174,15,3,234,95,253,83,176,
            187,255,13,246,196,189,93,100,111,80,30,30,167,
            220,41,236,119,135,93,159,204,2,57,132,207,28,
            91,54,128,228,85,112,81,129,18,140,99,90,53,128>>, Bin4} = extract_nal(Bin3),
  {ok, <<12,255,255,255,255,255,255,255,255,255,255,255,255,255,128>>, Bin5} = extract_nal(Bin4),
  {ok, <<12,255,255,255,255,255,255,255,255,255,255,255,255,255,255>>, <<>>} = extract_nal(Bin5).


extract_nal_erl_bm(N) ->
  Bin = nal_test_bin(large),
  T1 = erlang:now(),
  lists:foreach(fun(_) ->
    extract_nal_erl(Bin)
  end, lists:seq(1,N)),
  T2 = erlang:now(),
  ?D({"Timer erl", timer:now_diff(T2, T1) / N}).

extract_nal_c_bm(N) ->
  Bin = nal_test_bin(large),
  T1 = erlang:now(),
  lists:foreach(fun(_) ->
    extract_nal(Bin)
  end, lists:seq(1,N)),
  T2 = erlang:now(),
  ?D({"Timer native", timer:now_diff(T2, T1) / N}).





