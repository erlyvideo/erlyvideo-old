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
-include_lib("erlmedia/include/aac.hrl").
-include("log.hrl").
-include("../include/mpegts.hrl").
-include("../include/mpegts_psi.hrl").
% -include("mpegts_reader.hrl").

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("kernel/include/file.hrl").

-define(MAX_PAYLOAD, 16#100000000).

-record(decoder, {
  buffer = <<>>,
  pids = [],
  frames = [],
  consumer,
  pmt_pid,
  socket,
  options,
  byte_counter = 0,
  program_info = [],
  dump_psi = [],
  sdt,
  current_time
}).


-record(stream, {
  pid,
  program_num,
  demuxer,
  handler,
  codec,
  ts_buffer = undefined,
  es_buffer = <<>>,
  counter = 0,
  payload_size = ?MAX_PAYLOAD,
  pcr,
  start_dts,
  dts,
  pts,
  video_config = undefined,
  send_audio_config = false,
  sample_rate,
  h264
}).


-export([benchmark/0]).

-define(PID_TYPE(Pid), case lists:keyfind(Pid, #stream.pid, Pids) of #stream{codec = h264} -> "V"; _ -> "A" end).

-on_load(load_nif/0).



-export([extract_nal/1, adapt_field_info/1]).

-export([start_link/1, set_socket/2]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-export([decode/2, decode_ts/2, decode_pes/2]).

-export([pes/3, psi/3, emm/3]).
-export([program_info/1]).

-export([file_packets/1, packet/2]).

file_packets(Path) ->
  {ok, #file_info{size = Size}} = file:read_file_info(Path),
  Size rem 188 == 0 orelse throw({invalid_file_size, Size}),
  Size div 188.

packet(Path, N) ->
  {ok, F} = file:open(Path, [binary,raw,read]),
  {ok, <<16#47, _:1, Start:1, _:1, Pid:13, _:4, Counter:4>>} = file:pread(F, N*188, 4),
  file:close(F),
  {if Start == 1 -> start; true -> continue end, Pid, Counter }.

load_nif() ->
  Load = erlang:load_nif(code:lib_dir(mpegts,priv)++ "/mpegts_reader", 0),
  io:format("Load mpegts_reader: ~p~n", [Load]),
  ok.


start_link(Options) ->
  gen_server_ems:start_link(?MODULE, [Options], []).

set_socket(Reader, Socket) when is_pid(Reader) andalso is_port(Socket) ->
  ok = gen_tcp:controlling_process(Socket, Reader),
  gen_server:call(Reader, {set_socket, Socket}).


program_info(MpegTS) when is_pid(MpegTS) ->
  gen_server:call(MpegTS, program_info).

init([]) ->
  init([[]]);

init([Options]) ->
  Consumer = case proplists:get_value(consumer, Options) of
    undefined -> undefined;
    Cons when is_pid(Cons) ->
      erlang:monitor(process, Cons),
      Cons
  end,
  DumpPSI = proplists:get_value(dump_psi, Options, []),
  {ok, #decoder{consumer = Consumer, options = Options, pids = [
    #stream{handler = psi, pid = ?CAT_PID},
    #stream{handler = psi, pid = ?NIT_PID},
    #stream{handler = psi, pid = ?SDT_PID},
    #stream{handler = psi, pid = ?EIT_PID},
    #stream{handler = psi, pid = ?RST_PID},
    #stream{handler = psi, pid = ?TDT_PID}
  ], dump_psi = DumpPSI}}.



handle_call({set_socket, Socket}, _From, #decoder{} = Decoder) ->
  ok = inet:setopts(Socket, [{packet,raw},{active,once}]),
  % ?D({passive_accepted, Socket}),
  {reply, ok, Decoder#decoder{socket = Socket}};


handle_call(program_info, _From, #decoder{program_info = Program} = Decoder) ->
  {reply, Program, Decoder};

handle_call(connect, _From, #decoder{options = Options} = Decoder) ->
  URL = proplists:get_value(url, Options),
  Timeout = proplists:get_value(timeout, Options, 2000),
  {Schema, _, _Host, _Port, _Path, _Query} = http_uri2:parse(URL),
  case Schema of
    udp -> 
      connect_udp(URL);
    _ ->
      case  http_stream:get(URL, [{timeout,Timeout}]) of 
	{ok,_Headers,Socket} ->
	  ok = inet:setopts(Socket, [{packet,raw},{active,once}]),
	  ?D({connected, URL, Socket}),
	  {reply, ok, Decoder#decoder{socket = Socket}};
	{error,Reason} ->
	  {stop,{error,Reason},Decoder}
      end
  end;
    
handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.
  
handle_info({'DOWN', _Ref, process, Consumer, _Reason}, #decoder{consumer = Consumer} = State) ->
  {stop, normal, State};
  
handle_info({'DOWN', _Ref, process, _Pid, Reason}, #decoder{} = State) ->
  ?D({"MPEG TS reader lost pid handler", _Pid}),
  {stop, Reason, State};


handle_info({udp, Socket, _IP, _InPortNo, Bin}, #decoder{consumer = Consumer} = Decoder) ->
  inet:setopts(Socket, [{active,once}]),
  {ok, Decoder1, Frames} = decode(Bin, Decoder),
  [Consumer ! Frame || Frame <- Frames],
  {noreply, Decoder1};
  
handle_info({tcp, Socket, Bin}, #decoder{consumer = Consumer} = Decoder) ->
  inet:setopts(Socket, [{active,once}]),
  {ok, Decoder1, Frames} = decode(Bin, Decoder),
  [Consumer ! Frame || Frame <- Frames],
  {noreply, Decoder1};
  
handle_info({tcp_closed, _Socket}, Decoder) ->
  {stop, normal, Decoder};

handle_info({data, Bin}, #decoder{consumer = Consumer} = Decoder) ->
  {ok, Decoder1, Frames} = decode(Bin, Decoder),
  [Consumer ! Frame || Frame <- Frames],
  {noreply, Decoder1};

handle_info(Else, Decoder) ->
  {stop, {unknown_message, Else}, Decoder}.


handle_cast(Cast, Decoder) ->
  {stop, {unknown_cast, Cast}, Decoder}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

    
connect_udp(URL) ->
  {_, _, Host, Port, _Path, _Query} = http_uri2:parse(URL),
  {ok, Addr} = inet_parse:address(Host),
  {ok, Socket} = gen_udp:open(Port, [binary,{active,once},{recbuf,65536},inet,{ip,Addr}]),
  {ok, Socket}.
  


decode(Bin, #decoder{buffer = <<>>} = Decoder) when is_binary(Bin) ->
  decode(Bin, Decoder#decoder{}, []);

decode(Bin, #decoder{buffer = Buffer} = Decoder) when is_binary(Bin) ->
  decode(<<Buffer/binary, Bin/binary>>, Decoder, []).

decode(<<16#47, Packet:187/binary, Rest/binary>>, Decoder, Frames) ->
  case decode_ts(Packet, Decoder) of
    {ok, Decoder1, undefined} -> 
      decode(Rest, Decoder1, Frames);
    {ok, Decoder1, PESPacket} -> 
      {ok, Decoder2, Frames1} = decode_pes(Decoder1, PESPacket),
      decode(Rest, Decoder2, Frames ++ Frames1)
  end;

decode(<<_, Bin/binary>>, Decoder, Frames) when size(Bin) >= 374 ->
  % ?D(desync),
  decode(Bin, Decoder, Frames);

decode(Bin, #decoder{frames = Buffered} = Decoder, Frames) ->
  Sorted = lists:sort(fun flv:frame_sorter/2, Buffered ++ Frames),
  % case Sorted == Buffered ++ Frames of true -> ?D(sort_ok); false -> ?D({sorting_changed_order}) end,
  {Reply, Save} = if
    length(Sorted) > 4 -> lists:split(length(Sorted) - 4, Sorted);
    true -> {[], Sorted}
  end,
  {ok, Decoder#decoder{buffer = Bin, frames = Save}, Reply}.

decode_ts(<<_:3, ?PAT_PID:13, _/binary>> = Packet, Decoder) ->
  handle_psi(ts_payload(Packet), Decoder);
      

decode_ts(<<_:3, _Pid:13, Scrambling:2, _:6, _/binary>>, Decoder) when Scrambling > 0 ->
  % ?D({scrambled, Pid}),
  {ok, Decoder, undefined};


decode_ts(<<_Error:1, PayloadStart:1, _TransportPriority:1, Pid:13, _Scrambling:2,
            _HasAdaptation:1, _HasPayload:1, _Counter:4, _TSRest/binary>> = Packet, #decoder{pids = Pids} = Decoder) ->
  PCR = get_pcr(Packet),
  Payload = ts_payload(Packet),
  % Keyframe = case {HasAdaptation, TSRest} of
  %   {1, <<_:1, 1:1, _/bitstring>>} -> keyframe; % random_access_indicator is 1
  %   _ -> frame
  % end,
  % io:format("ts: ~p (~p) ~p~n", [Pid,PayloadStart, Keyframe]),
  case lists:keytake(Pid, #stream.pid, Pids) of
    {value, #stream{ts_buffer = undefined, handler = psi} = Stream, Streams} when PayloadStart == 1 ->
      <<_:20, Len:12, _/binary>> = Payload,
      Length = Len + 4, % size of PSI payload plus header
      if
        size(Payload) >= Length ->
          % ?D({early_flush,Pid,Len,size(Payload)}),
          ?MODULE:psi(Payload, Stream#stream{ts_buffer = undefined}, Decoder#decoder{pids = Streams});
        true ->
          {ok, Decoder#decoder{pids = [Stream#stream{ts_buffer = Payload, payload_size = Length}|Streams]}, undefined}
      end;
    
    % This clause happens when comes first continuation packet on new decoder
    {value, #stream{ts_buffer = undefined}, _} when PayloadStart == 0 ->
      ?D({"Not synced pes", Pid}),
      {ok, Decoder, undefined};

    % This clauses happens when comes first start-payload packet on new decoder
    {value, #stream{ts_buffer = undefined} = Stream, Streams} when PayloadStart == 1 -> %  andalso Keyframe == true
      ?D({"Synced PES", Pid}),
      {ok, Decoder#decoder{pids = [Stream#stream{pcr = PCR, ts_buffer = Payload}|Streams]}, undefined};

    % This clauses happens when start-payload packets comes to already filled stream decoder
    {value, #stream{ts_buffer = <<>>, payload_size = PayloadSize} = Stream, Streams} 
      when PayloadStart == 1 andalso size(Payload) < PayloadSize ->  
    {ok, Decoder#decoder{pids = [Stream#stream{ts_buffer = Payload}|Streams]}, undefined};
    
    
    {value, #stream{ts_buffer = Buf, handler = Handler, payload_size = PayloadSize} = Stream, Streams} 
      when PayloadStart == 1 orelse size(Buf) + size(Payload) >= PayloadSize ->
        
      % Здесь надо решить: что показываем, что сохраняем
      % Могут быть разные варианты: мы натолкнулись на начало нового PES-пакета, а старый в буфере
      % или мы вытащили TS пакет, а в нём всё содержимое, которое сразу надо сбросить
      % Или мы вытащили какой-то по очереди TS-пакет, наполнили им буфер и заполнили требуемый PayloadSize
      {Body, Rest} = case {PayloadStart, Buf} of
        {1, <<>>} -> {Payload, <<>>}; % Если у нас начало нового PayloadStart,  но пустой буфер, текущий Payload — единственный 
        {1, _} -> {Buf, Payload};
        {0, _} -> {<<Buf/binary, Payload/binary>>, undefined}
      end,
      Stream1 = stream_timestamp(Body, Stream#stream{pcr = PCR, ts_buffer = Rest}),
      % case Handler of
      %   psi ->
      %     <<_, TableId, _:4, Len:12, _/binary>> = Body,
      %     ?D({Handler, TableId, Pid, size(Body), PayloadStart, PayloadSize, Len});
      %   _ -> ok
      % end,
      ?MODULE:Handler(Body, Stream1, Decoder#decoder{pids = Streams});

    % This clause happens when continuation packets comes to initialized decoder
    {value, #stream{ts_buffer = Buf} = Stream, Streams} when PayloadStart == 0 andalso is_binary(Buf) ->
      {ok, Decoder#decoder{pids = [Stream#stream{pcr = PCR, ts_buffer = <<Buf/binary, Payload/binary>>}|Streams]}, undefined};

    false ->
      % ?D({unknown_pid, Pid}),
      {ok, Decoder, undefined}
  end;

decode_ts({eof,Codec}, #decoder{pids = Pids} = Decoder) ->
  case lists:keytake(Codec, #stream.codec, Pids) of
    {value, #stream{ts_buffer = Body, handler = pes} = Stream, Streams} ->
      % ?D({eof,Codec,Body}),
      Stream1 = stream_timestamp(Body, Stream),
      pes(Body, Stream1, Decoder#decoder{pids = Streams});
    false ->
      % ?D({unknown_pid, Pid}),
      {ok, Decoder, undefined}
  end.

ts_payload(<<_TEI:1, _Start:1, _Priority:1, _Pid:13, _Scrambling:2, 0:1, 1:1, _Counter:4, Payload/binary>>)  -> 
  Payload;

ts_payload(<<_TEI:1, _Start:1, _Priority:1, _Pid:13, _Scrambling:2, 1:1, 1:1, _Counter:4, 
              AdaptationLength, _AdaptationField:AdaptationLength/binary, Payload/binary>>) -> 
  Payload;

ts_payload(<<_TEI:1, _Start:1, _Priority:1, _Pid:13, _Scrambling:2, 
              _Adaptation:1, 0:1, _Counter:4, _Payload/binary>>)  ->
  % ?D({"Empty payload on pid", _Pid}),
  <<>>.


get_pcr(<<_:18, 1:1, _:5, Length, AdaptationField:Length/binary, _/binary>>) when Length > 0 ->
  extract_pcr(AdaptationField);
  
get_pcr(_) ->
  undefined.

extract_pcr(<<_Discontinuity:1, _RandomAccess:1, _Priority:1, PCR:1, _OPCR:1, _Splice:1, _Private:1, _Ext:1, Pcr1:33, Pcr2:9, _/bitstring>>) when PCR == 1 ->
  Pcr1 / 90 + Pcr2 / 27000;
extract_pcr(_) ->
  undefined.

adapt_field_info(<<_:18, 1:1, _:5, Length, Field:Length/binary, _/binary>>) when Length > 0 ->
  <<_Disc:1, RandomAccess:1, _/bitstring>> = Field,
  PCR = case extract_pcr(Field) of
    undefined -> "";
    PCR_ -> io_lib:format("~f", [PCR_])
  end,
  io_lib:format("~p ~s", [RandomAccess, PCR]);

adapt_field_info(_) -> "".


%%%%%%%%%%%%%%%   Program access table  %%%%%%%%%%%%%%


insert_pid(#stream{pid = Pid} = Stream, #decoder{pids = Streams} = Decoder) ->
  case lists:keymember(Pid, #stream.pid, Streams) of
    true -> Decoder;
    false -> ?D({"New pid",Stream#stream.handler,Stream#stream.codec,Pid}), Decoder#decoder{pids = [Stream|Streams]}
  end.

insert_pids(Streams, Decoder) ->
  lists:foldl(fun(Stream, Decoder1) ->
    insert_pid(Stream, Decoder1)
  end, Decoder, Streams).


psi(PSI, Stream, #decoder{pids = Streams} = Decoder) ->
  handle_psi(PSI, Decoder#decoder{pids = [Stream|Streams]}).

handle_psi(PSI, #decoder{dump_psi = Dump} = Decoder) ->
  Decoder1 = case mpegts_psi:psi(PSI) of
    {pat, Descriptors} -> 
      case lists:member(pat, Dump) of true -> io:format("PAT: ~p~n", [Descriptors]); false -> ok end,
      Streams = [#stream{handler = psi, pid = Pid, program_num = Program} || {Pid,Program} <- Descriptors],
      insert_pids(Streams, Decoder);
    {tdt, UTC} -> 
      Decoder#decoder{current_time = UTC};
    {cat, Descriptors} ->
      Streams = [#stream{handler = emm, pid = Pid} || #dvb_ca_desc{pid = Pid} <- Descriptors],
      insert_pids(Streams, Decoder);
    {pmt, Descriptors} ->
      case lists:member(pmt, Dump) of true -> io:format("PMT: ~p~n", [Descriptors]); false -> ok end,
      Streams = [#stream{handler = pes, pid = Pid, counter = 0, codec = Codec, program_num = Prnum, h264 = h264:init()} || 
                 #pmt_entry{pid = Pid, codec = Codec, program = Prnum} <- Descriptors],
      insert_pids(Streams, Decoder);
    {sdt, Info} ->
      Decoder#decoder{sdt = Info};
    % {eit, Events} ->
    %   Pids = [Pid || #stream{pid = Pid} <- Decoder#decoder.pids],
    %   
      % case lists:member(EitPid, Pids) of
      %   true ->
          % io:format("new EIT service_id=~p version=~p ts_id=~p network_id=~p~n", [_Pid, _Version, _TSId, _Network]),
          % [begin
          %   #eit_event{
          %     id = Id,
          %     start = Start,
          %     duration = Duration,
          %     status = Status,
          %     name = Name,
          %     language = Lang,
          %     about = About
          %   } = Event,
          %   io:format("  * event id=~p start_time:~p duration=~p running=~p~n    - short lang=~s '~s' : '~s'~n", [Id, Start, Duration, Status, Lang, Name, About])
          % end || Event <- _Info],
          % ?D({prg1, [Id || #eit_event{id = Id} <- NewProgram]}),
          % ?D({prg2, [Id || #eit_event{id = Id} <- Program]}),
          % Program = Decoder#decoder.program_info,
          % Decoder#decoder{program_info = lists:ukeymerge(#eit_event.id, Event, Program)};
      %   _ ->
      %     Decoder
      % end;
    _ ->
      Decoder
  end,
  {ok, Decoder1, undefined}.
  


pes(_, #stream{codec = unhandled} = Stream, #decoder{pids = Streams} = Decoder) ->
  {ok, Decoder#decoder{pids = [Stream|Streams]}, undefined};

pes(_, #stream{dts = undefined} = Stream, #decoder{pids = Streams} = Decoder) ->
  ?D({"No PCR or DTS yes"}),
  {ok, Decoder#decoder{pids = [Stream|Streams]}, undefined};

pes(<<1:24, _StreamId, _PESLength:16, _:2/binary, HeaderLength, _PESHeader:HeaderLength/binary, Data/binary>>, 
           #stream{es_buffer = Buffer, codec = Codec, pid = Pid, dts = DTS, pts = PTS} = Stream, #decoder{pids = Streams} = Decoder) ->
  % ?D({pes, _StreamId,_PESLength -3 - HeaderLength, size(Data)}),
  Body = case Buffer of
    <<>> -> Data;
    _ -> <<Buffer/binary, Data/binary>>
  end,
  % ?D({pes, Codec, DTS, size(Body)}),
  {ok, Decoder#decoder{pids = [Stream#stream{es_buffer = <<>>}|Streams]}, 
       #pes_packet{pid = Pid, codec = Codec, dts = DTS, pts = PTS, body = Body}}.


emm(_Packet, Stream, #decoder{pids = Streams} = Decoder) ->
  {ok, Decoder#decoder{pids = [Stream|Streams]}, undefined}.


decode_pes(#decoder{pids = Pids} = Decoder, #pes_packet{body = Body, pid = Pid}) ->
  case lists:keytake(Pid, #stream.pid, Pids) of
    {value, Stream, Streams} ->
      {Stream1, Frames} = decode_pes_packet(Stream#stream{es_buffer = Body}),
      {ok, Decoder#decoder{pids = [Stream1|Streams]}, Frames};
    _ ->
      {ok, Decoder, []}
  end.  


decode_pes_packet(#stream{codec = aac} = Packet) ->
  decode_aac(Packet);
  
decode_pes_packet(#stream{codec = h264} = Packet) ->
  decode_avc(Packet);


decode_pes_packet(#stream{codec = mp3, dts = DTS, pts = PTS, es_buffer = Data} = Stream) ->
  AudioFrame = #video_frame{       
    content = audio,
    flavor  = frame,
    dts     = DTS,
    pts     = PTS,
    body    = Data,
	  codec	  = mp3,
	  sound	  = {stereo, bit16, rate44}
  },
  % ?D({audio, Stream#stream.pcr, DTS}),
  {Stream, [AudioFrame]};

decode_pes_packet(#stream{codec = mpeg2audio, dts = DTS, pts = PTS, es_buffer = Data} = Stream) ->
  % TC1 = get(mp2_aac),
  % {TC2, Frames} = ems_sound2:mp2_aac(TC1, #video_frame{codec = mpeg2audio, pts = PTS, dts = DTS, body = Data, content = audio}),
  % put(mp2_aac, TC2),
  Frames = [#video_frame{
    content = audio,
    flavor = frame,
    dts = DTS,
    pts = PTS,
    body = Data,
    codec = mpeg2audio,
    sound = {stereo, bit16, rate44}
  }],
  {Stream#stream{es_buffer = <<>>}, Frames};


decode_pes_packet(#stream{dts = DTS, pts = PTS, es_buffer = Data, codec = mpeg2video} = Stream) when is_number(DTS) andalso is_number(PTS) ->
  % TC1 = get(mpeg2_h264),
  % {TC2, Frames} = ems_video:mpeg2_h264(TC1, #video_frame{codec = mpeg2video, pts = PTS, dts = DTS, body = Data, content = video}),
  % put(mpeg2_h264, TC2),
  Keyframe = detect_mp2v_keyframe(Data),
  % ?D({mpeg2video, round(DTS), Keyframe}),
  Frames = [#video_frame{
    content = video,
    flavor = Keyframe,
    dts = DTS,
    pts = PTS,
    body = Data,
    sound = undefined,
    codec = mpeg2video
  }],
  {Stream#stream{es_buffer = <<>>}, Frames}.
  
detect_mp2v_keyframe(Data) ->
  case extract_nal(Data) of
    undefined ->
      % ?D(false),
      frame;
    {ok, <<16#b3, _/binary>>, _Rest} ->
      keyframe;
    {ok, <<_Code, _/binary>>, Rest} ->
      % if
      %   Code == 181 -> ok;
      %   Code > 35 -> ?D({mp2v, Code});
      %   true -> ok
      % end,
      detect_mp2v_keyframe(Rest)
  end.
  
    
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
  

stream_timestamp(_, #stream{handler = psi} = Stream) ->
  Stream;
  
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
  ?D({"No timestamps at all"}),
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



decode_aac(#stream{send_audio_config = false, es_buffer = AAC, dts = DTS} = Stream) ->
  Config = aac:adts_to_config(AAC),
  #aac_config{sample_rate = SampleRate} = aac:decode_config(Config),
  AudioConfig = #video_frame{       
   	content = audio,
   	flavor  = config,
		dts     = DTS,
		pts     = DTS,
		body    = Config,
	  codec	  = aac,
	  sound	  = {stereo, bit16, rate44}
	},
	{Stream1, Frames} = decode_aac(Stream#stream{send_audio_config = true, sample_rate = SampleRate}),
	{Stream1, [AudioConfig] ++ Frames};
  

decode_aac(#stream{es_buffer = ADTS, dts = DTS, sample_rate = SampleRate} = Stream) ->
  % ?D({adts, DTS}),
  {Frames, Rest} = decode_adts(ADTS, DTS, SampleRate / 1000, 0, []),
  {Stream#stream{es_buffer = Rest}, Frames}.

decode_adts(<<>>, _BaseDTS, _SampleRate, _SampleCount, Frames) ->
  {lists:reverse(Frames), <<>>};

decode_adts(ADTS, BaseDTS, SampleRate, SampleCount, Frames) ->
  case aac:unpack_adts(ADTS) of
    {ok, Frame, Rest} ->
      DTS = BaseDTS + SampleCount / SampleRate,
      AudioFrame = #video_frame{       
        content = audio,
        flavor  = frame,
        dts     = DTS,
        pts     = DTS,
        body    = Frame,
    	  codec	  = aac,
    	  sound	  = {stereo, bit16, rate44}
      },
      % ?D({audio, Stream#stream.pcr, DTS}),
      decode_adts(Rest, BaseDTS, SampleRate, SampleCount + 1024, [AudioFrame|Frames]);
    {more, _} ->
      {lists:reverse(Frames), ADTS}
  end.
      
% decode_aac(#stream{es_buffer = <<_Syncword:12, _ID:1, _Layer:2, 0:1, _Profile:2, _Sampling:4,
%                                  _Private:1, _Channel:3, _Original:1, _Home:1, _Copyright:1, _CopyrightStart:1,
%                                  _FrameLength:13, _ADTS:11, _Count:2, _CRC:16, Rest/binary>>} = Stream) ->
%   send_aac(Stream#stream{es_buffer = Rest});
% 
% decode_aac(#stream{es_buffer = <<_Syncword:12, _ID:1, _Layer:2, _ProtectionAbsent:1, _Profile:2, _Sampling:4,
%                                  _Private:1, _Channel:3, _Original:1, _Home:1, _Copyright:1, _CopyrightStart:1,
%                                  _FrameLength:13, _ADTS:11, _Count:2, Rest/binary>>} = Stream) ->
%   % ?D({"AAC", Syncword, ID, Layer, ProtectionAbsent, Profile, Sampling, Private, Channel, Original, Home,
%   % Copyright, CopyrightStart, FrameLength, ADTS, Count}),
%   % ?D({"AAC", Rest}),
%   send_aac(Stream#stream{es_buffer = Rest}).
% 
% send_aac(#stream{es_buffer = Data, consumer = Consumer, dts = DTS, pts = PTS} = Stream) ->
%   % ?D({audio, }),
%   Stream#stream{es_buffer = <<>>}.
%   

decode_avc(#stream{es_buffer = Data, dts = DTS, pts = PTS, h264 = H264} = Stream) ->
  NALS = pes_to_nals(Data),
  Body = [[<<(size(NAL)):32>>, NAL] || <<_:3, Type:5, _/binary>> = NAL <- NALS, Type == ?NAL_IDR orelse Type == ?NAL_SINGLE],
  ConfigNALS = [NAL || <<_:3, Type:5,_/binary>> = NAL <- NALS, Type == ?NAL_SPS orelse Type == ?NAL_PPS],
  IDRS = [NAL || <<_:3, ?NAL_IDR:5,_/binary>> = NAL <- NALS],
  
  H264_1 = lists:foldl(fun(NAL, H264_) ->
    h264:parse_nal(NAL, H264_)
  end, H264, ConfigNALS),
  
  % ?D({avc,length(IDRS), length(SPS), length(PPS), h264:has_config(H264), h264:has_config(H264_1)}),
  
  ConfigFrames = case {h264:has_config(H264), h264:has_config(H264_1)} of
    {false, true} -> 
      Config = h264:video_config(H264_1),
      [Config#video_frame{dts = DTS, pts = DTS}];
    _ -> []
  end,
  
  
  Frame = #video_frame{
   	content = video,
		body    = iolist_to_binary(Body),
		flavor  = case length(IDRS) of 0 -> frame; _ -> keyframe end,
		codec   = h264,
		dts     = DTS,
		pts     = PTS
  },
  
  {Stream#stream{h264 = H264_1, es_buffer = <<>>}, ConfigFrames ++ [Frame]}.

% 
% decode_mpeg2_video(#stream{dts = DTS, pts = PTS, es_buffer = Data} = Stream, Frames) ->
%   case extract_nal(Data) of
%     undefined ->
%       {Stream, Frames};
%     {ok, Block, Rest} ->
%       VideoFrame = #video_frame{       
%         content = video,
%         flavor  = frame,
%         dts     = DTS,
%         pts     = PTS,
%         body    = Block,
%         codec   = mpeg2video
%       },
%       ?D({mpeg2video,size(Block),DTS}),
%       % ?D({video, round(DTS), size(Data)}),
%       decode_mpeg2_video(Stream#stream{es_buffer = Rest}, [VideoFrame|Frames])
%   end.

  

% handle_nal(Stream, <<_:3, 9:5, _/binary>>) ->
%   {Stream, []};
% 
% handle_nal(#stream{dts = DTS, pts = PTS, h264 = H264} = Stream, NAL) ->
%   % case get(first_dts) of undefined -> put(first_dts, DTS); _ -> ok end,
%   % ?D({h264, DTS - get(first_dts), PTS - get(first_dts)}),
%   {H264_1, Frames} = h264:decode_nal(NAL, H264),
%   ConfigFrames = case {h264:has_config(H264), h264:has_config(H264_1)} of
%     {false, true} -> 
%       Config = h264:video_config(H264_1),
%       [Config#video_frame{dts = DTS, pts = DTS}];
%     _ -> []
%   end,
%   {Stream#stream{h264 = H264_1}, ConfigFrames ++ [Frame#video_frame{dts = DTS, pts = PTS, body = <<2:32, 9,224, Body/binary>>} || #video_frame{body = Body} = Frame <- Frames]}.


pes_to_nals(PES) ->
  pes_to_nals(PES, []).
  
  
pes_to_nals(<<>>, Acc) ->
  lists:reverse(Acc);
  
pes_to_nals(PES, Acc) ->
  case extract_nal(PES) of
    {ok, NAL, Rest} ->
      pes_to_nals(Rest, [NAL|Acc]);
    undefined ->  
      % Here is very main decision taken: either to look for NAL end in next PES or reply now.
      % This clause means, that there is no NAL marker anymore
      LastNal = case PES of
        <<1:24, Rest/binary>> -> Rest;
        <<1:32, Rest/binary>> -> Rest;
        _ -> <<>>
      end,
      pes_to_nals(<<>>, [LastNal|Acc])
  end.
  

extract_nal(Data) ->
  case extract_nal1(Data) of
    undefined -> undefined;
    {ok, <<>>, Rest} -> extract_nal(Rest);
    {ok, NAL, Rest} -> {ok, NAL, Rest}
  end.

extract_nal1(Data) -> extract_nal_erl(Data).

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
  case find_nal_end_erl(Bin, 0) of
    undefined ->
      undefined; % this reply tells to look for NAL end in next PES and delays one frame
    Length ->
      <<NAL:Length/binary, Rest/binary>> = Bin,
      {ok, NAL, Rest}
  end.    
  
  
find_nal_end_erl(<<1:32, _/binary>>, Len) -> Len;
find_nal_end_erl(<<1:24, _/binary>>, Len) -> Len;
find_nal_end_erl(<<>>, _Len) -> undefined;
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
    0,0,0,1,12,255,255,255,255,255,255,255,255,255,255,255,255,255,255,0,0,1>>;                                                                                            
  
nal_test_bin(small) ->
  <<0,0,0,1,9,224,0,0,1,104,206,50,200>>.

extract_nal_test() ->
  ?assertEqual(undefined, extract_nal(<<0,0,1,9,224>>)),
  ?assertEqual({ok, <<9,224>>, <<0,0,1,104,206,50,200>>}, extract_nal(nal_test_bin(small))),
  ?assertEqual({ok, <<104,206,50,200>>, <<0,0,1>>}, extract_nal(<<0,0,1,104,206,50,200,0,0,1>>)),
  ?assertEqual(undefined, extract_nal(<<>>)).
  
extract_nal_erl_test() ->  
  ?assertEqual({ok, <<9,224>>, <<0,0,1,104,206,50,200>>}, extract_nal_erl(nal_test_bin(small))),
  ?assertEqual({ok, <<104,206,50,200>>, <<0,0,1>>}, extract_nal_erl(<<0,0,0,1,104,206,50,200,0,0,1>>)),
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
  {ok, <<12,255,255,255,255,255,255,255,255,255,255,255,255,255,255>>, <<0,0,1>>} = extract_nal(Bin5).


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




