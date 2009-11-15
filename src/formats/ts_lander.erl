-module(ts_lander).
-export([start_link/1, start_link/2]).
-behaviour(gen_server).

-include("../../include/ems.hrl").

% ems_sup:start_ts_lander("http://localhost:8080").


-record(ts_lander, {
  socket,
  url,
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
  counter = 0
}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([pat/4, pmt/4, pes/1]).

% start() ->
%     Pid = start(?HOST, ?PORT, ?PATH),
%     %VideoPesExPid = start_pes_extractor(start_pes_writer("video.264")),
%     VideoPesExPid = start_pes_extractor(spawn(?MODULE, nal_receiver, [[]])),
%     %AudioPesExPid = start_pes_extractor(start_pes_writer("audio.aac")),
%     Pid ! {demuxer, {subscribe, 69, VideoPesExPid}},
%     %Pid ! {demuxer, {subscribe, 68, AudioPesExPid}},
%     %DtsCounterPid = start_dts_counter(),
%     %VideoPesExPid = start_pes_extractor(DtsCounterPid),
%     %AudioPesExPid = start_pes_extractor(DtsCounterPid),
%     %Pid ! {demuxer, {subscribe, video, fun(P) -> P == 101 end, VideoPesExPid}},
%     %Pid ! {demuxer, {subscribe, audio, fun(P) -> P == 100 end, AudioPesExPid}},
% 
%     %SubtitlesPesExPid = start_pes_extractor(start_pes_writer("subs.bin")),
%     %Pid ! {demuxer, {subscribe, 66, SubtitlesPesExPid}},
% 
%     Pid.


% {ok, Socket} = gen_tcp:connect("ya.ru", 80, [binary, {packet, http_bin}, {active, false}], 1000),
% gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
% {ok, Reply} = gen_tcp:recv(Socket, 0, 1000),
% Reply.

% {ok, Pid1} = ems_sup:start_ts_lander("http://localhost:8080").

start_link(URL) ->
  gen_server:start_link(?MODULE, [URL, self()], []).

start_link(URL, Consumer) ->
  gen_server:start_link(?MODULE, [URL, Consumer], []).


init([URL, Consumer]) ->
  {_, _, Host, Port, Path, Query} = http_uri:parse(URL),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, http_bin}, {active, false}], 1000),
  gen_tcp:send(Socket, "GET "++Path++"?"++Query++" HTTP/1.0\r\n\r\n"),
  ok = inet:setopts(Socket, [{active, once}]),
  
  % timer:send_after(6*1000, {stop}),
  timer:send_after(3000, {byte_count}),
  
  {ok, #ts_lander{socket = Socket, url = URL, consumer = Consumer, pids = [#stream{pid = 0, handler = pat}]}}.
  
  % io:format("HTTP Request ~p~n", [RequestId]),
  % {ok, #ts_lander{request_id = RequestId, url = URL, pids = [#stream{pid = 0, handler = pat}]}}.
    


%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call(Request, _From, State) ->
  ?D({"Undefined call", Request, _From}),
  {stop, {unknown_call, Request}, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
  ?D({"Undefined cast", _Msg}),
  {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------

handle_info({http, Socket, {http_response, _Version, 200, _Reply}}, TSLander) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, TSLander};

handle_info({http, Socket, {http_header, _, _Header, _, _Value}}, TSLander) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, TSLander};


handle_info({http, Socket, http_eoh}, TSLander) ->
  inet:setopts(Socket, [{active, true}, {packet, raw}]),
  {noreply, TSLander};


handle_info({tcp, _Socket, Bin}, #ts_lander{buffer = <<>>, byte_counter = Counter} = TSLander) ->
  {noreply, synchronizer(Bin, TSLander#ts_lander{byte_counter = Counter + size(Bin)})};

handle_info({tcp, _Socket, Bin}, #ts_lander{buffer = Buf, byte_counter = Counter} = TSLander) ->
  {noreply, synchronizer(<<Buf/binary, Bin/binary>>, TSLander#ts_lander{byte_counter = Counter + size(Bin)})};

handle_info({tcp_closed, Socket}, #ts_lander{socket = Socket} = TSLander) ->
  {stop, normal, TSLander#ts_lander{socket = undefined}};
  
handle_info({byte_count}, #ts_lander{byte_counter = Counter} = TSLander) ->
  ?D({"Bytes read", Counter}),
  timer:send_after(3000, {byte_count}),
  {noreply, TSLander};

handle_info({stop}, #ts_lander{socket = Socket} = TSLander) ->
  gen_tcp:close(Socket),
  {stop, normal, TSLander#ts_lander{socket = undefined}};

handle_info(_Info, State) ->
  ?D({"Undefined info", _Info}),
  {noreply, State}.


synchronizer(<<16#47, _:187/binary, 16#47, _/binary>> = Bin, TSLander) ->
  {Packet, Rest} = split_binary(Bin, 188),
  Lander = demux(TSLander, Packet),
  synchronizer(Rest, Lander);

synchronizer(<<_, Bin/binary>>, TSLander) when size(Bin) >= 374 ->
  synchronizer(Bin, TSLander);

synchronizer(Bin, TSLander) ->
  TSLander#ts_lander{buffer = Bin}.


demux(#ts_lander{pids = Pids} = TSLander, <<16#47, _:1, PayloadStart:1, _:1, Pid:13, _:4, Counter:4, _/binary>> = Packet) ->
  case lists:keyfind(Pid, #stream.pid, Pids) of
    #stream{handler = Handler, counter = _OldCounter} = Stream ->
      % Counter = (OldCounter + 1) rem 15,
      ?MODULE:Handler(extract_ts_payload(Packet), TSLander, Stream#stream{counter = Counter}, PayloadStart);
    #stream_out{handler = Handler} ->
      Handler ! {ts_packet, PayloadStart, extract_ts_payload(Packet)},
      TSLander;
    false ->
      TSLander
  end.
  
      

extract_ts_payload(<<16#47, _TEI:1, _Start:1, _Priority:1, _Pid:13, _Scrambling:2, 
              0:1, 1:1, _Counter:4, Payload/binary>>)  ->
  Payload;

extract_ts_payload(<<16#47, _TEI:1, _Start:1, _Priority:1, _Pid:13, 
              _Scrambling:2, 1:1, 1:1, _Counter:4, 
              AdaptationLength, _AdaptationField:AdaptationLength/binary, Payload/binary>>) ->
  % io:format("~p bytes of adapt field pid ~p~n", [AdaptationLength, _Pid]),
  % ?D({_Pid, Packet, Payload}),
  Payload;

extract_ts_payload(<<16#47, _TEI:1, _Start:1, _Priority:1, _Pid:13, _Scrambling:2, 
              _Adaptation:1, 0:1, _Counter:4, _Payload/binary>>)  ->
  io:format("Empty payload on pid ~p~n", [_Pid]),
  <<>>.


%%%%%%%%%%%%%%%   Program access table  %%%%%%%%%%%%%%

pat(<<_PtField, 0, 2#10:2, 2#11:2, Length:12, _Misc:5/binary, PAT/binary>>, #ts_lander{pids = Pids} = TSLander, _, _) -> % PAT
  ProgramCount = round((Length - 5)/4) - 1,
  % io:format("PAT: ~p programs (~p)~n", [ProgramCount, size(PAT)]),
  Descriptors = extract_pat(PAT, ProgramCount, []),
  TSLander#ts_lander{pids = lists:keymerge(#stream.pid, Pids, Descriptors)}.


extract_pat(<<_CRC32/binary>>, 0, Descriptors) ->
  lists:keysort(#stream.pid, Descriptors);
extract_pat(<<ProgramNum:16, _:3, Pid:13, PAT/binary>>, ProgramCount, Descriptors) ->
  % io:format("Program ~p on pid ~p~n", [ProgramNum, Pid]),
  extract_pat(PAT, ProgramCount - 1, [#stream{handler = pmt, pid = Pid, counter = 0, program_num = ProgramNum} | Descriptors]).




pmt(<<_Pointer, 2, _SectionInd:1, 0:1, 2#11:2, SectionLength:12, 
    ProgramNum:16, _:2, _Version:5, _CurrentNext:1, _SectionNumber,
    _LastSectionNumber, _:3, _PCRPID:13, _:4, ProgramInfoLength:12, 
    _ProgramInfo:ProgramInfoLength/binary, PMT/binary>>, #ts_lander{pids = Pids, consumer = Consumer} = TSLander, _, _) ->
  _SectionCount = round(SectionLength - 13),
  % io:format("Program ~p v~p. PCR: ~p~n", [ProgramNum, _Version, PCRPID]),
  Descriptors = extract_pmt(PMT, []),
  io:format("Streams: ~p~n", [Descriptors]),
  Descriptors1 = lists:map(fun(#stream{pid = Pid} = Stream) ->
    case lists:keyfind(Pid, #stream.pid, Pids) of
      false ->
        Handler = spawn_link(?MODULE, pes, [Stream#stream{program_num = ProgramNum, type = video, consumer = Consumer}]),
        ?D({"Starting", Handler}),
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

extract_pmt(<<StreamType, 2#111:3, Pid:13, _:4, ESLength:12, _ES:ESLength/binary, Rest/binary>>, Descriptors) ->
  extract_pmt(Rest, [#stream{handler = pes, counter = 0, pid = Pid, type = StreamType}|Descriptors]);
  
extract_pmt(_CRC32, Descriptors) ->
  % io:format("Unknown PMT: ~p~n", [PMT]),
  lists:keysort(#stream.pid, Descriptors).


pes(#stream{synced = false, pid = Pid} = Stream) ->
  receive
    {ts_packet, 0, _} ->
      ?D({"Not synced pes", Pid}),
      ?MODULE:pes(Stream);
    {ts_packet, 1, Packet} ->
      ?D({"Synced PES", Pid}),
      Stream1 = Stream#stream{synced = true, ts_buffer = [Packet]},
      ?MODULE:pes(Stream1);
    Other ->
      ?D({"Undefined message to pid", Pid, Other})
  end;
  
pes(#stream{synced = true, pid = Pid, ts_buffer = Buf} = Stream) ->
  receive
    {ts_packet, 0, Packet} ->
      Stream1 = Stream#stream{synced = true, ts_buffer = [Packet | Buf]},
      ?MODULE:pes(Stream1);
    {ts_packet, 1, Packet} ->
      PES = list_to_binary(lists:reverse(Buf)),
      % ?D({"Decode PES", Pid, Stream#stream.es_buffer, PES}),
      Stream1 = pes_packet(PES, Stream),
      Stream2 = Stream1#stream{ts_buffer = [Packet]},
      ?MODULE:pes(Stream2);
    Other ->
      ?D({"Undefined message to pid", Pid, Other})
  end.
    
      


pes_packet(<<1:24,
            2#110:3, _StreamId:5,
            _PesPacketLength:16,
            2:2,
            _PESScramblingControl:2,
            _PESPriority:1,
            _DataAlignmentIndicator:1,
            _Copyright:1,
            _OriginalOrCopy:1,
            PTS_DTS_flags:2,
            _ESCRFlag:1,
            _ESRateFlag:1,
            _DSMTrickModeFlag:1,
            _AdditionalCopyInfoFlag:1,
            _PESCRCFlag:1,
            _PESExtensionFlag:1,
            PESHeaderLength:8,
            _PESHeader:PESHeaderLength/binary,
            _Data/binary>> = Packet, #stream{counter = Counter, pid = Pid} = Pes) ->
            % io:format("Pid ~p, Stream ~p~n", [Pid, _StreamId]),
            DTS = case PTS_DTS_flags of
                2#11 ->
                    <<_:9/binary, 3:4/integer, _:36/integer, 1:4/integer, Dts3:3/integer, 1:1/integer, Dts2:15/integer, 1:1/integer, Dts1:15/integer, 1:1/integer, _/binary>> = Packet,
                    Dts1 + (Dts2 bsl 15) + (Dts3 bsl 30);
                2#10 ->
                    <<_:9/binary, 2:4/integer, Pts3:3/integer, 1:1/integer, Pts2:15/integer, 1:1/integer, Pts1:15/integer, 1:1/integer, _/binary>> = Packet,
                    Pts1 + (Pts2 bsl 15) + (Pts3 bsl 30);
                _ ->
                    %io:format("No DTS found~n"),
                    Counter + 1
            end,
            case DTS > Counter of
                true ->
                    % io:format("New DTS ~p, Delta ~p on ~p~n", [DTS, DTS-Counter, Pid]),
                    Pes#stream{counter = Counter + 1, type = audio};
                false ->
                    io:format("!!! DTS ~p, Delta ~p on ~p~n", [DTS, DTS-Counter, Pid]),
                    Pes#stream{counter = Counter + 1, type = audio}
            end;

pes_packet(<<1:24, 
            _StreamId,
            _:4/binary,
            PESHeaderLength:8,
            _PESHeader:PESHeaderLength/binary,
            Rest/binary>>, #stream{es_buffer = Buffer} = Stream) ->
              
  decode_avc(Stream#stream{es_buffer = <<Buffer/binary, Rest/binary>>}).


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
  
extract_nal(#stream{es_buffer = Data, consumer = Consumer} = Stream, Offset1, Offset2) ->
  Length = Offset2-Offset1,
  <<_:Offset1/binary, NAL:Length/binary, Rest1/binary>> = Data,
  % <<Begin:40/binary, _/binary>> = NAL,
  % ?D({"AVC", Begin}),
  % decode_nal(NAL),
  ?D("Frame"),
  gen_fsm:send_event(Consumer, {video, NAL}),
  % pes_packet(TSLander, Stream#stream{es_buffer = <<>>}, Rest1)
  decode_avc(Stream#stream{es_buffer = Rest1}).



nal_unit_start_code_finder(Bin, Offset) ->
  case Bin of
    <<_:Offset/binary, Rest/binary>> -> find_nal_start_code(Rest, Offset);
    _ -> false
  end.

find_nal_start_code(<<16#000001:24, _/binary>>, Offset) -> Offset;
find_nal_start_code(<<_, Rest/binary>>, Offset) -> find_nal_start_code(Rest, Offset+1);
find_nal_start_code(<<>>, _) -> false.

decode_nal(<<0:1, _NalRefIdc:2, 1:5, Rest/binary>>) ->
  % io:format("Coded slice of a non-IDR picture :: "),
  slice_header(Rest);

decode_nal(<<0:1, _NalRefIdc:2, 2:5, Rest/binary>>) ->
  % io:format("Coded slice data partition A     :: "),
  slice_header(Rest);

decode_nal(<<0:1, _NalRefIdc:2, 5:5, Rest/binary>>) ->
  % io:format("~nCoded slice of an IDR picture~n"),
  slice_header(Rest);

decode_nal(<<0:1, _NalRefIdc:2, 8:5, _/binary>>) ->
  % io:format("Picture parameter set~n"),
  ok;

decode_nal(<<0:1, _NalRefIdc:2, 9:5, PrimaryPicTypeId:3, _:5, _/binary>>) ->
  PrimaryPicType = case PrimaryPicTypeId of
      0 -> "I";
      1 -> "I, P";
      2 -> "I, P, B";
      3 -> "SI";
      4 -> "SI, SP";
      5 -> "I, SI";
      6 -> "I, SI, P, SP";
      7 -> "I, SI, P, SP, B"
  end,
  io:format("Access unit delimiter, PPT = ~p~n", [PrimaryPicType]),
  ok;


decode_nal(<<0:1, _NalRefIdc:2, 7:5, ProfileId:8, _:3, 0:5, _Level:8, AfterLevel/binary>>) ->
  {_SeqParameterSetId, AfterSPSId} = exp_golomb_read(AfterLevel),
  {_Log2MaxFrameNumMinus4, _} = exp_golomb_read(AfterSPSId),
  _Profile = profile_name(ProfileId),
  % io:format("~nSequence parameter set ~p ~p~n", [Profile, Level/10]),
  % io:format("seq_parameter_set_id: ~p~n", [SeqParameterSetId]),
  % io:format("log2_max_frame_num_minus4: ~p~n", [Log2MaxFrameNumMinus4]),
  ok;


decode_nal(<<0:1, _NalRefIdc:2, _NalUnitType:5, _/binary>>) ->
  % io:format("Unknown NAL unit type ~p~n", [NalUnitType]),
  ok.

profile_name(66) -> "Baseline";
profile_name(77) -> "Main";
profile_name(88) -> "Extended";
profile_name(100) -> "High";
profile_name(110) -> "High 10";
profile_name(122) -> "High 4:2:2";
profile_name(144) -> "High 4:4:4";
profile_name(Profile) -> "Unknown "++integer_to_list(Profile).


slice_header(Bin) ->
    {_FirstMbInSlice, Rest} = exp_golomb_read(Bin),
    {SliceTypeId, Rest2 } = exp_golomb_read(Rest),
    {_PicParameterSetId, Rest3 } = exp_golomb_read(Rest2),
    <<_FrameNum:5, _FieldPicFlag:1, _BottomFieldFlag:1, _/bitstring>> = Rest3,
    _SliceType = slice_type(SliceTypeId),
    % io:format("~s~p:~p:~p:~p ~n", [_SliceType, _FrameNum, _PicParameterSetId, _FieldPicFlag, _BottomFieldFlag]),
    ok.

slice_type(0) -> 'P';
slice_type(1) -> 'B';
slice_type(2) -> 'I';
slice_type(3) -> 'p';
slice_type(4) -> 'i';
slice_type(5) -> 'P';
slice_type(6) -> 'B';
slice_type(7) -> 'I';
slice_type(8) -> 'p';
slice_type(9) -> 'i'.


exp_golomb_read(Bin) ->
    LeadingZeros = count_zeros(Bin,0),
    <<0:LeadingZeros, 1:1, ReadBits:LeadingZeros, Rest/bitstring>> = Bin,
    CodeNum = (1 bsl LeadingZeros) -1 + ReadBits,
    {CodeNum, Rest}.

count_zeros(Bin, Offset) ->
    case Bin of
        <<1:1, _/bitstring>> -> Offset;
        <<0:1, Rest/bitstring>> -> count_zeros(Rest, Offset+1)
    end.


%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _TSLander) ->
  ?D({"TS Lander terminating", _Reason}),
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
