-module(ts_lander).
-export([start_link/1]).
-behaviour(gen_server).

-include("../../include/ems.hrl").

% ems_sup:start_ts_lander("http://localhost:8080").


-record(ts_lander, {
  socket,
  url,
  buffer = <<>>,
  pids
}).

-record(stream_out, {
  pid,
  handler
}).

-record(stream, {
  pid,
  program_num,
  handler,
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
  gen_server:start_link(?MODULE, [URL], []).

init([URL]) ->
  {_, _, Host, Port, Path, Query} = http_uri:parse(URL),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, http_bin}, {active, false}], 1000),
  gen_tcp:send(Socket, "GET "++Path++"?"++Query++" HTTP/1.0\r\n\r\n"),
  ok = inet:setopts(Socket, [{active, once}]),
  
  % timer:send_after(6*1000, {stop}),
  
  {ok, #ts_lander{socket = Socket, url = URL, pids = [#stream{pid = 0, handler = pat}]}}.
  
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


handle_info({tcp, _Socket, Bin}, #ts_lander{buffer = <<>>} = TSLander) ->
  {noreply, synchronizer(TSLander, Bin)};

handle_info({tcp, _Socket, Bin}, #ts_lander{buffer = Buf} = TSLander) ->
  {noreply, synchronizer(TSLander, <<Buf/binary, Bin/binary>>)};

handle_info({tcp_closed, Socket}, #ts_lander{socket = Socket} = TSLander) ->
  {stop, normal, TSLander#ts_lander{socket = undefined}};

handle_info({stop}, #ts_lander{socket = Socket} = TSLander) ->
  gen_tcp:close(Socket),
  {stop, normal, TSLander#ts_lander{socket = undefined}};

handle_info(_Info, State) ->
  ?D({"Undefined info", _Info}),
  {noreply, State}.


synchronizer(TSLander, <<16#47, Body:187/binary, 16#47, Rest/binary>>) ->
  Lander = demux(TSLander, Body),
  synchronizer(Lander, <<16#47, Rest/binary>>);

synchronizer(TSLander, <<_, Bin/binary>>) when size(Bin) >= 374 ->
  synchronizer(TSLander, Bin);

synchronizer(TSLander, Bin) ->
  TSLander#ts_lander{buffer = Bin}.


demux(#ts_lander{pids = Pids} = TSLander, <<_:1, PayloadStart:1, _:1, Pid:13, _:4, Counter:4, _/binary>> = Packet) ->
  case lists:keyfind(Pid, #stream.pid, Pids) of
    #stream{handler = Handler, counter = OldCounter} = Stream ->
      % Counter = (OldCounter + 1) rem 15,
      ?MODULE:Handler(TSLander, Stream#stream{counter = Counter}, PayloadStart, extract_ts_payload(Packet));
    #stream_out{handler = Handler} ->
      Handler ! {ts_packet, PayloadStart, extract_ts_payload(Packet)},
      TSLander;
    false ->
      % io:format("Unknown pid ~p~n", [Pid]),
      TSLander
  end.
  
      

extract_ts_payload(<<_TEI:1, _Start:1, _Priority:1, _Pid:13, _Scrambling:2, 
              0:1, 1:1, _Counter:4, Payload/binary>> = Packet)  ->
  Payload;

extract_ts_payload(<<_TEI:1, _Start:1, _Priority:1, _Pid:13, 
              _Scrambling:2, 1:1, 1:1, _Counter:4, 
              AdaptationLength, AdaptationField:AdaptationLength/binary, Payload/binary>> = Packet) ->
  % io:format("~p bytes of adapt field pid ~p~n", [AdaptationLength, _Pid]),
  % ?D({_Pid, Packet, Payload}),
  Payload;

extract_ts_payload(<<_TEI:1, _Start:1, _Priority:1, _Pid:13, _Scrambling:2, 
              _Adaptation:1, 0:1, _Counter:4, _Payload/binary>> = Packet)  ->
  io:format("Empty payload on pid ~p~n", [_Pid]),
  <<>>.


%%%%%%%%%%%%%%%   Program access table  %%%%%%%%%%%%%%

pat(#ts_lander{pids = Pids} = TSLander, Stream, _, <<_PtField, 0, 2#10:2, 2#11:2, Length:12, _Misc:5/binary, PAT/binary>>) -> % PAT
  ProgramCount = round((Length - 5)/4) - 1,
  % io:format("PAT: ~p programs (~p)~n", [ProgramCount, size(PAT)]),
  Descriptors = extract_pat(PAT, ProgramCount, []),
  TSLander#ts_lander{pids = lists:keymerge(#stream.pid, Pids, Descriptors)}.


extract_pat(_CRC32, 0, Descriptors) ->
  lists:keysort(#stream.pid, Descriptors);
extract_pat(<<ProgramNum:16, _:3, Pid:13, PAT/binary>>, ProgramCount, Descriptors) ->
  % io:format("Program ~p on pid ~p~n", [ProgramNum, Pid]),
  extract_pat(PAT, ProgramCount - 1, [#stream{handler = pmt, pid = Pid, counter = 0, program_num = ProgramNum} | Descriptors]).




pmt(#ts_lander{pids = Pids} = TSLander, Stream, _, 
                           <<_Pointer, 2, _SectionInd:1, 0:1, 2#11:2, SectionLength:12, 
                             ProgramNum:16, _:2, _Version:5, _CurrentNext:1, _SectionNumber,
                             _LastSectionNumber, _:3, PCRPID:13, _:4, ProgramInfoLength:12, 
                             ProgramInfo:ProgramInfoLength/binary, PMT/binary>>) ->
  SectionCount = round(SectionLength - 13),
  % io:format("Program ~p v~p. PCR: ~p~n", [ProgramNum, _Version, PCRPID]),
  Descriptors = extract_pmt(PMT, []),
  io:format("Streams: ~p~n", [Descriptors]),
  Descriptors1 = lists:map(fun(#stream{pid = Pid} = Stream) ->
    case lists:keyfind(Pid, #stream.pid, Pids) of
      false ->
        Handler = spawn_link(?MODULE, pes, [Stream#stream{program_num = ProgramNum, type = video}]),
        ?D({"Starting", Handler}),
        #stream_out{pid = Pid, handler = Handler};
      Other ->
        Other
    end
  end, Descriptors),
  % TSLander#ts_lander{pids = lists:keymerge(#stream.pid, Pids, Descriptors1)}.
  TSLander#ts_lander{pids = Descriptors1}.

extract_pmt(<<StreamType, 2#111:3, Pid:13, _:4, ESLength:12, ES:ESLength/binary, Rest/binary>>, Descriptors) ->
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
      Stream1 = pes_packet(Stream, PES),
      Stream2 = Stream1#stream{ts_buffer = [Packet]},
      ?MODULE:pes(Stream2);
    Other ->
      ?D({"Undefined message to pid", Pid, Other})
  end.
    
      


pes_packet(#stream{counter = Counter, pid = Pid} = Pes, 
                                            <<1:24,
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
                                            PESHeader:PESHeaderLength/binary,
                                            Data/binary>> = Packet) ->
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

pes_packet(#stream{es_buffer = Buffer, pid = Pid} = Stream, 
                                                  <<1:24, 
                                                  StreamId,
                                                  _:4/binary,
                                                  PESHeaderLength:8,
                                                  PESHeader:PESHeaderLength/binary,
                                                  Rest/binary>> = Packet) ->
  Data = <<Buffer/binary, Rest/binary>>,
  % io:format("PES ~p ~p ~p ~p, ~p, ~p~n", [StreamId, _DataAlignmentIndicator, _PesPacketLength, PESHeaderLength, PESHeader, Rest]),
  % io:format("PES ~p ~p ~p ~p, ~p, ~p~n", [StreamId, _DataAlignmentIndicator, _PesPacketLength, PESHeaderLength, PESHeader, Rest]),
  Offset1 = nal_unit_start_code_finder(Data, 0) + 3,
  Offset2 = nal_unit_start_code_finder(Data, Offset1+3),
  case Offset2 of
      false -> Stream#stream{es_buffer = Data};
      _ ->
          Length = Offset2-Offset1-1,
          <<_:Offset1/binary, NAL:Length/binary, Rest1/binary>> = Data,
          decode_nal(NAL),
          % pes_packet(TSLander, Stream#stream{es_buffer = <<>>}, Rest1)
          Stream#stream{es_buffer = Rest1}
  end.

nal_unit_start_code_finder(Bin, Offset) when Offset + 3 =< size(Bin) ->
    case Bin of
        <<_:Offset/binary, 16#000001:24, _/binary>> ->
            Offset;
        _ ->
            nal_unit_start_code_finder(Bin, Offset + 1)
    end;

nal_unit_start_code_finder(_, _) -> false.


decode_nal(<<0:1, NalRefIdc:2, 1:5, Rest/binary>>) ->
  % io:format("Coded slice of a non-IDR picture :: "),
  slice_header(Rest, NalRefIdc);

decode_nal(<<0:1, NalRefIdc:2, 2:5, Rest/binary>>) ->
  % io:format("Coded slice data partition A     :: "),
  slice_header(Rest, NalRefIdc);

decode_nal(<<0:1, NalRefIdc:2, 5:5, Rest/binary>>) ->
  % io:format("~nCoded slice of an IDR picture~n"),
  slice_header(Rest, NalRefIdc);

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


decode_nal(<<0:1, NalRefIdc:2, 7:5, ProfileId:8, _:3, 0:5, Level:8, AfterLevel/binary>>) ->
  {SeqParameterSetId, AfterSPSId} = exp_golomb_read(AfterLevel),
  {Log2MaxFrameNumMinus4, _} = exp_golomb_read(AfterSPSId),
  Profile = case ProfileId of
      66 -> "Baseline";
      77 -> "Main";
      88 -> "Extended";
      100 -> "High";
      110 -> "High 10";
      122 -> "High 4:2:2";
      144 -> "High 4:4:4";
      _ -> "Uknkown "++integer_to_list(ProfileId)
  end,
  % io:format("~nSequence parameter set ~p ~p~n", [Profile, Level/10]),
  % io:format("seq_parameter_set_id: ~p~n", [SeqParameterSetId]),
  % io:format("log2_max_frame_num_minus4: ~p~n", [Log2MaxFrameNumMinus4]),
  ok;


decode_nal(<<0:1, _NalRefIdc:2, NalUnitType:5, _/binary>>) ->
  % io:format("Unknown NAL unit type ~p~n", [NalUnitType]),
  ok.

slice_header(Bin, NalRefIdc) ->
    {_FirstMbInSlice, Rest} = exp_golomb_read(Bin),
    {SliceTypeId, Rest2 } = exp_golomb_read(Rest),
    {PicParameterSetId, Rest3 } = exp_golomb_read(Rest2),
    <<FrameNum:5, FieldPicFlag:1, BottomFieldFlag:1, _/bitstring>> = Rest3,
    SliceType = case SliceTypeId of
        0 -> "P";
        1 -> "B";
        2 -> "I";
        3 -> "p";
        4 -> "i";
        5 -> "P";
        6 -> "B";
        7 -> "I";
        8 -> "p";
        9 -> "i"
    end,
    % io:format("~s~p:~p:~p:~p:~p ~n", [SliceType, FrameNum, PicParameterSetId, FieldPicFlag, BottomFieldFlag, NalRefIdc]),
    ok.

exp_golomb_read(Bin) ->
    LeadingZeros = count_zeros(Bin,0),
    <<0:LeadingZeros, 1:1, ReadBits:LeadingZeros, Rest/bitstring>> = Bin,
    CodeNum = (1 bsl LeadingZeros) -1 + ReadBits,
    {CodeNum, Rest}.

count_zeros(Bin, Offset) ->
    case Bin of
        <<_:Offset, 1:1, _/bitstring>> -> Offset;
        <<_:Offset, 0:1, _/bitstring>> -> count_zeros(Bin, Offset+1)
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
