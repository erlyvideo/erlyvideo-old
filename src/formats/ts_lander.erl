-module(ts_lander).
-export([start_link/1]).
-behaviour(gen_server).

-include("../../include/ems.hrl").

-record(ts_lander, {
  request_id,
  url,
  buffer = <<>>,
  pids,
  program_pids = []
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

-export([pat/4, pmt/4, pes/4]).

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


% {ok, Pid1} = ems_sup:start_ts_lander("http://localhost:8080").

start_link(URL) ->
  gen_server:start_link(?MODULE, [URL], []).

init([URL]) ->
  
  % {ok, RequestId} = http:request(get, {URL, []}, [], [{sync, false}, {full_result, false}, {stream_to, self}], erlyvideo),
  {ok, RequestId} = http:request(get, {URL, []}, [], [{sync, false}, {full_result, false}, {stream, self}]),
  % io:format("HTTP Request ~p~n", [RequestId]),
  timer:send_after(16*1000, {stop}),
  {ok, #ts_lander{request_id = RequestId, url = URL, pids = [#stream{pid = 0, handler = pat}]}}.
    


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

handle_info({http, {_RequestId, stream_start, _Headers}}, TSLander) ->
  % io:format("MPEG TS headers ~p~n", [_Headers]),
  {noreply, TSLander};


handle_info({http, {_RequestId, stream, Bin}}, #ts_lander{buffer = Buf} = TSLander) ->
  {noreply, synchronizer(TSLander, <<Buf/binary, Bin/binary>>)};


handle_info({http, {_RequestId, stream_end, _Headers}}, TSLander) ->
  io:format("MPEG TS end ~p~n", [_Headers]),
  {stop, normal, TSLander};


handle_info({stop}, #ts_lander{request_id = RequestId} = TSLander) ->
  http:cancel_request(RequestId),
  {stop, normal, TSLander#ts_lander{request_id = undefined}};

handle_info(_Info, State) ->
  ?D({"Undefined info", _Info}),
  {noreply, State}.


synchronizer(TSLander, <<16#47, Body:187/binary, 16#47, Rest/binary>>) ->
  Lander = demux(TSLander, Body),
  Lander#ts_lander{buffer = <<16#47, Rest/binary>>};

synchronizer(TSLander, <<_, Bin/binary>>) when size(Bin) >= 374 ->
  synchronizer(TSLander, Bin);

synchronizer(TSLander, Bin) ->
  TSLander#ts_lander{buffer = Bin}.


demux(#ts_lander{pids = Pids} = TSLander, <<_:1, PayloadStart:1, _:1, Pid:13, _:4, Counter:4, _/binary>> = Packet) ->
  % <<_TEI:1, PayloadStart:1, _Priority:1, TsPid:13, _Scrambling:2, _Adaptation:1, _Payload:1, _Counter:4, Payload/binary>> = Packet,
  
  case lists:keyfind(Pid, #stream.pid, Pids) of
    #stream{handler = Handler, counter = OldCounter} = Stream ->
      % io:format("Pid: ~p, handler ~p~n", [Pid, Handler, Counter]),
      % ?D({Handler, Pid, PayloadStart, (OldCounter + 1) rem 15, Counter, Packet, extract_ts_payload(Packet)}),
      ?MODULE:Handler(TSLander, Stream, PayloadStart, extract_ts_payload(Packet));
      % TSLander1#ts_lander{pids = lists:keyreplace(Pid, #stream.pid, Pids, Stream1)};
    false ->
      io:format("Unknown pid ~p~n", [Pid]),
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
  % io:format("PAT: ~p programs~n~n", [ProgramCount]),
  Descriptors = extract_pat(ProgramCount, PAT, []),
  TSLander#ts_lander{pids = lists:keymerge(#stream.pid, Pids, Descriptors)}.


extract_pat(0, <<_CRC32:4/binary>>, Descriptors) ->
  lists:keysort(#stream.pid, Descriptors);
extract_pat(ProgramCount, <<ProgramNum:16, _:3, Pid:13, PAT/binary>>, Descriptors) ->
  % io:format("Program ~p on pid ~p~n", [ProgramNum, Pid]),
  extract_pat(ProgramCount - 1, PAT, [#stream{handler = pmt, pid = Pid, counter = 0, program_num = ProgramNum} | Descriptors]).




pmt(#ts_lander{pids = Pids} = TSLander, Stream, _, 
                           <<_Pointer, 2, _SectionInd:1, 0:1, 2#11:2, SectionLength:12, 
                             ProgramNum:16, _:2, _Version:5, _CurrentNext:1, _SectionNumber,
                             _LastSectionNumber, _:3, PCRPID:13, _:4, ProgramInfoLength:12, 
                             ProgramInfo:ProgramInfoLength/binary, PMT/binary>> = _PMT) ->
  SectionCount = round(SectionLength - 13),
  io:format("Program ~p v~p. PCR: ~p~n", [ProgramNum, _Version, PCRPID]),
  Descriptors1 = lists:map(fun(Stream) -> Stream#stream{program_num = ProgramNum, type = video} end, extract_pmt(PMT, [])),
  % Descriptors1 = set_audio_stream_from_pcr(Descriptors, PCRPID),
  io:format("Streams: ~p~n", [Descriptors1]),
  TSLander#ts_lander{pids = lists:keymerge(#stream.pid, Pids, Descriptors1)}.

extract_pmt(<<StreamType, 2#111:3, Pid:13, _:4, ESLength:12, ES:ESLength/binary, Rest/binary>>, Descriptors) ->
  extract_pmt(Rest, [#stream{handler = pes, counter = 0, pid = Pid, type = StreamType}|Descriptors]);
  
extract_pmt(<<_CRC32:4/binary>>, Descriptors) ->
  % io:format("Unknown PMT: ~p~n", [PMT]),
  lists:keysort(#stream.pid, Descriptors).

set_audio_stream_from_pcr(Descriptors, PCRPID) ->
  Stream = lists:keyfind(PCRPID, #stream.pid, Descriptors),
  lists:keyreplace(PCRPID, #stream.pid, Descriptors, Stream#stream{type = audio}).


pes(TSLander, #stream{synced = false} = Pes, 0, Packet) ->
  ?D("No sync pes"),
  TSLander;

pes(#ts_lander{pids = Pids} = TSLander, #stream{synced = false, pid = Pid} = Stream, 1, Packet)->
  ?D({"Synced PES", Pid}),
  Stream1 = Stream#stream{synced = true, ts_buffer = [Packet]},
  TSLander#ts_lander{pids = lists:keyreplace(Pid, #stream.pid, Pids, Stream1)};

pes(#ts_lander{pids = Pids} = TSLander, #stream{synced = true, ts_buffer = Buf, pid = Pid} = Stream, 0, Packet)->
  Stream1 = Stream#stream{synced = true, ts_buffer = [Packet | Buf]},
  TSLander#ts_lander{pids = lists:keyreplace(Pid, #stream.pid, Pids, Stream1)};

pes(#ts_lander{pids = Pids} = TSLander, #stream{synced = true, ts_buffer = Buf, pid = Pid} = Stream, 1, Packet) ->
  PES = list_to_binary(lists:reverse(Buf)),
  % ?D({"Decode PES", Pid, Stream#stream.es_buffer, PES}),
  {TSLander1, Stream1} = pes_packet(TSLander, Stream, PES),
  Stream2 = Stream1#stream{synced = true, ts_buffer = [Packet]},
  TSLander#ts_lander{pids = lists:keyreplace(Pid, #stream.pid, TSLander1#ts_lander.pids, Stream2)}.




pes_packet(TSLander, #stream{counter = Counter, pid = Pid} = Pes, 
                                            <<1:24,
                                            2#110:3, _StreamId:5,
                                            % _StreamId:8,
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
            io:format("Pid ~p, Stream ~p~n", [Pid, _StreamId]),
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
                    io:format("New DTS ~p, Delta ~p on ~p~n", [DTS, DTS-Counter, Pid]),
                    {TSLander, Pes#stream{counter = Counter + 1, type = audio}};
                false ->
                    io:format("!!! DTS ~p, Delta ~p on ~p~n", [DTS, DTS-Counter, Pid]),
                    {TSLander, Pes#stream{counter = Counter + 1, type = audio}}
            end;

pes_packet(TSLander, #stream{es_buffer = Buffer, pid = Pid} = Stream, 
                                                  <<_:3/binary, 
                                                  2#1110:4, StreamId:4,
                                                  % _:4/binary,
                                                  % PESHeaderLength:8,
                                                  % PESHeader:PESHeaderLength/binary,
                                                  Rest/binary>> = Packet) ->
  Data = <<Buffer/binary, Rest/binary>>,
  % io:format("PES ~p ~p ~p ~p, ~p, ~p~n", [StreamId, _DataAlignmentIndicator, _PesPacketLength, PESHeaderLength, PESHeader, Rest]),
  % io:format("PES ~p ~p ~p ~p, ~p, ~p~n", [StreamId, _DataAlignmentIndicator, _PesPacketLength, PESHeaderLength, PESHeader, Rest]),
  ?D({"In pes", Pid, StreamId}),
  Offset1 = nal_unit_start_code_finder(Data, 0) + 3,
  Offset2 = nal_unit_start_code_finder(Data, Offset1+3),
  case Offset2 of
      false -> {TSLander, Stream#stream{es_buffer = Data}};
      _ ->
          Length = Offset2-Offset1-1,
          <<_:Offset1/binary, NAL:Length/binary, Rest1/binary>> = Data,
          decode_nal(NAL),
          % pes_packet(TSLander, Stream#stream{es_buffer = <<>>}, Rest1)
          {TSLander, Stream#stream{es_buffer = Rest1}}
  end.

nal_unit_start_code_finder(Bin, Offset) when Offset + 3 =< size(Bin) ->
    case Bin of
        <<_:Offset/binary, 16#000001:24, _/binary>> ->
            Offset;
        _ ->
            nal_unit_start_code_finder(Bin, Offset + 1)
    end;

nal_unit_start_code_finder(_, _) -> false.

decode_nal(Bin) ->
    <<0:1, NalRefIdc:2, NalUnitType:5, Rest/binary>> = Bin,
    case NalUnitType of
        7 ->
            <<ProfileId:8, _:3, 0:5, Level:8, AfterLevel/binary>> = Rest,
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
            io:format("~nSequence parameter set ~p ~p~n", [Profile, Level/10]),
            io:format("seq_parameter_set_id: ~p~n", [SeqParameterSetId]),
            io:format("log2_max_frame_num_minus4: ~p~n", [Log2MaxFrameNumMinus4]);
        8 ->
            io:format("Picture parameter set [~p]~n", [size(Bin)]);
        1 ->
            %io:format("Coded slice of a non-IDR picture :: "),
            slice_header(Rest, NalRefIdc);
        2 ->
            %io:format("Coded slice data partition A     :: "),
            slice_header(Rest, NalRefIdc);
        5 ->
            io:format("~nCoded slice of an IDR picture~n"),
            slice_header(Rest, NalRefIdc);
        9 ->
            <<PrimaryPicTypeId:3, _:5, _/binary>> = Rest,
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
            io:format("Access unit delimiter, PPT = ~p~n", [PrimaryPicType]);
        _ ->
            io:format("Unknown NAL unit type ~p~n", [NalUnitType])
    end.

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
    io:format("~s~p:~p:~p:~p:~p ", [SliceType, FrameNum, PicParameterSetId, FieldPicFlag, BottomFieldFlag, NalRefIdc]).

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
