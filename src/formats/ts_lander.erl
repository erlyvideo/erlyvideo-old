-module(ts_lander).
-export([start_link/1]).
-behaviour(gen_server).

-include("../../include/ems.hrl").

-record(ts_lander, {
  request_id,
  url,
  buffer = <<>>,
  pids
}).

-record(pes, {
  pid,
  synced = false,
  buffer = [],
  counter = 0
}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


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
  io:format("HTTP Request ~p~n", [RequestId]),
  timer:send_after(6*1000, {stop}),
  {ok, #ts_lander{request_id = RequestId, url = URL, pids = dict:new()}}.
    


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
  io:format("MPEG TS headers ~p~n", [_Headers]),
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

synchronizer(TSLander, <<_, Bin/binary>>) when size(Bin) >= 188 ->
  synchronizer(TSLander, Bin);

synchronizer(TSLander, Bin) ->
  TSLander#ts_lander{buffer = Bin}.


demux(#ts_lander{pids = Pids} = TSLander, <<_:1, PayloadStart:1, _:1, Pid:13, _/binary>> = Packet) ->
  % <<_TEI:1, PayloadStart:1, _Priority:1, TsPid:13, _Scrambling:2, _Adaptation:1, _Payload:1, _Counter:4, Payload/binary>> = Packet,
  Pes1 = case dict:find(Pid, Pids) of
    {ok, Pes} -> 
      receive_pes(Pes, Packet);
    error ->
      receive_pes(#pes{pid = Pid}, Packet)
  end,
  TSLander#ts_lander{pids = dict:store(Pid, Pes1, Pids)}.
      


receive_pes(#pes{synced = false} = Pes, <<_:1, Start:1, _:22, _Rest/binary>> = Packet) when Start == 0 ->
  Pes;

receive_pes(#pes{synced = false, pid = Pid} = Pes, <<_:1, Start:1, _:22, _/binary>> = Packet) when Start == 1->
  ?D({"Synced PES", Pid}),
  Pes#pes{synced = true, buffer = [extract_ts_payload(Packet)]};

receive_pes(#pes{synced = true, buffer = Buf} = Pes, <<_:1, Start:1, _:22, _/binary>> = Packet) when Start == 0->
  Pes#pes{synced = true, buffer = [extract_ts_payload(Packet) | Buf]};

receive_pes(#pes{synced = true, buffer = Buf} = Pes, <<_:1, Start:1, _:22, _/binary>> = Packet) when Start == 1 ->
  Pes1 = pes_packet(Pes, list_to_binary(lists:reverse(Buf))),
  Pes1#pes{synced = true, buffer = [extract_ts_payload(Packet)]}.
  
  
extract_ts_payload(<<_TEI:1, _Start:1, _Priority:1, _Pid:13, 
              _Scrambling:2, Adaptation:1, _Payload:1, _Counter:4, Payload/binary>> = Packet) when Adaptation == 0  ->
  Payload;
                
extract_ts_payload(<<_TEI:1, _Start:1, _Priority:1, _Pid:13, 
              _Scrambling:2, _Adaptation:1, _Payload:1, _Counter:4, 
              AdaptationLength, AdaptationField:AdaptationLength/binary, Payload/binary>> = Packet) ->
  Payload.

  

pes_packet(#pes{counter = Counter, pid = Pid} = Pes, <<1:24/integer,
                                            _StreamId:8/integer,
                                            _PesPacketLength:16/integer,
                                            2#10:2,
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
                                            _PESHeaderDataLength:8,
                                            _/binary>> = Packet) ->
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
                    Pes#pes{counter = Counter + 1};
                false ->
                    io:format("!!! DTS ~p, Delta ~p on ~p~n", [DTS, DTS-Counter, Pid]),
                    Pes#pes{counter = Counter + 1}
            end;
            
pes_packet(#pes{pid = Pid} = Pes, _Packet) ->
  io:format("Broken PES packet on pid ~p~n", [Pid]),
  Pes.

% TEST ONLY----
% start(FileName) ->
%     {ok, File} = file:open(FileName, [read,binary,raw]),
%     NalRcv = spawn(?MODULE, nal_receiver, [[]]),
%     feed_bytestream_file(File, NalRcv).

feed_bytestream_file(File, Pid) ->
    case file:read(File, 1024) of
        {ok, Data} ->
            Pid ! {raw, Data},
            feed_bytestream_file(File, Pid);
        eof ->
            file:close(File)
    end.

nal_receiver(Buffer) ->
    receive
        {pes_packet, Packet} ->
            <<1:24, _:24, 2#10:2, _:14, PESHeaderDataLength:8, _/binary>> = Packet,
            {_, Data} = split_binary(Packet, PESHeaderDataLength+9),
            nal_synchronizer(list_to_binary([Buffer, Data]));
        {raw, Data} ->
            nal_synchronizer(list_to_binary([Buffer, Data]))
    end.

nal_synchronizer(Buffer) ->
    Offset1 = nal_unit_start_code_finder(Buffer, 0)+3,
    Offset2 = nal_unit_start_code_finder(Buffer, Offset1+3),
    case Offset2 of
        false -> nal_receiver(Buffer);
        _ ->
            Length = Offset2-Offset1-1,
            <<_:Offset1/binary, NAL:Length/binary, Rest/binary>> = Buffer,
            decode_nal(NAL),
            nal_synchronizer(Rest)
    end.

nal_unit_start_code_finder(Bin, Offset) when Offset < size(Bin) ->
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
