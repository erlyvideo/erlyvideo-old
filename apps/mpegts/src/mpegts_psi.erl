%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        MPEG TS PSI decoder module
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
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
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(mpegts_psi).
-author('Max Lapshin <max@maxidoors.ru>').

-include("log.hrl").
-include("../include/mpegts.hrl").
-include("mpegts_reader.hrl").

-export([psi/2, encode/2]).

-define(PAT_TABLEID,       16#00).
-define(CAT_TABLEID,       16#01).
-define(PMT_TABLEID,       16#02).
-define(TSDT_TABLEID,      16#03).
-define(NIT_TABLEID,       16#40).
-define(NIT_OTHER_TABLEID, 16#41).
-define(SDT_TABLEID,       16#42).
-define(SDT_OTHER_TABLEID, 16#43).
-define(BAT_TABLEID,       16#42).
-define(EIT_1_TABLEID,     16#4E).
-define(EIT_2_TABLEID,     16#4F).
-define(EIT_3_TABLEID,     16#5F).
-define(EIT_4_TABLEID,     16#6F).
-define(TDT_TABLEID,       16#70).


-define(CA_DESC,           16#09).
-define(SERVICE_DESC,      16#48).
-define(SHORT_DESC,        16#4D).
-define(CONTENT_DESC,      16#54).
-define(PARENTAL_RATING_DESC, 16#55).


%
% Options:
% {programs, [{Number, Pid}]}
% {ts_stream, TSStream = 1}
% {current_next, CNT = 1}
% {section, Section = 0}
encode(pat, Options) ->
  Programs = [<<Number:16, 111:3, Pid:13>> || {Number, Pid} <- proplists:get_value(programs, Options)],
  TSStream = proplists:get_value(ts_stream, Options, 1), % Just the same, as VLC does
  Version = proplists:get_value(version, Options, 0),
  CNI = proplists:get_value(current_next, Options, 1),
  Section = proplists:get_value(section, Options, 0),
  LastSection = 0,
  Misc = <<2#11:2, Version:5, CNI:1, Section, LastSection>>,
  Length = iolist_size(Programs)+5+4,
  PAT1 = iolist_to_binary([<<?PAT_TABLEID, 2#1011:4, Length:12, TSStream:16, Misc/binary>>|Programs]),
  CRC32 = mpeg2_crc32:crc32(PAT1),
  <<0, PAT1/binary, CRC32:32>>;

%
% {program, Program = 1}
% {streams, [{Codec, Pid, Info = []}]}
% {}
encode(pmt, Options) ->
  SectionSyntaxInd = 1,
  ProgramNum = proplists:get_value(program, Options, 1),
  Version = 0,
  CurrentNext = 1,
  _SectionNumber = 0,
  _LastSectionNumber = 0,
  
  PcrPid = case proplists:get_value(pcr_pid, Options) of
    undefined ->
      [{_, FirstPid, _}|_] = proplists:get_value(streams, Options),
      FirstPid;
    Else -> Else
  end,  
  
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
  
  Streams = lists:map(fun({Codec, Pid, _Info = []}) ->
    ESInfo = <<>>,
    CodecId = case Codec of
      aac -> ?TYPE_AUDIO_AAC;
      mpeg2audio -> ?TYPE_AUDIO_MPEG2;
      mp3 -> ?TYPE_AUDIO_MPEG2;
      h264 -> ?TYPE_VIDEO_H264;
      mpeg2video -> ?TYPE_VIDEO_MPEG2
    end, 
    <<CodecId, 2#111:3, Pid:13, 2#1111:4, (size(ESInfo)):12, ESInfo/binary>>
  end, proplists:get_value(streams, Options)),
  
  %
  % Maybe add this for ES info
  % MultipleFrameRate = 0,
  % FrameRateCode = 0,
  % MPEG1Only = 0,
  % ProfileLevel = 0,
  % Chroma = 0,
  % FrameRateExt = 0,
  % VideoES = <<2, (size(VideoConfig)+3), MultipleFrameRate:1, FrameRateCode:4, MPEG1Only:1,
  %             0:1, 0:1, ProfileLevel, Chroma:2, FrameRateExt:1, 0:5,    VideoConfig/binary>>,
  Program = [<<ProgramNum:16, 
           2#11:2, Version:5, CurrentNext:1, 
           _SectionNumber,
           _LastSectionNumber, 
           2#111:3, PcrPid:13, 
           2#1111:4, (size(ProgramInfo)):12, 
           ProgramInfo/binary>>|Streams],
           
  SectionLength = iolist_size(Program) + 4, % Add CRC32
  PMT = iolist_to_binary([<<?PMT_TABLEID, SectionSyntaxInd:1, 0:1, 2#11:2, SectionLength:12>>|Program]),
  
  CRC32 = mpeg2_crc32:crc32(PMT),
  <<0, PMT/binary, CRC32:32>>.



% parse_sdt(<<ServiceId:16, _Reserved:6, EIT_Schedule:1, EIT_present_following:1, RunningStatus:3, FreeCA:1, DescLength:12, _/binary>>) ->
%   [{service_id,ServiceId},{eit_schedule,EIT_Schedule},{eit_present,EIT_present_following},{running,RunningStatus},{free_ca,FreeCA},{desc_len,DescLength}].

psi(<<_Pointer, TableId, _SectionInd:1, _:3, SectionLength:12, TransportStreamId:16, _:2, Version:5, CurrentNext:1, 
             SectionNumber, LastSectionNumber, PSIPayload/binary>> = PSIRaw, #decoder{} = Decoder) ->
  case extract_valid_psi(PSIRaw) of
    {ok, PSI} -> 
      PSITable = #psi_table{
        table_id = TableId,
        ts_stream_id = TransportStreamId,
        version = Version,
        current_next = CurrentNext,
        section_number = SectionNumber,
        last_section_number = LastSectionNumber
      },
      Decoder1 = handle_valid_psi(PSI, PSITable, Decoder),
      {ok, Decoder1, undefined};
    {error, Reason} ->
      ?D({invalid_psi, TableId, Reason, SectionLength - 5, size(PSIPayload)}),
      {ok, Decoder, undefined}
  end.

extract_valid_psi(<<_Ptr, ?TDT_TABLEID, _:4, 5:12, PSI:5/binary, _/binary>>) -> %% TDT has special treatment. Its header is its payload
  {ok, PSI};

extract_valid_psi(<<_Ptr, _:12, Length:12, _/binary>> = Bin) ->
  TotalLen = Length+3 - 4, % PSI table and length and table id minus CRC32
  case Bin of
    <<_Ptr, PSIRaw:TotalLen/binary, CRC32:32, _Trash/binary>> ->
      case mpeg2_crc32:crc32(PSIRaw) of
        CRC32 ->
          <<_PSIHeader:8/binary, PSI/binary>> = PSIRaw,
          {ok, PSI};
        _Else ->
          {error, {invalid_crc32, CRC32, _Else}}
      end;
    _ -> {error, too_short_data}
  end;

extract_valid_psi(_) ->
  {error, too_short_data}.
    

handle_valid_psi(PSI, #psi_table{table_id = TableId} = PSITable, Decoder) ->
  case TableId of
    ?PAT_TABLEID -> pat(PSI, PSITable, Decoder);
    ?CAT_TABLEID -> cat(PSI, PSITable, Decoder);
    ?PMT_TABLEID -> pmt(PSI, PSITable, Decoder);
    ?SDT_TABLEID -> sdt(PSI, PSITable, Decoder);
    ?SDT_OTHER_TABLEID -> sdt(PSI, PSITable, Decoder);
    _ when TableId == 16#4E orelse TableId == 16#4F orelse (TableId >= 16#50 andalso TableId =< 16#6F) -> eit(PSI, PSITable, Decoder);
    16#4E -> eit(PSI, PSITable, Decoder);
    ?TDT_TABLEID -> tdt(PSI, PSITable, Decoder);
    _ ->
      % ?D({psi, TableId, PSI}),
      Decoder
  end.

pat(PAT, _PSITable, #decoder{options = Options, pids = Pids} = Decoder) ->
  Descriptors = extract_pat(PAT, []),
  {PmtPid, SelectedProgram} = select_pmt_pid(Descriptors, proplists:get_value(program, Options)),
  case lists:keyfind(PmtPid, #stream.pid, Pids) of
    false ->
      ?D({"Selecting program", SelectedProgram, "on pid",PmtPid}),
      Decoder#decoder{pids = [#stream{handler = psi, pid = PmtPid}|Pids]};
    _ ->
      Decoder
  end.


select_pmt_pid([{PmtPid, ProgramNum}], undefined) -> % Means no program specified and only one in stream
  {PmtPid, ProgramNum};
select_pmt_pid(Descriptors, SelectedProgram) ->
  case lists:keyfind(SelectedProgram, 2, Descriptors) of
    {PmtPid, SelectedProgram} -> {PmtPid, SelectedProgram};
    _ ->
      ?D({"Has many programs in MPEG-TS, don't know which to choose", Descriptors, SelectedProgram}),
      {undefined, undefined}
  end.

extract_pat(<<ProgramNum:16, _:3, Pid:13, PAT/binary>>, Descriptors) ->
  extract_pat(PAT, [{Pid, ProgramNum} | Descriptors]);

extract_pat(<<_CRC32/binary>>, Descriptors) ->
  lists:reverse(Descriptors).


cat(PSI, _Table, #decoder{pids = Pids} = Decoder) ->
  Descriptors = parse_descriptors(PSI, []),
  CaPids = [Pid || #dvb_ca_desc{pid = Pid} <- Descriptors],
  Pids1 = lists:foldl(fun(CaPid, DecodedPids) ->
    case lists:keymember(CaPid, #stream.pid, DecodedPids) of
      true -> DecodedPids;
      false -> [#stream{handler = emm, pid = CaPid}|DecodedPids]
    end
  end, Pids, CaPids),
  % ?D({cat, Descriptors}),
  Decoder#decoder{pids = Pids1}.


tdt(<<UTC:40>>, _Table, Decoder) ->
  Decoder#decoder{current_time = UTC}.

pmt(<<_Some:3, _PCRPID:13, _Some2:4, ProgramInfoLength:12, _ProgramInfo:ProgramInfoLength/binary, PMT/binary>>,
  #psi_table{ts_stream_id = ProgramNum}, #decoder{pids = Pids} = Decoder) ->
  % ?D({"PMT", size(PMTBin), PMTBin, SectionLength - 13, size(PMT), PMT}),
  % ?D({"Selecting MPEG-TS program", ProgramNum}),
  % io:format("Program info: ~p~n", [_ProgramInfo]),
  % ?D({"PMT", size(PMT), PMTLength, _ProgramInfo}),
  Descriptors = extract_pmt(PMT, Pids),
  % io:format("Streams: ~p~n", [Descriptors]),
  Descriptors1 = lists:map(fun(#stream{} = Stream) ->
    Stream#stream{program_num = ProgramNum, h264 = h264:init()}
  end, Descriptors),
  % AllPids = [self() | lists:map(fun(A) -> element(#stream_out.handler, A) end, Descriptors1)],
  % eprof:start(),
  % eprof:start_profiling(AllPids),
  % Decoder#decoder{pids = lists:keymerge(#stream.pid, Pids, Descriptors1)}.
  Decoder#decoder{pids = Descriptors1}.

extract_pmt(<<_CRC32:32>>, Descriptors) ->
  lists:keysort(#stream.pid, Descriptors);

extract_pmt(<<>>, Descriptors) ->
  lists:keysort(#stream.pid, Descriptors);

extract_pmt(<<StreamType, 2#111:3, Pid:13, _:4, ESLength:12, _ES:ESLength/binary, Rest/binary>>, Descriptors) ->
  Descriptors1 = case lists:keyfind(Pid, #stream.pid, Descriptors) of
    false -> 
      ?D({"Pid -> Type", Pid, StreamType, _ES}),
      [#stream{handler = pes, counter = 0, pid = Pid, codec = stream_codec(StreamType)}|Descriptors];
    _ -> Descriptors
  end,
  extract_pmt(Rest, Descriptors1).


stream_codec(?TYPE_VIDEO_H264) -> h264;
stream_codec(?TYPE_VIDEO_MPEG2) -> mpeg2video;
stream_codec(?TYPE_AUDIO_AAC) -> aac;
stream_codec(?TYPE_AUDIO_AAC2) -> aac;
stream_codec(?TYPE_AUDIO_MPEG1) -> mp3;
stream_codec(?TYPE_AUDIO_MPEG2) -> mpeg2audio;
stream_codec(Type) -> ?D({"Unknown TS PID type", Type}), unhandled.




sdt(<<_OriginalNetwork:16, _Reserved:8, SDT/binary>>, #psi_table{table_id = _TableId}, Decoder) ->
  Info = parse_sdt(SDT, []),
  % ?D({sdt, OriginalNetwork, Info}),
  Decoder#decoder{sdt = Info}.


parse_sdt(<<ServiceId:16, _Reserved:6, EIT_schedule:1, EIT_present_following:1, StatusId:3, FreeCA:1, Length:12, DescriptorsBin:Length/binary, SDT/binary>>, Acc) ->
  Descriptors = parse_descriptors(DescriptorsBin, []),
  Encryped = case FreeCA of
    0 -> false;
    1 -> true
  end,
  Info = [{pid, ServiceId},{eit_schedule,EIT_schedule},{eit_following,EIT_present_following},{status,decode_status(StatusId)},{encrypted,Encryped}]++Descriptors,
  parse_sdt(SDT, [Info|Acc]);


parse_sdt(<<_CRC32/binary>>, Info) ->
  lists:reverse(Info).






eit(<<_TSId:16, _Network:16, _LastSect:8, _LastTable:8, EIT/binary>>, #psi_table{ts_stream_id = _Pid, version = _Version}, 
    #decoder{pids = Streams, program_info = Program} = Decoder) ->
  Pids = [Pid || #stream{pid = Pid} <- Streams],
  case lists:member(_Pid, Pids) of
    true ->
      NewProgram = parse_eit(EIT, []),
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
      Decoder#decoder{program_info = lists:ukeymerge(#eit_event.id, NewProgram, Program)};
    _ ->
      Decoder
  end.

parse_eit(<<EventId:16, StartTime:5/binary, Duration:3/binary, StatusId:3, FreeCA:1, Length:12, DescriptorsBin:Length/binary, EIT/binary>>, Acc) ->
  Descriptors = parse_descriptors(DescriptorsBin, []),
  Encryped = case FreeCA of
    0 -> false;
    1 -> true
  end,
  Event = #eit_event{
    id = EventId,
    start = eit_date_to_erlang(StartTime),
    duration = eit_duration_to_erlang(Duration),
    status = decode_status(StatusId),
    encrypted = Encryped,
    name = proplists:get_value(name, Descriptors),
    language = proplists:get_value(lang, Descriptors),
    about = proplists:get_value(about, Descriptors)
  },
  parse_eit(EIT, [Event|Acc]);

parse_eit(<<_CRC32/binary>>, Acc) -> lists:keysort(#eit_event.id, Acc).


  


decode_status(StatusId) ->
  case StatusId of
    0 -> undefined;
    1 -> not_running;
    2 -> starts_soon;
    3 -> pausing;
    4 -> running;
    _ -> undefined
  end.


parse_descriptors(<<>>, Acc) ->
  lists:reverse(Acc);

parse_descriptors(<<DescId, Len, Content:Len/binary, Bin/binary>>, Acc) ->
  case parse_descriptor(DescId, Content) of
    undefined ->
      parse_descriptors(Bin, Acc);
    {service_description, Desc} ->
      parse_descriptors(Bin, Desc ++ Acc);  
    {short_description, Desc} ->
      parse_descriptors(Bin, Desc ++ Acc);  
    Else ->
      parse_descriptors(Bin, [Else|Acc])
  end.

parse_descriptor(?CA_DESC, <<CaSystemId:16, _:3, Pid:13, Private/binary>>) ->
  #dvb_ca_desc{system_id = CaSystemId, pid = Pid, private = Private};

parse_descriptor(?SERVICE_DESC, <<TypeId, ProvNameLen, ProvName:ProvNameLen/binary, NameLen, Name:NameLen/binary>>) ->
  Type = case TypeId of
    0 -> reserved;
    1 -> digital_tv;
    2 -> digital_radio;
    3 -> teletext;
    4 -> nvod_reference;
    5 -> nvod_shifted;
    6 -> mosaic;
    7 -> pal;
    8 -> secam;
    9 -> d_mac;
    10 -> fm_radio;
    11 -> ntsc;
    12 -> data;
    13 -> common_interface;
    14 -> rcs_map;
    15 -> rcs_fls;
    16 -> dvb_mhp;
    _ -> TypeId
  end,
  {service_description, [{type,Type},{provider,parse_text(ProvName)},{service,parse_text(Name)}]};

parse_descriptor(?SHORT_DESC, <<Lang:3/binary, NameLen, Name:NameLen/binary, TextLen, Text:TextLen/binary>>) ->
  % Short description
  {short_description, [{language,Lang},{name,parse_text(Name)},{about,parse_text(Text)}]};

parse_descriptor(?CONTENT_DESC, _Content) ->
  % Content descriptor
  undefined;

parse_descriptor(?PARENTAL_RATING_DESC, _Content) ->
  % Parental descriptor
  undefined;

parse_descriptor(Unknown, Bin) ->
  ?D({descriptor, Unknown, Bin}),
  undefined.


parse_text(<<1, Text/binary>>) ->
  from_latin(Text);

parse_text(Text) ->
  Text.

from_latin(Text) ->
  iso8859_5:decode(Text).
  % Iconv = case get(iconv_8859_5) of
  %   undefined ->
  %     {ok, Conv} = iconv:open("utf-8", "iso8859-5"),
  %     put(iconv_8859_5, Conv),
  %     Conv;
  %   Else ->
  %     Else
  % end,
  % {ok, Output} = iconv:conv(Iconv, Text),
  % Output.
  % <<1, Text/binary>>.

-define(MJD_1970_01_01, 40587).
-define(SECONDS_IN_DAY, 86400).

eit_duration_to_erlang(<<H1:4, H2:4, M1:4,M2:4, S1:4,S2:4>>) ->
  Hour = H1*10+H2,
  Minute = M1*10+M2,
  Second = S1*10+S2,
  {Hour,Minute,Second}.

eit_date_to_erlang(<<MJD:16, H1:4, H2:4, M1:4,M2:4, S1:4,S2:4>>) ->
  Hour = H1*10+H2,
  Minute = M1*10+M2,
  Second = S1*10+S2,
  
  Epoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
  UTCDate = (MJD - ?MJD_1970_01_01)*?SECONDS_IN_DAY,
  {Date, _} = calendar:gregorian_seconds_to_datetime(UTCDate + Epoch),
  
  Result = {Date, {Hour,Minute,Second}},
  % UTC = calendar:datetime_to_gregorian_seconds(Result) - Epoch,
  % {Result, UTC}.
  Result.


-include_lib("eunit/include/eunit.hrl").

eit_date_to_erlang_test() ->
  ?assertEqual({{2011,5,27},{5,0,0}}, eit_date_to_erlang(<<217,156,5,0,0>>)).

eit_duration_to_erlang_test() ->
  ?assertEqual({0,20,0}, eit_duration_to_erlang(<<0,32,0>>)).
