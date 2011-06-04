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
-module(mpegts_psi_decoder).
-author('Max Lapshin <max@maxidoors.ru>').

-include("log.hrl").
-include("../include/mpegts.hrl").
-include("mpegts_reader.hrl").

-export([psi/3]).


% parse_sdt(<<ServiceId:16, _Reserved:6, EIT_Schedule:1, EIT_present_following:1, RunningStatus:3, FreeCA:1, DescLength:12, _/binary>>) ->
%   [{service_id,ServiceId},{eit_schedule,EIT_Schedule},{eit_present,EIT_present_following},{running,RunningStatus},{free_ca,FreeCA},{desc_len,DescLength}].

psi(<<_Pointer, TableId, _SectionInd:1, _:3, SectionLength:12, TransportStreamId:16, _:2, Version:5, CurrentNext:1, 
             SectionNumber, LastSectionNumber, PSIRaw/binary>> = _Bin, Stream, #decoder{pids = Streams} = Decoder) ->
  PSILength = SectionLength - 5,             
  {PSI, _Trash} = erlang:split_binary(PSIRaw, PSILength),
  %   size(PSIRaw) >= PSILength -> ;
  %   true ->
  %     ?D({too_short_psi,PSILength, TableId, size(PSIRaw), PSIRaw}),
  %     {PSIRaw, <<>>}
  % end,
  PSITable = #psi_table{
    id = TableId,
    ts_stream_id = TransportStreamId,
    version = Version,
    current_next = CurrentNext,
    section_number = SectionNumber,
    last_section_number = LastSectionNumber
  },
  % ?D({psi, TableId}),
  Decoder1 = Decoder#decoder{pids = [Stream|Streams]},
  Decoder2 = case TableId of
    ?PMT_TABLEID -> pmt(PSI, PSITable, Decoder1);
    ?SDT_TABLEID -> sdt(PSI, PSITable, Decoder1);
    ?SDT_OTHER_TABLEID -> sdt(PSI, PSITable, Decoder1);
    % _ when TableId == 16#4E orelse TableId == 16#4F orelse (TableId >= 16#50 andalso TableId =< 16#6F) -> eit(PSI, PSITable, Decoder1);
    16#4E -> eit(PSI, PSITable, Decoder1);
    _ ->
      % ?D({psi, TableId, PSI}),
      Decoder1
  end,
  % ?D({psi, length(Decoder1#decoder.pids)}),
  {ok, Decoder2, undefined}.




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




sdt(<<_OriginalNetwork:16, _Reserved:8, SDT/binary>>, #psi_table{id = _TableId}, Decoder) ->
  Info = parse_sdt(SDT, []),
  % ?D({sdt, OriginalNetwork, Info}),
  Decoder#decoder{sdt = Info}.


parse_sdt(<<_CRC32:32>>, Info) ->
  lists:reverse(Info);

parse_sdt(<<ServiceId:16, _Reserved:6, EIT_schedule:1, EIT_present_following:1, StatusId:3, FreeCA:1, Length:12, DescriptorsBin:Length/binary, SDT/binary>>, Acc) ->
  Descriptors = parse_descriptors(DescriptorsBin, []),
  Encryped = case FreeCA of
    0 -> false;
    1 -> true
  end,
  Info = [{pid, ServiceId},{eit_schedule,EIT_schedule},{eit_following,EIT_present_following},{status,decode_status(StatusId)},{encrypted,Encryped}]++Descriptors,
  parse_sdt(SDT, [Info|Acc]).




eit(<<TSId:16, Network:16, _LastSect:8, _LastTable:8, EIT/binary>>, #psi_table{ts_stream_id = Pid, version = Version}, Decoder) ->
  _Info = parse_eit(EIT, []),
  io:format("new EIT service_id=~p version=~p ts_id=~p network_id=~p~n", [Pid, Version, TSId, Network]),
  [begin
    Id = proplists:get_value(id, Event),
    Start = proplists:get_value(start, Event),
    Duration = proplists:get_value(duration, Event),
    Status = proplists:get_value(status, Event),
    Lang = proplists:get_value(language, Event),
    Name = proplists:get_value(name, Event),
    About = proplists:get_value(about, Event),
    io:format("  * event id=~p start_time:~p duration=~p running=~p~n    - short lang=~s '~s' : '~s'~n", [Id, Start, Duration, Status, Lang, Name, About])
  end || Event <- _Info],  
  % ?D({eit, _Info}),
  Decoder.

parse_eit(<<EventId:16, StartTime:5/binary, Duration:3/binary, StatusId:3, FreeCA:1, Length:12, DescriptorsBin:Length/binary, EIT/binary>>, Acc) ->
  Descriptors = parse_descriptors(DescriptorsBin, []),
  Encryped = case FreeCA of
    0 -> false;
    1 -> true
  end,
  Info = [{id, EventId},{start,eit_date_to_erlang(StartTime)},{duration,eit_duration_to_erlang(Duration)},{status,decode_status(StatusId)},{encrypted,Encryped}]++Descriptors,
  parse_eit(EIT, [Info|Acc]);

parse_eit(<<_CRC32/binary>>, Acc) -> lists:reverse(Acc).


  


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
  Iconv = case get(iconv_8859_5) of
    undefined ->
      {ok, Conv} = iconv:open("utf-8", "iso8859-5"),
      put(iconv_8859_5, Conv),
      Conv;
    Else ->
      Else
  end,
  {ok, Output} = iconv:conv(Iconv, Text),
  Output.
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
