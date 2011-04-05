%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2011 Max Lapshin
%%% @doc        ASF unpacking module
%%% http://download.microsoft.com/download/9/5/E/95EF66AF-9026-4BB0-A41D-A4F81802D92C/%5BMS-WMSP%5D.pdf
%%%
%%% http://download.microsoft.com/download/7/9/0/790fecaa-f64a-4a5e-a430-0bccdab3f1b4/ASF_Specification.doc
%%% http://www.microsoft.com/windows/windowsmedia/forpros/format/asfspec.aspx
%%% http://msdn.microsoft.com/en-us/library/ms983668(loband).aspx
%%% http://msdn.microsoft.com/en-us/library/dd757562(VS.85).aspx
%%% http://www.digitalpreservation.gov/formats/fdd/fdd000067.shtml
%%% http://avifile.sourceforge.net/docs.htm
%%% http://avifile.sourceforge.net/asf-1.0.htm
%%% http://msdn.microsoft.com/en-us/library/ms983653.aspx
%%% http://www.microsoft.com/windows/windowsmedia/knowledgecenter/technicalarticles.aspx
%%% 
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlmedia.
%%% 
%%%---------------------------------------------------------------------------------------
-module(asf).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include_lib("eunit/include/eunit.hrl").



-compile(export_all).

guids() ->
  [
    %% 10.1 Top Level ASF Object GUIDs.
    {header, <<"75B22630-668E-11CF-A6D9-00AA0062CE6C">>},
    {data, <<"75B22636-668E-11CF-A6D9-00AA0062CE6C">>},
    {simple_index, <<"33000890-E5B1-11CF-89F4-00A0C90349CB">>},
    {index, <<"D6E229D3-35DA-11D1-9034-00A0C90349BE">>},
    {media_object_index, <<"FEB103F8-12AD-4C64-840F-2A1D2F7AD48C">>},
    {timecode_index, <<"3CB73FD0-0C4A-4803-953D-EDF7B6228F0C">>},

    %% 10.2 Header Object GUIDs.
    {file_properties, <<"8CABDCA1-A947-11CF-8EE4-00C00C205365">>},
    {stream_properties, <<"B7DC0791-A9B7-11CF-8EE6-00C00C205365">>},
    {header_extension, <<"5FBF03B5-A92E-11CF-8EE3-00C00C205365">>},
    {codec_list, <<"86D15240-311D-11D0-A3A4-00A0C90348F6">>},
    {script_command, <<"1EFB1A30-0B62-11D0-A39B-00A0C90348F6">>},
    {marker, <<"F487CD01-A951-11CF-8EE6-00C00C205365">>},
    {bitrate_mutual_exclusion, <<"D6E229DC-35DA-11D1-9034-00A0C90349BE">>},
    {error_correction, <<"75B22635-668E-11CF-A6D9-00AA0062CE6C">>},
    {content_description, <<"75B22633-668E-11CF-A6D9-00AA0062CE6C">>},
    {extended_content_description, <<"D2D0A440-E307-11D2-97F0-00A0C95EA850">>},
    {content_branding, <<"2211B3FA-BD23-11D2-B4B7-00A0C955FC6E">>},
    {stream_bitrate_properties, <<"7BF875CE-468D-11D1-8D82-006097C9A2B2">>},
    {content_encryption, <<"2211B3FB-BD23-11D2-B4B7-00A0C955FC6E">>},
    {extended_content_encryption, <<"298AE614-2622-4C17-B935-DAE07EE9289C">>},
    {digital_signature, <<"2211B3FC-BD23-11D2-B4B7-00A0C955FC6E">>},
    {padding, <<"1806D474-CADF-4509-A4BA-9AABCB96AAE8">>},
    
    %% 10.3 Header Extension Object GUIDs
    {extended_stream_properties, <<"14E6A5CB-C672-4332-8399-A96952065B5A">>},
    {advanced_mutual_exclusion, <<"A08649CF-4775-4670-8A16-6E35357566CD">>},
    {group_mutual_exclusion, <<"D1465A40-5A79-4338-B71B-E36B8FD6C249">>},
    {stream_prioritization, <<"D4FED15B-88D3-454F-81F0-ED5C45999E24">>},
    {bandwidth_sharing, <<"A69609E6-517B-11D2-B6AF-00C04FD908E9">>},
    {language_list, <<"7C4346A9-EFE0-4BFC-B229-393EDE415C85">>},
    {metadata, <<"C5F8CBEA-5BAF-4877-8467-AA8C44FA4CCA">>},
    {metadata_library, <<"44231C94-9498-49D1-A141-1D134E457054">>},
    {index_parameters, <<"D6E229DF-35DA-11D1-9034-00A0C90349BE">>},
    {media_object_index_parameters, <<"6B203BAD-3F11-48E4-ACA8-D7613DE2CFA7">>},
    {timecode_index_parameters, <<"F55E496D-9797-4B5D-8C8B-604DFE9BFB24">>},
    {compatibility, <<"26F18B5D-4584-47EC-9F5F-0E651F0452C9">>},
    {advanced_content_encryption, <<"43058533-6981-49E6-9B74-AD12CB86D58C">>},
    

    %% 10.4 Stream Properties Object Stream Type GUIDs
    {audio_media, <<"F8699E40-5B4D-11CF-A8FD-00805F5C442B">>},
    {video_media, <<"BC19EFC0-5B4D-11CF-A8FD-00805F5C442B">>},
    {command_media, <<"59DACFC0-59E6-11D0-A3AC-00A0C90348F6">>},
    {jfif_media, <<"B61BE100-5B4E-11CF-A8FD-00805F5C442B">>},
    {degradable_jpeg_media, <<"35907DE0-E415-11CF-A917-00805F5C442B">>},
    {file_transfer_media, <<"91BD222C-F21C-497A-8B6D-5AA86BFC0185">>},
    {binary_media, <<"3AFB65E2-47EF-40F2-AC2C-70A90D71D343">>},
    
    %% 10.5 Stream Properties Object Error Correction Type GUIDs
    {no_error_correction, <<"20FB5700-5B55-11CF-A8FD-00805F5C442B">>},
    {audio_spread, <<"BFC3CD50-618F-11CF-8BB2-00AA00B4E220">>},
    
    %% 10.13
   {payload_ext_timecode, <<"399595EC-8667-4E2D-8FDB-98814CE76C1E">>},
   {payload_ext_file_name, <<"E165EC0E-19ED-45D7-B4A7-25CBD1E28E9B">>},
   {payload_ext_content_type, <<"D590DC20-07BC-436C-9CF7-F3BBFBF1A4DC">>},
   {payload_ext_par, <<"1B1EE554-F9EA-4BC8-821A-376B74E4C4B8">>},
   {payload_ext_sample_duration, <<"C6BD9450-867F-4907-83A3-C77921B733AD">>},
   {payload_ext_encryption_sample, <<"6698B84E-0AFA-4330-AEB2-1C0A98D7A44D">>},
   {payload_ext_degradable_jpeg, <<"00E1AF06-7BEC-11D1-A582-00C04FC29CFB">>},
   % {, <<"">>},
   % {, <<"">>},
    {null, <<"">>}
  ].


decode_guid(<<G1:32/little, G2:16/little, G3:16/little, G4:16, G5:48>>) ->
  GUID = <<G1:32, G2:16, G3:16, G4:16, G5:48>>,
  UUID = list_to_binary(string:to_upper(uuid:to_string(GUID))),
  case lists:keyfind(UUID, 2, guids()) of
    false -> UUID;
    {Key, UUID} -> Key
  end.  

encode_guid(Key) when is_atom(Key) ->
  encode_guid(proplists:get_value(Key, guids()));

encode_guid(GUID) when is_list(GUID) ->
  encode_guid(list_to_binary(GUID));

encode_guid(_GUID) ->
  ok.

get(_URL) ->
  RequestHeaders = [
    {"Accept", "*/*"},
    {"User-Agent", "NSPlayer/7.10.0.3059"},
    {"Pragma", "no-cache,rate=1.000000,stream-time=0,stream-offset=0:0,request-context=1,max-duration=0"},
    {"Pragma", "xClientGUID={"++uuid:to_string(uuid:v4())++"}"},
    {"Connection", "Close"}
  ],
  {ok, _Headers, MMSHeader} = http_stream:get_with_body("http://live.rfn.ru/vesti_24", [{send_hostpath,true}, {headers, RequestHeaders}]),
  ?D({reply, _Headers}),
  file:write("vesti_24.asf", MMSHeader),
  mms_header(MMSHeader).
  
mms_header(<<>>) ->
  ok;

mms_header(<<$$, $H, Length:16/little, Packet:Length/binary>>) ->
  <<LocationId:32/little, Incarnation, AFFlags, PacketSize:16/little, ASF/binary>> = Packet,
  ?D({Length, LocationId, Incarnation, AFFlags, PacketSize, size(ASF)}),
  ?D(asf_objects(ASF)),
  ok.
  
% mms_header(<<$$, $H, Length:16/little, Packet:Length/binary, Rest/binary>>) ->
%   % asf_header(Packet),
%   mms_header(Rest).

asf_objects(Bin) ->
  asf_objects(Bin, []).
  
asf_objects(Bin, Acc) ->  
  case asf_object(Bin) of
    {Object, <<>>} -> lists:reverse([Object|Acc]);
    {Object, Rest} -> asf_objects(Rest, [Object|Acc])
  end.
  
asf_object(<<GUID:16/binary, Len:64/little, ASF/binary>>) ->
  Length = Len - 24,
  <<Data:Length/binary, Rest/binary>> = ASF,
  {asf_object(GUID, Data), Rest}.

asf_object(GUID, ASF) ->
  unpack_asf_object(decode_guid(GUID), ASF).

unpack_asf_object(GUID, ASF) when is_atom(GUID) ->
  case erlang:function_exported(?MODULE, GUID, 2) of
    true -> {GUID, ?MODULE:GUID(ASF, ok)};
    false -> {GUID, ASF}
  end;

unpack_asf_object(GUID, ASF) ->
  {GUID, ASF}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%     ASF specific tags                                               %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
b(1) -> true; b(0) -> false.

header(<<_Count:32/little, _Reserve1, _Reserve2, Rest/binary>>, _) ->
  asf_objects(Rest).

file_properties(<<FileID:16/binary, FileSize:64/little, CreationDate:64/little, DataPackets:64/little,
                  PlayDuration:64/little, SendDuration:64/little, Preroll:64/little,
                  Broadcast:1, Seekable:1, _Reserved:30, MinDataSize:32/little, MaxDataSize:32/little, MaxBitrate:32/little>>, _) ->
  [{file_id,decode_guid(FileID)}, {file_size, FileSize}, {creation_date, CreationDate}, {data_packets, DataPackets},
   {play_duration, PlayDuration / 10000}, {send_duration, SendDuration / 10000}, {preroll, Preroll},
   {broadcast, b(Broadcast)}, {seekable, b(Seekable)}, {min_data_size, MinDataSize}, {max_data_size, MaxDataSize}, 
   {max_bitrate, MaxBitrate}].


stream_properties(<<StreamType:16/binary, ErrorCorrectionType:16/binary, TimeOffset:64/little, DataLength:32/little, ErrorCorrectionLength:32/little,
                    StreamNumber:7/little, _Reserved1:8, Encrypted:1, _Reserved2:32, 
                    Data:DataLength/binary, ErrorCorrection:ErrorCorrectionLength/binary>>, _) ->
  [{stream_type, decode_guid(StreamType)}, {error_correction_type, decode_guid(ErrorCorrectionType)},
   {time_offset, TimeOffset / 10000}, {stream_number, StreamNumber}, {encrypted, b(Encrypted)}, 
   {properties, unpack_stream_properties(decode_guid(StreamType), Data)}, 
   {error_correction, unpack_error_correction(decode_guid(ErrorCorrectionType), ErrorCorrection)}].


header_extension(<<_Reserved1:16/binary, _Reserved2:16/little, Length:32/little, Data:Length/binary>>, _) ->
  asf_objects(Data).


padding(_, _) ->
  null.

codec_list(<<_Reserved1:16/binary, _Count:32/little, Codecs/binary>>, _) ->
  decode_codec_list(Codecs).

language_list(<<_Count:16/little, Languages/binary>>, _) ->
  F = fun
    (<<>>, Acc, _) -> lists:reverse(Acc);
    (<<Len, Lang:Len/binary, Bin/binary>>, Acc, G) -> G(Bin, [unpack_unicode(Lang)|Acc], G)
  end,
  F(Languages, [], F).

metadata(<<_Count:16/little, Meta/binary>>, _) ->
  unpack_metadata(Meta, []).


content_description(<<TitleLen:16/little, AuthorLen:16/little, CopyrightLen:16/little, DescLen:16/little, RatingLen:16/little,
                      Title:TitleLen/binary, Author:AuthorLen/binary, Copyright:CopyrightLen/binary, Desc:DescLen/binary, Rating:RatingLen/binary>>, _) ->
  [{title, unpack_unicode(Title)}, {author, unpack_unicode(Author)}, {copyright, unpack_unicode(Copyright)}, {description, unpack_unicode(Desc)},
   {rating, unpack_unicode(Rating)}].

extended_content_description(<<_Count:16/little, Desc/binary>>, _) ->
  unpack_ext_content_desc(Desc, []).

extended_stream_properties(<<Start:64/little, End:64/little, Bitrate:32/little, BufferSize:32/little, InitialBufferFullness:32/little,
                             AltDataBitrate:32/little, AltBufferSize:32/little, AltBufferFullness:32/little,
                             MaximumObjectSize:32/little, Reliable:1, Seekable:1, NoCleanpoints:1, ResendLiveCleanpoints:1, _Reserve1:28,
                             StreamId:16/little, StreamLanguageId:16/little, AvgTimePerFrame:64/little, 
                             StreamNameCount:16/little, PayloadExtensionCount:16/little, Bin/binary>>, _) ->
  {Names, Rest1} = extract_stream_names(Bin, StreamNameCount, []),
  {PayloadExtensions, Rest2} = extract_payload_extensions(Rest1, PayloadExtensionCount, []),
  <<>> = Rest2,
  [{start, Start/10000},{'end', End/10000},{bitrate,Bitrate},{buffer_size,BufferSize},{initial_buffer_fullness, InitialBufferFullness},
  {alt_bitrate,AltDataBitrate},{alt_buffer_size,AltBufferSize},{alt_buffer_fullness,AltBufferFullness},
  {max_object,MaximumObjectSize},{reliable,b(Reliable)},{seekable,b(Seekable)},{no_cleanpoints,b(NoCleanpoints)},
  {resend_cleanpoints,b(ResendLiveCleanpoints)},{stream_id,StreamId},{language_id,StreamLanguageId},{avg_frame_time, AvgTimePerFrame/10000},
  {names, Names},{payload_extensions, PayloadExtensions}].

stream_bitrate_properties(<<_Count:16/little, Bin/binary>>, _) ->
  unpack_bitrate_props(Bin, []).


simple_index(<<FileId:16/binary, EntryTime:64/little, MaxPacketCount:32/little, _Count:32/little, Data/binary>>, _) ->
  Keyframes = parse_simple_index(Data, []),
  [{file_id,decode_guid(FileId)},{entry_time,EntryTime},{max_count,MaxPacketCount},{keyframes,Keyframes}].


data(<<FileId:16/binary, Packets:64/little, _Reserved:16, Data/binary>>, _) ->
  [{file_id,decode_guid(FileId)},{count,Packets},{frames, parse_data(Data, [])}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%     decoder helpers                                                 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


decode_codec_list(Codecs) -> decode_codec_list(Codecs, []).
decode_codec_list(<<>>, Acc) -> lists:reverse(Acc);
decode_codec_list(<<Type:16/little, Data/binary>>, Acc) ->
  F = fun(1) -> video; (2) -> audio; (16#FFFF) -> undefined end,
  {Name, Rest1} = read_unicode(Data),
  {Desc, <<InfoLen:16/little, Info:InfoLen/binary, Codecs/binary>>} = read_unicode(Rest1),
  decode_codec_list(Codecs, [{F(Type), Name, Desc, Info}|Acc]).

read_unicode(<<Length:16/little, Bin/binary>>) ->
  Len = Length*2,
  <<Unicode:Len/binary, Rest/binary>> = Bin,
  {unpack_unicode(Unicode), Rest}.

unpack_unicode(Bin) -> unpack_unicode(Bin, []).
unpack_unicode(<<>>, [0|Acc]) -> unpack_unicode(<<>>, Acc);
unpack_unicode(<<>>, Acc) -> lists:reverse(Acc);
unpack_unicode(<<Code:16/little, Bin/binary>>, Acc) -> unpack_unicode(Bin, [Code|Acc]).


unpack_metadata(<<>>, Acc) -> lists:reverse(Acc);

unpack_metadata(<<0:16, StreamId:16/little, NameLen:16/little, DataType:16/little, DataLen:32/little, 
                  Name:NameLen/binary, Data:DataLen/binary, Rest/binary>>, Acc) ->
  Value = decode_value_type(DataType, Data),
  unpack_metadata(Rest, [{unpack_unicode(Name),Value,StreamId}|Acc]).

decode_value_type(0, Data) -> unpack_unicode(Data);
decode_value_type(1, Data) -> Data;
decode_value_type(2, <<0:16/little>>) -> false;
decode_value_type(2, <<1:16/little>>) -> true;
decode_value_type(2, <<0:32/little>>) -> false;
decode_value_type(2, <<1:32/little>>) -> true;
decode_value_type(3, <<I:32/little>>) -> I;
decode_value_type(4, <<I:64/little>>) -> I;
decode_value_type(5, <<I:16/little>>) -> I.


unpack_ext_content_desc(<<>>, Acc) -> lists:reverse(Acc);
unpack_ext_content_desc(<<NameLen:16/little, Name:NameLen/binary, Type:16/little, 
                          ValLen:16/little, Value:ValLen/binary, Rest/binary>>, Acc) ->
  unpack_ext_content_desc(Rest, [{unpack_unicode(Name), decode_value_type(Type,Value)}|Acc]).


extract_stream_names(Bin, _Count = 0, Acc) -> {lists:reverse(Acc), Bin};
extract_stream_names(<<Lang:16/little, Len:16/little, Name:Len/binary, Bin/binary>>, Count, Acc) -> 
  extract_stream_names(Bin, Count - 1, [{unpack_unicode(Name),Lang}|Acc]).


extract_payload_extensions(Bin, _Count = 0, Acc) -> {lists:reverse(Acc), Bin};
extract_payload_extensions(<<GUID:16/binary, DataSize:16/little, Len:32/little, Info:Len/binary, Bin/binary>>, Count, Acc) -> 
  extract_payload_extensions(Bin, Count - 1, [{decode_guid(GUID),DataSize, Info}|Acc]).

unpack_bitrate_props(<<>>, Acc) -> lists:reverse(Acc);
unpack_bitrate_props(<<StreamId:7, _Reserve:9, AvgBitrate:32/little, Bin/binary>>, Acc) -> unpack_bitrate_props(Bin, [{StreamId,AvgBitrate}|Acc]).


unpack_stream_properties(audio_media, <<CodecId:16/little, Channels:16/little, SampleRate:32/little, AvgByterate:32/little,
                         BlockAlign:16/little, BitsPerSample:16/little, Len:16/little, CodecSpecific:Len/binary>>) ->
  [{codec,CodecId},{channels,Channels},{sample_rate,SampleRate},{avg_bitrate,AvgByterate*8},{block_align,BlockAlign},
  {bits_per_sample,BitsPerSample},{specific,CodecSpecific}];

unpack_stream_properties(video_media, <<Width:32/little, Height:32/little, _Reserve1, Len:16/little, Format:Len/binary>>) ->
  <<_Len1:32/little, Width:32/little, Height:32/little, _Reserve2:16, BPP:16/little, CompressionId:32/little,
  ImageSize:32/little, HorizPix:32/little, VertPix:32/little, ColorsCount:32/little, ImportantColors:32/little, CodecSpecific/binary>> = Format,
  [{width,Width},{height,Height},{specific,CodecSpecific},{bit_per_pixel,BPP},{compression_id,CompressionId},
  {image_size,ImageSize},{horiz_pix,HorizPix},{vert_pix,VertPix},{colors_count,ColorsCount},{important_colors,ImportantColors}].


unpack_error_correction(no_error_correction, <<>>) -> none;
unpack_error_correction(audio_spread, <<Span,PacketLen:16/little,ChunkLen:16/little,Len:16/little,Silence:Len/binary>>) ->
  [{span,Span},{packet_len,PacketLen},{chunk_len,ChunkLen},{silence,Silence}].


parse_simple_index(<<>>, Acc) -> lists:reverse(Acc);
parse_simple_index(<<Number:32/little, Count:16/little, Data/binary>>, Acc) -> parse_simple_index(Data, [{Number,Count}|Acc]).

parse_data(<<>>, Acc) -> lists:reverse(Acc);

% 5.2.1 Error correction data
parse_data(<<_:7, 1:1, _/binary>> = Data, Acc) ->
  read_err_correction_data(Data, Acc);

parse_data(<<_:7, 0:1, _/binary>> = Data, Acc) ->
  read_payload_parsing_data(Data, [], Acc).

read_err_correction_data(<<Len:4, HasOpaque:1, 0:2, _ErrPresent:1, CorrectionData:Len/binary, Data/binary>>, Acc) ->
  HasOpaque = 0,
  read_payload_parsing_data(Data, [{correction, CorrectionData}], Acc).

read_payload_parsing_data(<<MultiplePayload:1, SeqType:2, PadLenType:2, PacketLenType:2, _HasErrCorr:1, 
                            ReplDataLenType:2, OfftLenType:2, MediaObjNumLenType:2, _StreamIdType:2, Bin/binary>>, ErrCorr, Acc) ->
  MultiplePayload = 1,
  Word = fun
    (0, Bin_) -> {0, 0, Bin_};
    (1, <<I, Bin_/binary>>) -> {I, 1, Bin_};
    (2, <<I:16/little, Bin_/binary>>) -> {I, 2, Bin_};
    (3, <<I:32/little, Bin_/binary>>) -> {I, 4, Bin_}
  end,
  {PacketLen, PacketLenSize, BinWithRest1} = Word(PacketLenType, Bin),
  RestPacketLen = PacketLen - PacketLenSize,
  <<OnePacket:RestPacketLen/binary, Rest/binary>> = BinWithRest1,
  {Sequence, _, Rest2} = Word(SeqType, OnePacket),
  {_PaddingLen, _, <<SendTime:32/little, Duration:32/little, StreamId, Rest3/binary>>} = Word(PadLenType, Rest2),
  {MediaObjNumber, _, Rest4} = Word(MediaObjNumLenType, Rest3),
  {_OffsetInfo, _, Rest5} = Word(OfftLenType, Rest4),
  {ReplDataLen, _, Rest6} = Word(ReplDataLenType, Rest5),
  <<_ReplData:ReplDataLen/binary, _Rest7/binary>> = Rest6,
  Info = ErrCorr ++ [{stream_id,StreamId},{send_time,SendTime},{duration,Duration},{number,MediaObjNumber},{sequence,Sequence}],
  parse_data(Rest, [Info|Acc]).
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%     file reading functions                                          %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


read_asf_object(F, Pos) ->
  case file:pread(F, Pos, 24) of
    eof -> eof;
    {ok, <<GUID:16/binary, Len:64/little>>} ->
      {ok, Data} = file:pread(F, Pos + 24, Len - 24),
      {ok, asf_object(GUID, Data), Pos + Len}
  end.

read_wmv_file(F, Pos) ->
  case read_asf_object(F, Pos) of
    eof -> ok;
    {ok, {GUID, ASF}, NewPos} ->
      ?D({GUID, dump(ASF)}),
      read_wmv_file(F, NewPos)
  end.

dump(Bin) when is_binary(Bin) -> size(Bin);
dump(Else) -> Else.

read_file_test() ->
  {ok, F} = file:open(ems_test_helper:file_path("v.wmv"), [read,binary]),
  read_wmv_file(F, 0).


