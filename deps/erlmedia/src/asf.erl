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
    {header, <<"75B22630-668E-11CF-A6D9-00AA0062CE6C">>},
    {data, <<"75B22636-668E-11CF-A6D9-00AA0062CE6C">>},
    {simple_index, <<"33000890-E5B1-11CF-89F4-00A0C90349CB">>},
    {index, <<"D6E229D3-35DA-11D1-9034-00A0C90349BE">>},
    {media_object_index, <<"FEB103F8-12AD-4C64-840F-2A1D2F7AD48C">>},
    {timecode_index, <<"3CB73FD0-0C4A-4803-953D-EDF7B6228F0C">>},

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
    {padding, <<"1806D474-CADF-4509-A4BA-9AABCB96AAE8">>}
  ].

decode_guid(GUID) ->
  UUID = list_to_binary(string:to_upper(uuid:to_string(GUID))),
  case lists:keyfind(UUID, 2, guids()) of
    false -> UUID;
    {Key, UUID} -> Key
  end.  

encode_guid(Key) when is_atom(Key) ->
  encode_guid(proplists:get_value(Key, guids()));

encode_guid(GUID) when is_list(GUID) ->
  encode_guid(list_to_binary(GUID));

encode_guid(GUID) ->
  ok.

get(URL) ->
  RequestHeaders = [
    {"Accept", "*/*"},
    {"User-Agent", "NSPlayer/7.10.0.3059"},
    {"Pragma", "no-cache,rate=1.000000,stream-time=0,stream-offset=0:0,request-context=1,max-duration=0"},
    {"Pragma", "xClientGUID={"++uuid:to_string(uuid:v4())++"}"},
    {"Connection", "Close"}
  ],
  {ok, Headers, MMSHeader} = http_stream:get_with_body("http://live.rfn.ru/vesti_24", [{send_hostpath,true}, {headers, RequestHeaders}]),
  mms_header(MMSHeader).
  
mms_header(<<>>) ->
  ok;

mms_header(<<$$, $H, Length:16/little, Packet:Length/binary>>) ->
  <<LocationId:32/little, Incarnation, AFFlags, PacketSize:16/little, ASF/binary>> = Packet,
  ?D({Length, LocationId, Incarnation, AFFlags, PacketSize, size(ASF)}),
  asf_packet(ASF),
  ok.
  
% mms_header(<<$$, $H, Length:16/little, Packet:Length/binary, Rest/binary>>) ->
%   % asf_header(Packet),
%   mms_header(Rest).
  
asf_packet(<<GUID:16/binary, Len:64/little, ASF/binary>>) ->
  ?D({GUID, Len, size(ASF)}),
  ok.

read_asf_object(F, Pos) ->
  case file:pread(F, Pos, 16) of
    eof -> eof;
    {ok, <<G1:32/little, G2:16/little, G3:16/little, G4:16, G5:48>>} ->
      GUID = <<G1:32, G2:16, G3:16, G4:16, G5:48>>,
      {ok, <<Len:64/little>>} = file:pread(F, Pos + 16, 8),
      {ok, ASF} = file:pread(F, Pos + 24, Len - 24),
      {ok, {GUID, ASF}, Pos + Len}
  end.

read_wmv_file(F, Pos) ->
  case read_asf_object(F, Pos) of
    eof -> ok;
    {ok, {GUID, ASF}, NewPos} ->
      ?D({decode_guid(GUID), size(ASF)}),
      read_wmv_file(F, NewPos)
  end.

read_file_test() ->
  {ok, F} = file:open(ems_test_helper:file_path("v.wmv"), [read,binary]),
  read_wmv_file(F, 0).


