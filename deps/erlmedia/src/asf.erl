%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2011 Max Lapshin
%%% @doc        ASF unpacking module
%%% http://download.microsoft.com/download/9/5/E/95EF66AF-9026-4BB0-A41D-A4F81802D92C/%5BMS-WMSP%5D.pdf
%%%
%%% http://download.microsoft.com/download/7/9/0/790fecaa-f64a-4a5e-a430-0bccdab3f1b4/ASF_Specification.doc
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



-compile(export_all).

guids() ->
  [
    {header, <<"75B22630-668E-11CF-A6D9-00AA0062CE6C">>},
    {data, <<"75B22636-668E-11CF-A6D9-00AA0062CE6C">>},
    {simple_index, <<"33000890-E5B1-11CF-89F4-00A0C90349CB">>},
    {index, <<"D6E229D3-35DA-11D1-9034-00A0C90349BE">>},
    {media_object_index, <<"FEB103F8-12AD-4C64-840F-2A1D2F7AD48C">>},
    {timecode_index, <<"3CB73FD0-0C4A-4803-953D-EDF7B6228F0C">>}
  ].

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
