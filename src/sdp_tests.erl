%% @hidden
-module(sdp_tests).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("eunit/include/eunit.hrl").



beward_sdp() ->
  <<"v=0
o=- 1275067839203788 1 IN IP4 0.0.0.0
s=Session streamed by \"nessyMediaServer\"
i=h264
t=0 0
a=tool:LIVE555 Streaming Media v2008.04.09
a=type:broadcast
a=control:*
a=range:npt=0-
a=x-qt-text-nam:Session streamed by \"nessyMediaServer\"
a=x-qt-text-inf:h264
m=video 0 RTP/AVP 99
c=IN IP4 0.0.0.0
a=rtpmap:99 H264/90000
a=fmtp:99 packetization-mode=28;profile-level-id=4D0028.; sprop-parameter-sets=J00AKI2NKAoAt2AtQEBAUAAAPoAADDUGhgBQAABZ9LvLjQwAoAAAs+l3lwT6LAA=,KO48gA==
a=control:track1
a=cliprect:0,0,1280,720
a=framerate:25.000000
m=audio 7878 RTP/AVP 8
a=rtpmap:8 PCMA/8000/1
a=control:track2">>.

beward_test() ->
  ?assertEqual([
   {rtsp_stream,video,90.0,"track1",h264,
                <<40,238,60,128>>,
                <<39,77,0,40,141,141,40,10,0,183,96,45,64,64,64,80,0,0,62,128,0,12,53,6,134,0,
                  80,0,0,89,244,187,203,141,12,0,160,0,0,179,233,119,151,4,250,44,0>>,
                undefined},
  {rtsp_stream,audio,8.0,"track2",pcma,
                undefined,undefined,undefined}], sdp:decode(beward_sdp())).
  



quicktime_broadcaster_sdp() ->
<<"v=0
o=- 165 931665148 IN IP4 127.0.0.0
s=QuickTime
c=IN IP4 127.0.0.1
t=0 0
a=x-qt-text-an©:railsconf
a=range:npt=now-
a=isma-compliance:2,2.0,2
m=audio 0 RTP/AVP 96
b=AS:8
a=rtpmap:96 mpeg4-generic/8000/1
a=fmtp:96 profile-level-id=15;mode=AAC-hbr;sizelength=13;indexlength=3;indexdeltalength=3;config=1588
a=mpeg4-esid:101
a=control:trackid=1
m=video 0 RTP/AVP 97
b=AS:300
a=rtpmap:97 H264/90000
a=fmtp:97 packetization-mode=1;profile-level-id=4D400C;sprop-parameter-sets=J01ADKkYUI/LgDUGAQa2wrXvfAQ=,KN4JF6A=
a=mpeg4-esid:201
a=cliprect:0,0,120,160
a=framesize:97 160-120
a=control:trackid=2
">>.

quicktime_broadcaster_test() ->
  ?assertEqual([
    {rtsp_stream,audio,8.0,"trackid=1",aac,undefined,undefined,<<21,136>>},
    {rtsp_stream,video,90.0,"trackid=2",h264,
                <<40,222,9,23,160>>,
                <<39,77,64,12,169,24,80,143,203,128,53,6,1,6,182,194,181,239,124,4>>,
                undefined}], sdp:decode(quicktime_broadcaster_sdp())).
  

axis_m1011_sdp() ->
  <<"v=0
o=- 1273580251173374 1273580251173374 IN IP4 axis-00408ca51334.local
s=Media Presentation
e=NONE
c=IN IP4 0.0.0.0
b=AS:50000
t=0 0
a=control:*
a=range:npt=0.000000-
m=video 0 RTP/AVP 96
b=AS:50000
a=framerate:30.0
a=control:trackID=1
a=rtpmap:96 H264/90000
a=fmtp:96 packetization-mode=1; profile-level-id=420029; sprop-parameter-sets=Z0IAKeNQFAe2AtwEBAaQeJEV,aM48gA==">>.
  
  
axis_m1011_test() ->
  ?assertEqual([
   {rtsp_stream,video,90.0,"trackID=1",h264,
                <<104,206,60,128>>,
                <<103,66,0,41,227,80,20,7,182,2,220,4,4,6,144,120,145,21>>,
                undefined}], sdp:decode(axis_m1011_sdp())).
  

axis_p1311_test() ->
  SDP = <<"v=0
o=- 1269257289197834 1269257289197834 IN IP4 10.10.11.48
s=Media Presentation
e=NONE
c=IN IP4 0.0.0.0
b=AS:50032
t=0 0
a=control:rtsp://10.10.11.48:554/axis-media/media.amp?videocodec=h264
a=range:npt=0.000000-
m=video 0 RTP/AVP 96
b=AS:50000
a=framerate:30.0
a=control:rtsp://10.10.11.48:554/axis-media/media.amp/trackID=1?videocodec=h264
a=rtpmap:96 H264/90000
a=fmtp:96 packetization-mode=1; profile-level-id=420029; sprop-parameter-sets=Z0IAKeNQFAe2AtwEBAaQeJEV,aM48gA==
m=audio 0 RTP/AVP 97
b=AS:32
a=control:rtsp://10.10.11.48:554/axis-media/media.amp/trackID=2?videocodec=h264
a=rtpmap:97 mpeg4-generic/16000/1
a=fmtp:97 profile-level-id=15; mode=AAC-hbr;config=1408; SizeLength=13; IndexLength=3;IndexDeltaLength=3; Profile=1; bitrate=32000;">>,
  ?assertEqual([{rtsp_stream,video,90.0,"rtsp://10.10.11.48:554/axis-media/media.amp/trackID=1?videocodec=h264",h264,
                <<104,206,60,128>>,
                <<103,66,0,41,227,80,20,7,182,2,220,4,4,6,144,120,145,21>>,
                undefined},
      {rtsp_stream,audio,16.0,
         "rtsp://10.10.11.48:554/axis-media/media.amp/trackID=2?videocodec=h264",aac,
        undefined,undefined,
        <<20,8>>}], sdp:decode(SDP)).


axis_test() ->
  ?assertEqual([{rtsp_stream,video,90.0,"trackID=1",h264,
                <<104,206,60,128>>,
                <<103,66,0,41,227,80,20,7,182,2,220,4,4,6,144,120,145,21>>,
                undefined}], sdp:decode(<<"v=0\r\no=- 1266472632763124 1266472632763124 IN IP4 192.168.4.1\r\ns=Media Presentation\r\ne=NONE\r\nc=IN IP4 0.0.0.0\r\nb=AS:50000\r\nt=0 0\r\na=control:*\r\na=range:npt=0.000000-\r\nm=video 0 RTP/AVP 96\r\nb=AS:50000\r\na=framerate:25.0\r\na=control:trackID=1\r\na=rtpmap:96 H264/90000\r\na=fmtp:96 packetization-mode=1; profile-level-id=420029; sprop-parameter-sets=Z0IAKeNQFAe2AtwEBAaQeJEV,aM48gA==\r\n">>)).
                

% D-Link is an IP-camera, not AVC-camera. ertsp doesn't currently support it.
% http://github.com/erlyvideo/erlyvideo/issues/issue/5              
dlink_dcs_2121_test() ->
  ?assertError(function_clause, sdp:decode(<<"v=0\r\no=CV-RTSPHandler 1123412 0 IN IP4 10.48.135.130\r\ns=D-Link DCS-2121\r\nc=IN IP4 0.0.0.0\r\nt=0 0\r\na=charset:Shift_JIS\r\na=range:npt=now-\r\na=control:*\r\na=etag:1234567890\r\nm=video 0 RTP/AVP 96\r\nb=AS:18\r\na=rtpmap:96 MP4V-ES/90000\r\na=control:trackID=1\r\na=fmtp:96 profile-level-id=1;config=000001B001000001B509000001000000012000C488BA98514043C1443F;decode_buf=76800\r\na=sendonly\r\nm=audio 0 RTP/AVP 0\r\na=rtpmap:0 PCMU/8000\r\na=control:trackID=2\r\na=sendonly\r\n">>)).



darwinss_test() ->
  ?assertEqual([{rtsp_stream,video,90.0,"trackID=1",h264,
                undefined,undefined,undefined}], sdp:decode(<<"v=0\r\no=- 1188340656180883 1 IN IP4 224.1.2.3\r\ns=Session streamed by GStreamer\r\ni=server.sh\r\nt=0 0\r\na=tool:GStreamer\r\na=type:broadcast\r\nm=video 10000 RTP/AVP 96\r\nc=IN IP4 224.1.2.3\r\na=rtpmap:96 H264/90000\r\na=framerate:30">>)).
                
darwin_sdp() ->
  <<"v=0
o=StreamingServer 3485077701 1211414520000 IN IP4 129.85.244.160
s=/evolution/coyne.mov
u=http:///
e=admin@
c=IN IP4 0.0.0.0
b=AS:876
t=0 0
a=control:*
a=range:npt=0-3973.80667
m=video 0 RTP/AVP 96
b=AS:734
a=rtpmap:96 H264/90000
a=control:trackID=1
a=cliprect:0,0,360,480
a=range:npt=0-3973.8071
a=fmtp:96 packetization-mode=1;profile-level-id=42E016;sprop-parameter-sets=Z0LAFpZ0DwX/LgCBAAALuwACvyC0YAw4BJd73weEQjU=,aN48gA==
m=audio 0 RTP/AVP 97
b=AS:142
a=rtpmap:97 MP4A-LATM/48000/2
a=control:trackID=2
a=range:npt=0-3973.8240
a=fmtp:97 profile-level-id=15;object=2;cpresent=0;config=400023203FC0
">>.

darwin_test() ->
  ?assertEqual([{rtsp_stream,video,90.0,"trackID=1",h264,<<104,222,60,128>>,<<103,66,192,22,150,116,15,5,255,46,0,129,0,0,11,
                 187,0,2,191,32,180,96,12,56,4,151,123,223,7,132,66,53>>,
                      undefined},
                      {rtsp_stream,audio,48.0,"trackID=2","MP4A-LATM",undefined,
                                          undefined,<<64,0,35,32,63,192>>}], sdp:decode(darwin_sdp())).

                