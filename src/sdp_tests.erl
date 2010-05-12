-module(sdp_tests).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("eunit/include/eunit.hrl").


axis_p1311_test() ->
  ?assertEqual([{rtsp_stream,undefined,audio,undefined,97,16.0,aac,
                undefined,undefined,
                <<20,8>>},
   {rtsp_stream,undefined,video,undefined,96,90.0,h264,
                <<104,206,60,128>>,
                <<103,66,0,41,227,80,20,7,182,2,220,4,4,6,144,120,145,21>>,
                undefined}], sdp:decode(<<"v=0\r\no=- 1269257289197834 1269257289197834 IN IP4 10.10.11.48\r\ns=Media Presentation\r\ne=NONE\r\nc=IN IP4 0.0.0.0\r\nb=AS:50032\r\nt=0 0\r\na=control:rtsp://10.10.11.48:554/axis-media/media.amp?videocodec=h264\r\na=range:npt=0.000000-\r\n""m=video 0 RTP/AVP 96\r\nb=AS:50000\r\na=framerate:30.0\r\na=control:rtsp://10.10.11.48:554/axis-media/media.amp/trackID=1?videocodec=h264\r\na=rtpmap:96 H264/90000\r\na=fmtp:96 packetization-mode=1; profile-level-id=420029; sprop-parameter-sets=Z0IAKeNQFAe2AtwEBAaQeJEV,aM48gA==\r\nm=audio 0 RTP/AVP 97\r\nb=AS:32\r\na=control:rtsp://10.10.11.48:554/axis-media/media.amp/trackID=2?videocodec=h264\r\na=rtpmap:97 mpeg4-generic/16000/1\r\na=fmtp:97 profile-level-id=15; mode=AAC-hbr;config=1408; SizeLength=13; IndexLength=3;IndexDeltaLength=3; Profile=1; bitrate=32000;\r\n">>)).


axis_test() ->
  ?assertEqual([{rtsp_stream,undefined,video,undefined,96,90.0,h264,
                <<104,206,60,128>>,
                <<103,66,0,41,227,80,20,7,182,2,220,4,4,6,144,120,145,21>>,
                undefined}], sdp:decode(<<"v=0\r\no=- 1266472632763124 1266472632763124 IN IP4 192.168.4.1\r\ns=Media Presentation\r\ne=NONE\r\nc=IN IP4 0.0.0.0\r\nb=AS:50000\r\nt=0 0\r\na=control:*\r\na=range:npt=0.000000-\r\nm=video 0 RTP/AVP 96\r\nb=AS:50000\r\na=framerate:25.0\r\na=control:trackID=1\r\na=rtpmap:96 H264/90000\r\na=fmtp:96 packetization-mode=1; profile-level-id=420029; sprop-parameter-sets=Z0IAKeNQFAe2AtwEBAaQeJEV,aM48gA==\r\n">>)).
                

% D-Link is an IP-camera, not AVC-camera. ertsp doesn't currently support it.
% http://github.com/erlyvideo/erlyvideo/issues/issue/5              
dlink_dcs_2121_test() ->
  ?assertError(function_clause, sdp:decode(<<"v=0\r\no=CV-RTSPHandler 1123412 0 IN IP4 10.48.135.130\r\ns=D-Link DCS-2121\r\nc=IN IP4 0.0.0.0\r\nt=0 0\r\na=charset:Shift_JIS\r\na=range:npt=now-\r\na=control:*\r\na=etag:1234567890\r\nm=video 0 RTP/AVP 96\r\nb=AS:18\r\na=rtpmap:96 MP4V-ES/90000\r\na=control:trackID=1\r\na=fmtp:96 profile-level-id=1;config=000001B001000001B509000001000000012000C488BA98514043C1443F;decode_buf=76800\r\na=sendonly\r\nm=audio 0 RTP/AVP 0\r\na=rtpmap:0 PCMU/8000\r\na=control:trackID=2\r\na=sendonly\r\n">>)).



darwinss_test() ->
  ?assertEqual([{rtsp_stream,undefined,video,undefined,96,90.0,h264,
                undefined,undefined,undefined}], sdp:decode(<<"v=0\r\no=- 1188340656180883 1 IN IP4 224.1.2.3\r\ns=Session streamed by GStreamer\r\ni=server.sh\r\nt=0 0\r\na=tool:GStreamer\r\na=type:broadcast\r\nm=video 10000 RTP/AVP 96\r\nc=IN IP4 224.1.2.3\r\na=rtpmap:96 H264/90000\r\na=framerate:30">>)).