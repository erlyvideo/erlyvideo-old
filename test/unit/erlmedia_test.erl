#!/usr/bin/env escript
%% -*- erlang -*-


main(["aac"]) ->
  aac:test();
  
main(["flv_video_frame"]) ->
  flv_video_frame:test();
  
main(["h264"]) ->
  h264:test();
  
main(["mp4"]) ->
  mp4:test().

