-module(rtsp).
-author(max@maxidoors.ru).

-export([start_server/2]).

start_server(RTSP, Callback) ->
  rtsp_sup:start_rtsp_listener(RTSP, Callback).
  
