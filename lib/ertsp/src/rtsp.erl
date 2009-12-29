-module(rtsp).
-author(max@maxidoors.ru).

-export([start_server/3]).

start_server(Port, Name, Callback) ->
  rtsp_sup:start_rtsp_listener(Port, Name, Callback).
  
