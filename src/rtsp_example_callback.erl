-module(rtsp_example_callback).
-behaviour(rtsp).
-author('Max Lapshin <max@maxidoors.ru>').

% -export([handle_rtsp_response/2, handle_rtp_packet/2, handle_rtsp_request/2, media/1]).

-export([record/2, recorder/1, announce/3]).

record(URL, _Headers) ->
  Pid = spawn_link(?MODULE, recorder, [URL]),
  {ok, Pid}.
  
  
recorder(URL) ->
  receive
    stop ->
      io:format("TEARDOWN~n"),
      ok;
    Frame ->
      Type = element(2, Frame),
      DTS = round(element(3, Frame)),
      io:format("F: ~s ~p~n", [Type, DTS]),
      recorder(URL)
  end.


announce(_URL, _Headers, _Body) ->
  ok.
