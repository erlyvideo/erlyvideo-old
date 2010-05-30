-ifndef(D).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-define(TIMEOUT, 1000).
-endif.

-record(rtsp_stream, {
  type,
  clock_map,
  track_control,
  codec,
  pps,
  sps,
  config
}).
