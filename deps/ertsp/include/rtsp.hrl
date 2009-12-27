-ifndef(D).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-define(TIMEOUT, 1000).
-endif.

-record(rtsp_stream, {
  id,
  type,
  transport,
  payload_type,
  clock_map,
  pps,
  sps,
  config
}).
