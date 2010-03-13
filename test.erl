#!/usr/local/bin/escript 


main([]) ->
  inets:start(),
  % URL = "http://s3.amazonaws.com/erlyvideo/test/video.mp4",
  URL = "http://erlang.org/",
  Limit = 100,
  {ok, Result} = pread(URL, 100, Limit),
  io:format("~p ~s~n", [size(Result), Result]).
  
  
pread(URL, Offset, Limit) ->
  Range = lists:flatten(io_lib:format("bytes=~p-~p", [Offset, Offset+Limit])),
  {ok, RequestID} = http:request(get, {URL, [{"Range", Range}]}, [], [{sync, false},{receiver, self()},{body_format,binary},{stream,self}]),
  wait_response(RequestID, <<>>, Limit).
  
  
wait_response(RequestID, Buffer, Limit) when size(Buffer) >= Limit ->
  {Result, _} = split_binary(Buffer, Limit),
  httpc:cancel_request(RequestID),
  {ok, Result};
  
wait_response(RequestID, Buffer, Limit) ->
  receive
    {http, {RequestID, stream, Bin}} ->
      wait_response(RequestID, <<Buffer/binary, Bin/binary>>, Limit);
    {http, {RequestID, stream_end, _}} ->
      Buffer;
    Message ->
      wait_response(RequestID, Buffer, Limit)
  end.

  
  
  