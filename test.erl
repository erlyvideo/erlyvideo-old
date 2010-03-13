#!/usr/local/bin/escript 
%%! -pa ebin


main([]) ->
  inets:start(),
  % URL = "http://s3.amazonaws.com/erlyvideo/test/video.mp4",
  URL = "http://erlang.org/",
  Limit = 100,
  File = http_file:open(URL, []),
  {ok, Result} = http_file:pread(File, 100, Limit),
  io:format("~p ~s~n", [size(Result), Result]).
  
  
  
  
  
  