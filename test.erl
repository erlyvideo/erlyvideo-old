#!/usr/local/bin/escript 
%%! -pa ebin


main(["test"]) ->
  http_file:test();

main([]) ->
  inets:start(),
  URL = "http://s3.amazonaws.com/erlyvideo/test/video.mp4",
  Limit = 100,
  File = http_file:open(URL, [{cache_file, "video.mp4"}]),
  {ok, Result} = http_file:pread(File, 200000, Limit),
  
  io:format("~p~n", [size(Result)]).
  
  
  
  
  