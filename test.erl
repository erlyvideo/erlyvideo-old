#!/usr/local/bin/escript 
%%! -pa ebin


main(["test"]) ->
  http_file:test();

main([]) ->
  inets:start(),
  % URL = "http://s3.amazonaws.com/erlyvideo/test/video.mp4",
  _URL = "http://erlang.org/",
  Limit = 100,
  File = http_file:open("http://erlang.org/", [{cache_file, "erlang.html"}]),
  {ok, Result} = http_file:pread(File, 20000, Limit),
  
  io:format("~s~n", [Result]).
  
  
  
  
  