#!/usr/local/bin/escript 
%%! -pa ebin


main([]) ->
  inets:start(),
  % URL = "http://s3.amazonaws.com/erlyvideo/test/video.mp4",
  URL = "http://erlang.org/",
  Limit = 100,
  File = http_file:open("http://erlang.org/", []),
  {ok, Result} = http_file:pread(File, 100, Limit),
  
  File2 = http_file:open("http://ya.ru/", []),
  {ok, Result2} = http_file:pread(File2, 100, Limit),
  io:format("~s~n---------~n ~s~n", [Result, Result2]).
  
  
  
  
  
  