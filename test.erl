#!/usr/local/bin/escript 
%%! -pa ebin


main(["test"]) ->
  http_file:test();

main([]) ->
  inets:start(),
  URL = "http://erlyvideo.org/video.mp4",
  Limit = 100,
  File = http_file:open(URL, [{cache_file, "video.mp4"}]),
  
  Self = self(),
  spawn(fun() ->
    {ok, Result} = http_file:pread(File, 200000, Limit),
    io:format("~p~n", [size(Result)]),
    Self ! tick
  end),

  spawn(fun() ->
    {ok, Result} = http_file:pread(File, 1000000, Limit),
    io:format("~p~n", [size(Result)]),
    Self ! tick
  end),
  
  spawn(fun() ->
    {ok, Result} = http_file:pread(File, 2000000, Limit),
    io:format("~p~n", [size(Result)]),
    Self ! tick
  end),
  
  wait(3).
  
wait(0) ->
  ok;
  
wait(N) ->
  receive
    tick -> wait(N-1)
  end.
  
  