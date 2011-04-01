#!/usr/local/bin/escript 
%%! -pa ebin
-define(D(X), io:format("DEBUG ~p ~p~n",[?LINE, X])).

main(["test"]) ->
  http_file:test();

main([]) ->
  URL = "http://localhost/video.mp4",
  Limit = 100,
  {ok, File} = http_file:open(URL, [binary,{timeout,360000}]),
  
  ?D({open, File}),
  
  Self = self(),
  spawn(fun() ->
    {ok, Result} = http_file:pread(File, 0, 33213403),
    io:format("1 ~p~n", [size(Result)]),
    Self ! tick
  end),
  wait(1),
  init:stop(),



  spawn(fun() ->
    {ok, Result} = http_file:pread(File, 33212403, Limit),
    io:format("1 ~p~n", [size(Result)]),
    Self ! tick
  end),

  spawn(fun() ->
    {ok, Result} = http_file:pread(File, 33212400, Limit),
    io:format("2 ~p~n", [size(Result)]),
    Self ! tick
  end),

  spawn(fun() ->
    {ok, Result} = http_file:pread(File, 1000000, Limit),
    io:format("3 ~p~n", [size(Result)]),
    Self ! tick
  end),
  
  spawn(fun() ->
    {ok, Result} = http_file:pread(File, 2000000, Limit),
    io:format("4 ~p~n", [size(Result)]),
    Self ! tick
  end),
  
  timer:sleep(1000),
  {ok, File1} = http_file:open(URL, []),
  ?D({open,File1}),
  
  wait(4),
  http_file:close(File),
  http_file:close(File1),
  
  {http_file,F, _} = File,
  erlang:monitor(process, F),
  receive
    {'DOWN', _, process, F, _Reason} -> ok
  end.
    
  
wait(0) ->
  ok;
  
wait(N) ->
  receive
    tick -> wait(N-1)
  end.
  
  