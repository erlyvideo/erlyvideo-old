#!/usr/local/bin/escript 
%%! -pa ebin
-define(D(X), io:format("DEBUG ~p ~p~n",[?LINE, X])).

main(["test"]) ->
  http_file:test();

main([]) ->
  http_file:start(),
  URL = "http://localhost/video.mp4",
  Limit = 100,
  {ok, File} = http_file:open(URL, []),
  
  ?D({open, File}),
  
  Self = self(),
  spawn(fun() ->
    Result = http_file:pread(File, 33212403, Limit),
    io:format("~p~n", [Result]),
    Self ! tick
  end),

  spawn(fun() ->
    Result = http_file:pread(File, 33212400, Limit),
    io:format("~p~n", [Result]),
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
  
  timer:sleep(1000),
  {ok, File1} = http_file:open(URL, []),
  
  timer:sleep(10000),
  
  wait(4),
  http_file:close(File),
  http_file:close(File1),
  erlang:monitor(process, File),
  receive
    {'DOWN', _, process, File, _Reason} -> ok
  end.
    
  
wait(0) ->
  ok;
  
wait(N) ->
  receive
    tick -> wait(N-1)
  end.
  
  