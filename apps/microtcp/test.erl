#!/usr/bin/env ERL_LIBS=.. escript
%%! -pa ebin

main([]) ->
  Listener = spawn(fun() ->
    listen(9000)
  end),
  erlang:monitor(process, Listener),
  
  Client = spawn(fun() ->
    connect(9000)
  end),
  erlang:monitor(process, Client),
  
  receive
    {'DOWN', _, process, Client, Reason1} -> io:format("client died ~p~n", [Reason1])
  end,
  receive
    {'DOWN', _, process, Listener, Reason2} -> io:format("listener died ~p~n", [Reason2])
  end,
  ok.


listen(Port) ->
  Opts1 = [binary, {packet, raw}, {reuseaddr, true}, 
          {keepalive, true}, {backlog, 30}, {active, once}],
  {ok, Listen} = microtcp:listen(Port, Opts1),
  io:format("Open port: ~p~n", [Listen]),
  receive
    {tcp_connection, Listen, Socket} ->
      Pid = spawn(fun() ->
        client_launch()
      end),
      microtcp:controlling_process(Socket, Pid),
      Pid ! {socket, Socket},
      io:format("Client connected~n"),
      erlang:monitor(process, Pid)
  end,
  receive
    Msg -> io:format("Msg: ~p~n", [Msg])
  end,
  ok.

-define(SIZE, 100000).

connect(Port) ->
  {ok, Sock} = gen_tcp:connect("localhost", Port, [binary]),
  gen_tcp:recv(Sock, 0),
  gen_tcp:recv(Sock, 0),
  gen_tcp:recv(Sock, 0),
  gen_tcp:close().

client_launch() ->
  Bin = crypto:rand_bytes(?SIZE),
  receive
    {socket, Socket} -> client_loop(Socket, Bin)
  end.

client_loop(Socket, Bin) ->
  microtcp:send(Socket, <<(size(Bin)):32, Bin/binary>>),
  receive
    {tcp_closed, Socket} -> ok;
    Else -> 
      io:format("Msg: ~p~n", [Else]),
      inet:setopts(Socket, [{active,once}]),
      client_loop(Socket, Bin)
  after
    40 -> client_loop(Socket, Bin)
  end.
  % microtcp:active_once(Socket),
  % receive
  %   {tcp, Socket, Data} ->
  %     io:format("Data from client: ~p~n", [Data]),
  %     client_loop(Socket);
  %   Else ->
  %     io:format("Client msg: ~p~n", [Else]),
  %     ok
  % end.