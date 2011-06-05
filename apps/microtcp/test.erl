#!/usr/bin/env ERL_LIBS=.. escript
%%! -pa ebin

main([]) ->
  Opts1 = [binary, {packet, raw}, {reuseaddr, true}, 
          {keepalive, true}, {backlog, 30}, {active, false}],
  {ok, Listen} = microtcp:listen(9000, Opts1),
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



client_launch() ->
  Bin = crypto:rand_bytes(1000000),
  receive
    {socket, Socket} -> client_loop(Socket, Bin)
  end.

client_loop(Socket, Bin) ->
  microtcp:send(Socket, Bin),
  receive
    {tcp_closed, Socket} -> ok;
    Else -> io:format("Msg: ~p~n", [Else]),
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