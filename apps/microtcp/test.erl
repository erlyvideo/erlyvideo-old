#!/usr/bin/env ERL_LIBS=.. escript
%%! -pa ebin

-define(C(X), io:format("client:~p ~p~n", [?LINE,X])).
-define(S(X), io:format("server:~p ~p~n", [?LINE,X])).

main([]) ->
  inets:start(),
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
  ?S({open_port,Listen}),
  receive
    {tcp_connection, Listen, Socket} ->
      Pid = spawn(fun() ->
        client_launch()
      end),
      microtcp:controlling_process(Socket, Pid),
      Pid ! {socket, Socket},
      ?S({client,connected}),
      erlang:monitor(process, Pid)
  end,
  receive
    Msg -> io:format("Msg: ~p~n", [Msg])
  end,
  ok.

-define(SIZE, 100000).

connect(Port) ->
  {ok, _R1} = httpc:request("http://localhost:"++integer_to_list(Port)++"/index.html"),
  {ok, _R2} = httpc:request("http://localhost:"++integer_to_list(Port)++"/index.html"),
  {ok, _R3} = httpc:request("http://localhost:"++integer_to_list(Port)++"/index.html"),
  ok.
  
  
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