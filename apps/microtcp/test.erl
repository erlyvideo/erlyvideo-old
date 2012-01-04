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
  
  % Client = spawn(fun() ->
  %   connect(9000)
  % end),
  % erlang:monitor(process, Client),
  % 
  % receive
  %   {'DOWN', _, process, Client, Reason1} -> io:format("client died ~p~n", [Reason1])
  % end,
  receive
    {'DOWN', _, process, Listener, Reason2} -> io:format("listener died ~p~n", [Reason2])
  end,
  ok.

-define(SIZE, 100000).


listen(Port) ->
  Opts1 = [binary, {packet, raw}, {reuseaddr, true}, 
          {keepalive, true}, {backlog, 400}, {active, once}],
  {ok, Listen} = microtcp:listen(Port, Opts1),
  ?S({open_port,Listen}),
  Bin = crypto:rand_bytes(?SIZE),
  put(clients, 0),  
  listen_loop(Listen, Bin).
  
listen_loop(Listen, Bin) ->  
  receive
    {tcp_connection, Listen, Socket} ->
      Pid = spawn(fun() ->
        client_launch(Bin)
      end),
      microtcp:controlling_process(Socket, Pid),
      Pid ! {socket, Socket},
      % ?S({client,connected});
      erlang:monitor(process, Pid),
      put(clients, get(clients)+1),
      ?S({spawned_client,get(clients)}),
      listen_loop(Listen, Bin);
    {'DOWN', _, _, _, _} ->
      put(clients, get(clients)-1),
      ?S({died_client, get(clients)}),
      listen_loop(Listen, Bin);
    Else ->
      ?S(Else)  
  end.

connect(Port) ->
  % {ok, _R1} = httpc:request("http://localhost:"++integer_to_list(Port)++"/index.html"),
  % {ok, _R2} = httpc:request(post, {"http://localhost:"++integer_to_list(Port)++"/index.html", [], "application/octet-stream", "a=b&c=d"}, [], []),
  % {ok, _R3} = httpc:request(put, {"http://localhost:"++integer_to_list(Port)++"/index.html", [], "text/plain", "Hi!!!\ndamn\n"}, [], []),
  ok.
  
  
client_launch(Bin) ->
  % Bin = <<"Hello world!\n">>,
  Reply = iolist_to_binary([
    "HTTP/1.1 200 OK\r\n",
    "Connection: Keep-Alive\r\n",
    io_lib:format("Content-Length: ~p\r\n", [size(Bin)]),
    "\r\n",
    Bin
  ]),
  receive
    {socket, Socket} -> client_loop(Socket, Reply)
  end.

client_loop(Socket, Reply) ->
  microtcp:active_once(Socket),
  % receive
  %   {tcp, Socket, Bin} ->
  %     ?S({Bin, size(Bin)})
  % end,  
  receive
    {http, Socket, Method, URL, _Version, Headers} = Req ->
      % ?S(Req),
      microtcp:send(Socket, Reply),
      client_loop(Socket, Reply);
    {tcp_closed, Socket} ->
      ok;
    {tcp_error, Socket, timeout} ->
      microtcp:close(Socket)  
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