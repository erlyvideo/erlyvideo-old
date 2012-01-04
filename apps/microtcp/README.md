TCP-HTTP driver for erlang
==========================

This module is a high efficient special TCP-HTTP driver.

It uses http://github.com/joyent/http-parser/ code based on Nginx http parser, which is beleived
to be one of the fastest on earth.

Microtcp has non-gen_tcp API:


```erlang
% listener.erl:
{ok, Listener} = microtcp:listen(8080, [{reuseaddr,true},{backlog,400}]),
receive
  {tcp_connection, Listen, Socket} ->
    Pid = spawn(fun() -> handle_client() end),
    microtcp:controlling_process(Socket, Pid),
    Pid ! {socket, Socket},
    receive
      {done, Pid}
    end
end.

handle_client() ->
  Socket = receive
    {socket, S} -> S
  end,
  client_loop(Socket).

client_loop(Socket) ->
  microtcp:active_once(Socket),
  receive
  {http, Socket, Method, URL, _Version, Headers} = Req ->
    microtcp:send(Socket, "HTTP/1.1 200 OK\r\n\r\n"),
    client_loop(Socket);
    {tcp_closed, Socket} ->
      ok;
    {tcp_error, Socket, timeout} ->
      microtcp:close(Socket)  
  end.
```

Current state is only proof-of-concept, so does not have handling anything except GET request.
