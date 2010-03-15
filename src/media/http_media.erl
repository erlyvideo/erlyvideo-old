-module(http_media).
-author('Max Lapshin <max@maxidoors.ru>').
-export([start_link/2]).

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

%% gen_server callbacks
start_link(URL, Opts) when is_binary(URL) ->
  start_link(binary_to_list(URL), Opts);
  
start_link(URL, Opts) ->
  {_, _, Host, Port, Path, Query} = http_uri:parse(URL),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, line}, {active, false}], 4000),
  ?D({Host, Path, Query, "GET "++Path++" HTTP/1.1\r\nHost: "++Host++":"++integer_to_list(Port)++"\r\nAccept: */*\r\n\r\n"}),
  gen_tcp:send(Socket, "GET "++Path++" HTTP/1.1\r\nHost: "++Host++":"++integer_to_list(Port)++"\r\nAccept: */*\r\n\r\n"),
  {ok, Line} = gen_tcp:recv(Socket, 0, 4000),
  Response = string:tokens(binary_to_list(Line), " "),
  case Response of
    ["ICY", "200"| _] ->
      ?D({"Shoutcast detected on", URL}),
      {ok, Pid} = ems_sup:start_media(URL, shoutcast, Opts),
      Pid ! {tcp, Socket, Line};
    [HTTP, "200"|_] when HTTP == "HTTP/1.0" orelse HTTP == "HTTP/1.1"-> 
      ?D({"MPEG TS detected on", URL}),
      {ok, Pid} = ems_sup:start_media(URL, mpeg_ts, Opts),
      Pid ! {http, Socket, {http_response, 0, 200, 0}}
  end,
  stream_media:pass_socket(Pid, Socket),
  {ok, Pid}.

% {ok, Socket} = gen_tcp:connect("ya.ru", 80, [binary, {packet, line}, {active, false}], 4000),
% gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
% {ok, Packet} = gen_tcp:recv(Socket, 0).
