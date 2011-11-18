%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        reverse mpegts
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(reverse_mpegts).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([start/2, loop/2]).

-define(COUNTER, 100).
-define(TIMEOUT, 5000).

-record(pusher, {
  from_url,
  to_url,
  from_counter = 0,
  to_counter = 0,
  from = undefined,
  to = undefined
}).


start(FromUrl, ToUrl) ->
  spawn(?MODULE, loop, [FromUrl, ToUrl]).

loop(FromUrl, ToUrl) ->
  Pid = spawn(fun() ->
    run_loop(#pusher{from_url = FromUrl, to_url = ToUrl})
  end),
  erlang:monitor(process, Pid),
  receive
    {'DOWN', _Ref, process, _Source, _Reason} ->
      io:format("Down ~p ~p, restarting~n", [_Source, _Reason])
  end,
  loop(FromUrl, ToUrl).    
  

  

connect_source("http://"++_ = From) ->
  {_, _, Host, Port, Path, Query} = http_uri:parse(From),
  ?D({connecting_to, source, From}),
  case gen_tcp:connect(Host, Port, [binary, {packet, http_bin}, {active, false}], 1000) of
    {ok, Socket} -> 
      ?D({connected_to,source, From, Socket}),
      ok = gen_tcp:send(Socket, "GET "++Path++Query++" HTTP/1.1\r\nHost: "++Host++"\r\n\r\n"),
      read_response(Socket, From);
    Else ->
      ?D({"Cannot connect source", From, Else}),
      timer:sleep(500),
      connect_source(From)
  end;
  
connect_source(File) ->
  Port = erlang:open_port({spawn_executable, os:find_executable("cat")}, [binary, {args, [File]}, exit_status]),
  {port, Port}.
  
read_response(Socket, From) ->
  ok = inet:setopts(Socket, [{active, once}]),
  receive 
    {http, Socket, {http_response, _Version, 200, _Reply}} ->
      read_headers(Socket, From);
    {tcp_closed, Socket} ->
      ?D({"Socket closed", From}),
      timer:sleep(500),
      connect_source(From);
    Else -> 
      erlang:error({read_response_error, Else})
  after
    ?TIMEOUT -> 
      ?D({"Timeout reading http response"}),
      false
  end.
  
read_headers(Socket, From) ->
  ok = inet:setopts(Socket, [{active, once}]),
  receive
    {http, Socket, {http_header, _, _Header, _, _Value}} ->
      read_headers(Socket, From);
    {http, Socket, http_eoh} ->
      ok = inet:setopts(Socket, [{packet, raw}]),
      ?D({"Connected to source"}),
      {socket, Socket};
    {tcp_closed, Socket} ->
      ?D({"Socket closed", From}),
      timer:sleep(500),
      connect_source(From)
  after
    ?TIMEOUT -> 
      ?D({"Timeout reading http headers"}),
      false
  end.
  
    


connect_to(To) ->
  {_, _, Host, Port, Path, Query} = http_uri:parse(To),
  ?D({connecting_to,destination,To}),
  case gen_tcp:connect(Host, Port, [binary, {packet, http_bin}, {active, false}], 1000) of
    {ok, Socket} ->
      ok = gen_tcp:send(Socket, "PUT "++Path++Query++" HTTP/1.1\r\nHost: "++Host++"\r\n\r\n"),
      ?D({connected_to,destination,To,Socket}),
      Socket;
    Else ->
      ?D({"Destination is down", To, Else}),
      timer:sleep(1000),
      connect_to(To)
  end.
  

run_loop(#pusher{from_counter = ?COUNTER, from_url = URL}) ->
  ?D({"Too many connections to ", URL}),
  ok;

run_loop(#pusher{to_counter = ?COUNTER, to_url = URL}) ->
  ?D({"Too many connections to ", URL}),
  ok;

run_loop(#pusher{from = false, from_counter = Counter} = Pusher) ->
  run_loop(Pusher#pusher{from = undefined, from_counter = Counter + 1});

run_loop(#pusher{to = false, to_counter = Counter} = Pusher) ->
  run_loop(Pusher#pusher{to = undefined, to_counter = Counter + 1});
  
run_loop(#pusher{from = undefined, from_url = URL} = Pusher) ->
  run_loop(Pusher#pusher{from = connect_source(URL)});

run_loop(#pusher{to = undefined, to_url = URL} = Pusher) ->
  run_loop(Pusher#pusher{to = connect_to(URL)});


run_loop(#pusher{from = {Mode, From}, to = To} = Pusher) ->
  case Mode of
    socket -> inet:setopts(From, [{active, once}]);
    _ -> ok
  end,
  receive
    {tcp, From, Bin} ->
      ok = gen_tcp:send(To, Bin),
      run_loop(Pusher);
    {From, {data, Bin}} ->
      ok = gen_tcp:send(To, Bin),
      run_loop(Pusher);
    {tcp_closed, From} ->
      timer:sleep(500),
      run_loop(Pusher#pusher{from = undefined});
    {From, {exit_status, _ExitStatus}} ->
      timer:sleep(500),
      run_loop(Pusher#pusher{from = undefined});
    {tcp_closed, To} ->
      ?D({destination,closed_port}),
      timer:sleep(500),
      run_loop(Pusher#pusher{to = undefined});
    {tcp_closed, From} ->
      ?D({source, closed_port}),
      timer:sleep(500),
      run_loop(Pusher#pusher{from = undefined});
    Else ->
      ?D({"Undefined message", Else}),
      run_loop(Pusher)
  after
    ?TIMEOUT -> 
      ?D("Timeout in pusher"),
      timer:sleep(500),
      run_loop(Pusher#pusher{to = undefined, from = undefined})
  end.
