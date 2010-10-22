%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        RTMP benchmarking module. 
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information.
%%% @end
%%%
%%% This file is part of erlang-rtmp.
%%% 
%%% erlang-rtmp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtmp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtmp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtmp_bench).
-author('Max Lapshin <max@maxidoors.ru>').


-include("../include/rtmp.hrl").

-record(reader, {
  time_start,
  dts_start,
  last_dts,
  socket,
  delta = 0,
  path,
  count = 1,
  debug,
  seeked = false
}).

-record(spawner, {
  url,
  count,
  debug,
  server,
  port,
  app,
  path
}).


-export([start_spawner/1, init/3]).

init(URL, Count, Debug) ->
  {ok, Re1} = re:compile("([^:]+)://([^/]+):(\\d+)/([^/]+)/(.*)"),
  {ok, Re2} = re:compile("([^:]+)://([^/]+)/([^/]+)/(.*)"),
  process_flag(trap_exit, true),
	case re:run(URL, Re1, [{capture, all, list}]) of
	  {match, [_, _Protocol, Server, PortS, App, Path]} ->
			Port = list_to_integer(PortS);
		_ ->
			{match, [_, _Protocol, Server, App, Path]} = re:run(URL, Re2, [{capture, all, list}]),
			Port = 1935
	end,
	io:format("~p:~p ~p ~p~n", [Server, Port, App, Path]),
  #spawner{server = Server, port = Port, app = list_to_binary(App), path = Path, count = Count, debug = Debug}.


start_spawner(Spawner) ->
  start_spawner(Spawner, 0).

start_spawner(#spawner{count = Count} = Spawner, Number) when Number < Count ->
  io:format("Starting client ~p~n", [Number+1]),
  spawn_link(fun() -> init_rtmp_client(Spawner) end),
  receive
    {'EXIT', _Pid, _Reason} ->
      NewCount = flush_exits(Number),
      start_spawner(Spawner, NewCount)
  after
    500 ->
      start_spawner(Spawner, Number + 1)
  end;

start_spawner(#spawner{count = Count} = Spawner, Count) ->
  receive
    {'EXIT', _Pid, _Reason} ->
      io:format("Dead client ~p~n", [_Reason]),
      NewCount = flush_exits(Count - 1),
      start_spawner(Spawner, NewCount);
    Else ->
      io:format("Spawner message: ~p~n", [Else]),
      start_spawner(Spawner, Count)
  end.

flush_exits(Count) ->
  receive
    {'EXIT', _Pid, _Reason} -> flush_exits(Count - 1)
  after
    0 -> Count
  end.
  
init_rtmp_client(#spawner{server = Server, port = Port, debug = Debug, app = App, path = Path}) ->
  {ok, Socket} = gen_tcp:connect(Server, Port, [binary, {active, false}, {packet, raw}]),
  % io:format("Socket opened to ~s~n", [Server]),
  {ok, RTMP} = rtmp_socket:connect(Socket),
  rtmp_socket:setopts(RTMP, [{debug,Debug}]),
  io:format("Connected to ~s~n", [Server]),
  rtmp_client(RTMP, App, Path, Debug).
  
rtmp_client(RTMP, App, Path, Debug) ->
  receive 
    {rtmp, RTMP, connected} ->
      rtmp_socket:setopts(RTMP, [{active, true}]),
      play(RTMP, App, Path, Debug);
    Else ->
      io:format("Client message: ~p (~p)~n", [Else, RTMP]),
      rtmp_client(RTMP, App, Path, Debug)
  after
    10000 ->
      io:format("Client timeout~n"),
      ok
  end.

play(RTMP, App, Path, Debug) ->
  rtmp_lib:connect(RTMP, [{app, App}, {tcUrl, <<"rtmp://localhost/live/a">>}]),
  Stream = rtmp_lib:createStream(RTMP),
  rtmp_lib:play(RTMP, Stream, Path),
  io:format("Playing ~s~n", [Path]),
  read_frame(#reader{socket = RTMP, path = Path, debug = Debug}).

% read_frame(#reader{count = Count, last_dts = DTS} = Reader) when is_number(DTS) andalso Count rem 10000 == 0->
%   io:format("time: ~p~n", [round(DTS/1000)]),
%   read_frame(Reader#reader{count = Count+1});
% 
read_frame(#reader{count = Count, delta = Delta, last_dts = DTS} = Reader) when Count rem 1000 == 0->
  % {Time, _} = erlang:statistics(wall_clock),
  % io:format("start: ~p/~p, current: ~p/~p, lag: ~p/~p = ~p~n", [Reader#reader.time_start, Reader#reader.dts_start, Time, DTS, 
  %   Time - Reader#reader.time_start, DTS - Reader#reader.dts_start, Delta]),
  case Delta of
    _ when Delta > 0 ->
      io:format("pid ~p, time = ~ps, lag = ~pms~n", [self(), round(DTS/1000), Delta]);
    _ ->
      io:format("pid ~p, time = ~ps~n", [self(), round(DTS/1000)])
  end,
  read_frame(Reader#reader{count = Count+1});

read_frame(#reader{socket = RTMP, count = Count} = Reader) ->
  receive
    {rtmp, RTMP, #rtmp_message{type = Type} = Message} when Type == audio orelse Type == video ->
      Reader1 = store_time(Reader, Message),
      warn_delta(Reader1),
      read_frame(Reader1#reader{count = Count+1});
    {rtmp, RTMP, #rtmp_message{} = _Message} ->
      read_frame(Reader#reader{count = Count+1});
    {rtmp, RTMP, disconnect} -> ok;
    Else -> 
      io:format("Unknown message ~p~n", [Else])
  after
    10000 -> io:format("Timeout in reading~n")
  end.
        
store_time(#reader{dts_start = undefined} = Reader, #rtmp_message{timestamp = DTS} = _Message) when DTS > 0 ->
  {Time, _} = erlang:statistics(wall_clock),
  Reader#reader{dts_start = DTS, last_dts = DTS, time_start = Time, delta = 0};
  
store_time(#reader{dts_start = StartDTS, time_start = TimeStart} = Reader, #rtmp_message{timestamp = DTS} = _Message) when DTS > 0 ->
  {Time, _} = erlang:statistics(wall_clock),
  Delta = (Time - TimeStart) - round(DTS - StartDTS),
  Reader#reader{last_dts = DTS, delta = Delta};
  
store_time(Reader, _Message) -> % We ignore decoder config
  Reader.
  
warn_delta(#reader{delta = Delta}) when is_number(Delta) andalso Delta > 1000 ->
  % io:format("Warning, delta: ~p~n", [Delta]),
  ok;
warn_delta(_) ->
  ok.
