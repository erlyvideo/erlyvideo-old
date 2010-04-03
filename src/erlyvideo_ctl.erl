-module(erlyvideo_ctl).
-author('Max Lapshin <max@maxidoors.ru>').

-export([start/0, stop/0]).

start() ->
  [NodeStr, Command | Args] = init:get_plain_arguments(),
  Node = list_to_atom(NodeStr),
  io:format("Calling ~p:  ~s(~p)~n", [Node, Command, Args]),
  handle(Node, list_to_atom(Command), Args),
  halt(0).

handle(Node, stop, _Args) ->
  rpc:call(Node, init, stop, []);
  
handle(Node, reload, _Args) ->
  rpc:call(Node, ems, reload, []);

handle(Node, restart, _Args) ->
  rpc:call(Node, ems, restart, []).
  
  


stop() ->
  ok.
