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
  case validate_config() of
    true -> rpc:call(Node, ems, reload, []);
    false -> error
  end;

handle(Node, restart, _Args) ->
  case validate_config() of
    true -> rpc:call(Node, ems, restart, []);
    false -> error
  end.
  
  
validate_config() ->
  case file:path_consult(["priv", "/etc/erlyvideo"], "erlyvideo.conf") of
    {ok, _Env, Path} ->
      io:format("Loading config from valid file ~p~n", [Path]),
      true;
    {error, enoent} ->
      io:format("No config found~n"),
      false;
    {error, Reason} ->
      io:format("Couldn't parse file: ~p~n", [Reason]),
      false
  end.

stop() ->
  ok.
