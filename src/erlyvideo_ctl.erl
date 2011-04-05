%%% @hidden
%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        control from shell
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
-module(erlyvideo_ctl).
-author('Max Lapshin <max@maxidoors.ru>').

-export([start/0, stop/0, main/1]).

main(Args) ->
  erlyvideo:main(Args).

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
