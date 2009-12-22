-module(ems_vhosts).
-author(max@maxidoors.ru).
-include("../include/ems.hrl").
-export([start/0, stop/0]).


start() ->
  vhosts = ets:new(vhosts, [set, named_table, public]),
  case application:get_env(?APPLICATION, vhosts) of
    {ok, Hosts} when is_list(Hosts) -> init_vhosts(Hosts);
    _ -> ok
  end.
  

init_vhosts([]) ->
  ok;

init_vhosts([{Name, Host} | Hosts]) ->
  lists:foreach(fun({Key, Value}) ->
    ets:insert(vhosts, {{Name, Key}, Value})
  end, Host),
  lists:foreach(fun(Hostname) ->
    true = ets:insert(vhosts, {Hostname, Name})
  end, proplists:get_value(hostname, Host, [])),
  init_vhosts(Hosts).


stop() ->
  ets:delete(vhosts).
