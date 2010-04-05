-module(ems_vhosts).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/ems.hrl").
-export([start/0, stop/0]).


start() ->
  case ets:info(vhosts) of
    undefined -> vhosts = ets:new(vhosts, [set, named_table, public]);
    _ -> ets:delete_all_objects(vhosts)
  end,
  case application:get_env(erlyvideo, vhosts) of
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
  (catch ets:delete(vhosts)).
