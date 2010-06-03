-module(ems_media_tests).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("eunit/include/eunit.hrl").

-define(assertAlive(Name), ?assertMatch(Pid when is_pid(Pid), whereis(Name))).
-define(assertDead(Name), ?assertEqual(undefined, whereis(Name))).

dump_source() ->
  receive
    _Msg -> ok
  after 
    2000 -> ok
  end.

source_timeout_test_() ->
  {setup,
    fun() -> 
      (catch erlang:exit(whereis(ems_media_test), kill)),
      {ok, Pid} = ems_media:start_link(test_media, [{source_timeout, 3}]),
      erlang:register(ems_media_test, Pid),
      {ok, Pid} 
    end,
    fun({ok, Pid}) -> erlang:exit(Pid, shutdown) end,
    [fun() ->
      Source = spawn_link(fun dump_source/0),
      ems_media:set_source(ems_media_test, Source),
      
      
      ?assertAlive(ems_media_test),
      timer:sleep(60),
      ?assertAlive(ems_media_test),

      Source ! stop,               
      timer:sleep(2),
      
      Source1 = spawn_link(fun dump_source/0),
      ems_media:set_source(ems_media_test, Source1),
      timer:sleep(60),
      ?assertAlive(ems_media_test),
      

      Source1 ! stop,
      timer:sleep(2),
      
      ?assertAlive(ems_media_test),
      timer:sleep(6),
      ?assertDead(ems_media_test)
    end]
  }.
  
clients_timeout_test() ->
  ?assertEqual(a, a).
  

  