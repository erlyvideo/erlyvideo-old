-module(ems_media_tests).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("eunit/include/eunit.hrl").
-include("../../include/ems_media.hrl").

-define(assertAlive(Name), ?assertMatch(Pid when is_pid(Pid), whereis(Name))).
-define(assertDead(Name), ?assertEqual(undefined, whereis(Name))).

dump_source() ->
  receive
    _Msg -> ok
  after 
    2000 -> ok
  end.

source_timeout_test_() ->
  {spawn, {setup,
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
  }}.

source_false_timeout_test_() ->
  {spawn, {setup,
    fun() -> 
      (catch erlang:exit(whereis(ems_media_test), kill)),
      {ok, Pid} = ems_media:start_link(test_media, [{source_timeout, false}]),
      erlang:register(ems_media_test, Pid),
      {ok, Pid} 
    end,
    fun({ok, Pid}) -> erlang:exit(Pid, shutdown) end,
    [fun() ->
      Source = spawn_link(fun dump_source/0),
      ems_media:set_source(ems_media_test, Source),

      timer:sleep(60),
      ?assertAlive(ems_media_test),

      Source ! stop,               
      timer:sleep(60),
      ?assertAlive(ems_media_test),
      
      Source ! stop
    end]
  }}.
  
clients_timeout_test_() ->
  {spawn, {setup,
    fun() -> 
      (catch erlang:exit(whereis(ems_media_test), kill)),
      {ok, Pid} = ems_media:start_link(test_media, [{clients_timeout, 3}]),
      erlang:register(ems_media_test, Pid),
      {ok, Pid} 
    end,
    fun({ok, Pid}) -> erlang:exit(Pid, shutdown) end,
    [fun() ->
      ems_media:subscribe(ems_media_test, [{start, 20}]),

      ?assertAlive(ems_media_test),
      timer:sleep(60),
      ?assertAlive(ems_media_test),

      ems_media:unsubscribe(ems_media_test),
      timer:sleep(2),

      ems_media:subscribe(ems_media_test, [{start, 20}]),
      timer:sleep(60),
      ?assertAlive(ems_media_test),


      ems_media:unsubscribe(ems_media_test),
      timer:sleep(2),

      ?assertAlive(ems_media_test),
      timer:sleep(6),
      ?assertDead(ems_media_test)
    end]
  }}.

clients_false_timeout_test_() ->
  {spawn, {setup,
    fun() -> 
      (catch erlang:exit(whereis(ems_media_test), kill)),
      {ok, Pid} = ems_media:start_link(test_media, [{clients_timeout, false}]),
      erlang:register(ems_media_test, Pid),
      {ok, Pid} 
    end,
    fun({ok, Pid}) -> erlang:exit(Pid, shutdown) end,
    [fun() ->
      ems_media:subscribe(ems_media_test, [{start, 20}]),

      timer:sleep(60),
      ?assertAlive(ems_media_test),

      ems_media:unsubscribe(ems_media_test),
      timer:sleep(60),
      ?assertAlive(ems_media_test)
    end]
  }}.


false_timeouts_test_() ->
  {spawn, {setup,
    fun() -> 
      (catch erlang:exit(whereis(ems_media_test), kill)),
      {ok, Pid} = ems_media:start_link(test_media, [{clients_timeout, false},{source_timeout, false}]),
      erlang:register(ems_media_test, Pid),
      {ok, Pid} 
    end,
    fun({ok, Pid}) -> erlang:exit(Pid, shutdown) end,
    [fun() ->
      Source = spawn_link(fun dump_source/0),
      ems_media:set_source(ems_media_test, Source),
      ems_media:subscribe(ems_media_test, [{start, 20}]),

      timer:sleep(60),
      ?assertAlive(ems_media_test),
      
      Source ! stop,
      ems_media:unsubscribe(ems_media_test),
      
      timer:sleep(60),
      ?assertAlive(ems_media_test)
    end]
  }}.
  

file_media_test_() ->
  {spawn, [
    fun() ->
      {ok, Media, _} = ems_media:init([file_media, [{url, <<"video.mp4">>}, {host, default}]]),
      ?assertEqual(false, Media#ems_media.source_timeout),
      ?assertEqual(file_media:default_timeout(), Media#ems_media.clients_timeout)
    end,
    fun() ->
      {ok, Media, _} = ems_media:init([file_media, [{url, <<"video.mp4">>}, {host, default}, {clients_timeout, false}]]),
      ?assertEqual(false, Media#ems_media.source_timeout),
      ?assertEqual(false, Media#ems_media.clients_timeout)
    end
  ]}.

live_media_test_() ->
  {spawn, [
    fun() ->
      {ok, Media, _} = ems_media:init([live_media, []]),
      ?assertEqual(live_media:default_timeout(), Media#ems_media.source_timeout),
      ?assertEqual(false, Media#ems_media.clients_timeout)
    end,
    fun() ->
      {ok, Media, _} = ems_media:init([live_media, [{source_timeout, 150}]]),
      ?assertEqual(150, Media#ems_media.source_timeout),
      ?assertEqual(false, Media#ems_media.clients_timeout)
    end
  ]}.

mpegts_file_media_test_() ->
  {spawn, [
    fun() ->
      {ok, Media, _} = ems_media:init([mpegts_file_media, [{url, <<"video.ts">>}, {host, default}, {make_request, false}]]),
      ?assertEqual(1, Media#ems_media.source_timeout),
      ?assertEqual(file_media:default_timeout(), Media#ems_media.clients_timeout)
    end
  ]}.

mpegts_media_test_() ->
  {spawn, [
    fun() ->
      {ok, Media, _} = ems_media:init([mpegts_media, [{url, <<"video.ts">>}, {host, default}, {type, mpegts_passive}]]),
      ?assertEqual(false, Media#ems_media.clients_timeout)
    end
  ]}.

rtsp_media_test_() ->
  {spawn, [
    fun() ->
      {ok, Media, _} = ems_media:init([rtsp_media, [{url, <<"rtsp://localhost/">>}, {host, default}]]),
      ?assertEqual(live_media:default_timeout(), Media#ems_media.clients_timeout),
      ?assertEqual(live_media:default_timeout(), Media#ems_media.source_timeout)
    end
  ]}.

  