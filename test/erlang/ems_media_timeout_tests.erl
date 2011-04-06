%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2010 Max Lapshin
%%% @doc        Client of erlyvideo license server
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
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
-module(ems_media_timeout_tests).
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

wait4(Pid) ->
  erlang:monitor(process, Pid),
  receive
    {'DOWN', _, _, Pid, Reason} -> Reason
  end.

source_timeout_test_() ->
  {spawn, {setup,
    fun() -> 
      log4erl:change_log_level(error),
      (catch erlang:exit(whereis(ems_media_test), kill)),
      {ok, Pid} = ems_media:start_link(test_media, [{source_timeout, 3}]),
      erlang:register(ems_media_test, Pid),
      {ok, Pid} 
    end,
    fun({ok, Pid}) ->
      Pid ! stop,
      wait4(Pid),
      log4erl:change_log_level(debug)
    end,
    [fun() ->
      Source = spawn_link(fun dump_source/0),
      ems_media:set_source(whereis(ems_media_test), Source),
      
      
      ?assertAlive(ems_media_test),
      timer:sleep(60),
      ?assertAlive(ems_media_test),

      Source ! stop,               
      timer:sleep(2),
      
      Source1 = spawn_link(fun dump_source/0),
      ems_media:set_source(whereis(ems_media_test), Source1),
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
      log4erl:change_log_level(error),
      (catch erlang:exit(whereis(ems_media_test), kill)),
      {ok, Pid} = ems_media:start_link(test_media, [{source_timeout, false}]),
      erlang:register(ems_media_test, Pid),
      {ok, Pid} 
    end,
    fun({ok, Pid}) ->
      Pid ! stop,
      wait4(Pid),
      log4erl:change_log_level(debug)
    end,
    [fun() ->
      Source = spawn_link(fun dump_source/0),
      ems_media:set_source(whereis(ems_media_test), Source),

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
      log4erl:change_log_level(error),
      (catch erlang:exit(whereis(ems_media_test), kill)),
      {ok, Pid} = ems_media:start_link(test_media, [{clients_timeout, 30}]),
      erlang:register(ems_media_test, Pid),
      {ok, Pid} 
    end,
    fun({ok, Pid}) -> 
      Pid ! stop,
      wait4(Pid),
      log4erl:change_log_level(debug)
    end,
    [fun() ->
      ems_media:subscribe(whereis(ems_media_test), []),

      ?assertAlive(ems_media_test),
      timer:sleep(60),
      ?assertAlive(ems_media_test),

      ems_media:unsubscribe(whereis(ems_media_test)),
      timer:sleep(2),

      ems_media:subscribe(whereis(ems_media_test), []),
      timer:sleep(60),
      ?assertAlive(ems_media_test),


      ems_media:unsubscribe(whereis(ems_media_test)),
      timer:sleep(2),

      ?assertAlive(ems_media_test),
      timer:sleep(60),
      ?assertDead(ems_media_test)
    end]
  }}.

clients_false_timeout_test_() ->
  {spawn, {setup,
    fun() -> 
      log4erl:change_log_level(error),
      (catch erlang:exit(whereis(ems_media_test), kill)),
      {ok, Pid} = ems_media:start_link(test_media, [{clients_timeout, false}]),
      erlang:register(ems_media_test, Pid),
      {ok, Pid} 
    end,
    fun({ok, Pid}) ->
      Pid ! stop,
      wait4(Pid),
      log4erl:change_log_level(debug)
    end,
    [fun() ->
      ems_media:subscribe(whereis(ems_media_test), []),

      timer:sleep(60),
      ?assertAlive(ems_media_test),

      ems_media:unsubscribe(whereis(ems_media_test)),
      timer:sleep(60),
      ?assertAlive(ems_media_test)
    end]
  }}.


false_timeouts_test_() ->
  {spawn, {setup,
    fun() -> 
      log4erl:change_log_level(error),
      (catch erlang:exit(whereis(ems_media_test), kill)),
      {ok, Pid} = ems_media:start_link(test_media, [{clients_timeout, false},{source_timeout, false}]),
      erlang:register(ems_media_test, Pid),
      {ok, Pid} 
    end,
    fun({ok, Pid}) ->
      Pid ! stop,
      wait4(Pid),
      log4erl:change_log_level(debug)
    end,
    [fun() ->
      Source = spawn_link(fun dump_source/0),
      ems_media:set_source(whereis(ems_media_test), Source),
      ems_media:subscribe(whereis(ems_media_test), []),

      timer:sleep(60),
      ?assertAlive(ems_media_test),
      
      Source ! stop,
      ems_media:unsubscribe(whereis(ems_media_test)),
      
      timer:sleep(60),
      ?assertAlive(ems_media_test)
    end]
  }}.
  

  