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
-include("../media/ems_media.hrl").

% -define(assertAlive(Name), ?assertMatch(Pid when is_pid(Pid), whereis(Name))).
% -define(assertDead(Name), ?assertEqual(undefined, whereis(Name))).
-define(assertAlive(Pid), ?assert(lists:member(Pid, processes()))).
-define(assertDead(Pid), ?assertNot(lists:member(Pid, processes()))).

dump_source() ->
  receive
    _Msg -> ok
  after 
    2000 -> ok
  end.


setup_test_stream(Options, Fun) ->
  {spawn, {setup,
  fun() ->
    log4erl:change_log_level(error),
    {ok, _Pid} = ems_media:start_link(test_media, Options)
  end,
  fun({ok, Pid}) ->
    Pid ! stop,
    ems_test_helper:wait4(Pid),
    log4erl:change_log_level(debug)
  end,
  fun({ok, Pid}) ->
    [fun() -> Fun(Pid) end]
  end
  }}.


source_timeout_test_() ->
  setup_test_stream([{source_timeout, 3}], fun(Media) ->
    Source = spawn_link(fun dump_source/0),
    ems_media:set_source(Media, Source),
    
    
    ?assertAlive(Media),
    timer:sleep(60),
    ?assertAlive(Media),

    Source ! stop,               
    timer:sleep(2),
    
    Source1 = spawn_link(fun dump_source/0),
    ems_media:set_source(Media, Source1),
    timer:sleep(60),
    ?assertAlive(Media),
    

    Source1 ! stop,
    timer:sleep(2),
    
    ?assertAlive(Media),
    timer:sleep(6),
    ?assertDead(Media)
  end).

source_false_timeout_test_() ->
  setup_test_stream([{source_timeout, false}], fun(Media) ->
    Source = spawn_link(fun dump_source/0),
    ems_media:set_source(Media, Source),

    timer:sleep(60),
    ?assertAlive(Media),

    Source ! stop,               
    timer:sleep(60),
    ?assertAlive(Media),
    
    Source ! stop
  end).
  
clients_timeout_test_() ->
  setup_test_stream([{clients_timeout, 30}], fun(Media) ->
    ems_media:subscribe(Media, []),

    ?assertAlive(Media),
    timer:sleep(60),
    ?assertAlive(Media),

    ems_media:unsubscribe(Media),
    timer:sleep(2),

    ems_media:subscribe(Media, []),
    timer:sleep(60),
    ?assertAlive(Media),


    ems_media:unsubscribe(Media),
    timer:sleep(2),

    ?assertAlive(Media),
    timer:sleep(60),
    ?assertDead(Media)
  end).

clients_false_timeout_test_() ->
  setup_test_stream([{clients_timeout, false}], fun(Media) ->
    ems_media:subscribe(Media, []),

    timer:sleep(60),
    ?assertAlive(Media),

    ems_media:unsubscribe(Media),
    timer:sleep(60),
    ?assertAlive(Media)
  end).
  

false_timeouts_test_() ->
  setup_test_stream([{clients_timeout, false},{source_timeout, false}], fun(Media) ->
    Source = spawn_link(fun dump_source/0),
    ems_media:set_source(Media, Source),
    ems_media:subscribe(Media, []),

    timer:sleep(60),
    ?assertAlive(Media),
    
    Source ! stop,
    ems_media:unsubscribe(Media),
    
    timer:sleep(60),
    ?assertAlive(Media)
  end).
  

  