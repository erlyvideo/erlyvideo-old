%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Generic cache server with timeout
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2010 Max Lapshin
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(gen_cache).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).

-define(STREAM_TIME, 10000).
-define(TIMEOUT, 60000).

%% External API
-export([start_link/0, start_link/1]).

-export([set/2, set/3, set/4, get/1, get/2, get/3, fetch/2, fetch/3, fetch/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(gen_cache, {
  cache = [],
  timeout = ?TIMEOUT
}).


start_link() ->
  start_link([{local, ?MODULE},{timeout,?TIMEOUT}]).

start_link(Options) ->
  case proplists:get_value(local, Options) of
    undefined -> gen_server:start_link(?MODULE, [Options], []);
    Name -> gen_server:start_link({local, Name}, ?MODULE, [Options], [])
  end.


set(Key, Value) ->
  set(?MODULE, Key, Value, ?TIMEOUT).

set(Cache, Key, Value) ->
  set(Cache, Key, Value, ?TIMEOUT).
  
set(Cache, Key, Value, Timeout) ->
  gen_server:call(Cache, {set, Key, Value, Timeout}).



get(Key) ->
  get(?MODULE, Key, undefined).
  
get(Cache, Key) ->
  get(Cache, Key, undefined).
  
get(Cache, Key, Default) ->
  case gen_server:call(Cache, {get, Key}) of
    {ok, Value} -> Value;
    undefined when is_function(Default) -> Default();
    undefined -> Default
  end.


fetch(Key, Default) ->
  fetch(?MODULE, Key, Default, ?TIMEOUT).

fetch(Cache, Key, Default) ->
  fetch(Cache, Key, Default, ?TIMEOUT).

fetch(Cache, Key, Default, Timeout) ->
  get(Cache, Key, fun() ->
    Value = if
      is_function(Default) -> Default();
      true -> Default
    end,
    gen_server:call(Cache, {set, Key, Value, Timeout})
  end).
  

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------


init([Options]) ->
  Timeout = proplists:get_value(timeout, Options, ?TIMEOUT),
  {ok, #gen_cache{timeout = Timeout}}.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------

handle_call({set, Key, Value, LiveTimeout}, _From, #gen_cache{cache = Cache,timeout = Timeout} = State) ->
  {Now, _} = erlang:statistics(wall_clock),
  Cache1 = lists:keystore(Key, 1, Cache, {Key, Value, Now+LiveTimeout}),
  {reply, Value, State#gen_cache{cache = Cache1}, Timeout};

handle_call({get, Key}, _From, #gen_cache{cache = Cache, timeout = Timeout} = State) ->
  Value = case lists:keyfind(Key, 1, Cache) of
    false -> undefined;
    {_, V, _} -> {ok, V}
  end,
  {reply, Value, State, Timeout};
  
handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info(timeout, #gen_cache{cache = Cache, timeout = Timeout} = Server) ->
  {Now, _} = erlang:statistics(wall_clock),
  Cache1 = lists:filter(fun({_,_,Time}) when Time < Now -> false;
                           (_) -> true end, Cache),
  {noreply, Server#gen_cache{cache = Cache1}, Timeout};


handle_info(_Info, #gen_cache{} = State) ->
  io:format("Unknown message: ~p~n", [_Info]),
  {noreply, State}.



%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").


start_registered_test_() ->
  {spawn,{setup,
  fun() -> gen_cache:start_link() end,
  fun({ok, Pid}) -> erlang:exit(Pid, kill) end,
  ?_assert(lists:member(gen_cache, erlang:registered()))
  }}.

start_custom_registered_test_() ->
  {spawn,{setup,
  fun() -> gen_cache:start_link([{local, some_cache}]) end,
  fun({ok, Pid}) -> erlang:exit(Pid, kill) end,
  ?_assert(lists:member(some_cache, erlang:registered()))
  }}.


set2_test_() ->
  {spawn,{setup,
  fun() -> gen_cache:start_link() end,
  fun({ok, Pid}) -> erlang:exit(Pid, kill) end,
  ?_assertEqual(v, gen_cache:set(k, v))
  }}.

set3_test_() ->
  {spawn,{setup,
  fun() -> gen_cache:start_link([{local,some_cache1}]) end,
  fun({ok, Pid}) -> erlang:exit(Pid, kill) end,
  ?_assertEqual(v, gen_cache:set(some_cache1, k, v))
  }}.

set4_test_() ->
  {spawn,{setup,
  fun() -> gen_cache:start_link([{local,some_cache2}]) end,
  fun({ok, Pid}) -> erlang:exit(Pid, kill) end,
  ?_assertEqual(v, gen_cache:set(some_cache2, k, v, 1))
  }}.


set_and_get3_test_() ->
  {spawn,{setup,
  fun() -> 
    {ok, Pid} = gen_cache:start_link([{local,some_cache3}]),
    gen_cache:set(some_cache3, k, v, 100),
    {ok, Pid}
  end,
  fun({ok, Pid}) -> erlang:exit(Pid, kill) end,
  ?_assertEqual(v, gen_cache:get(some_cache3, k, undefined))
  }}.

nonset_and_get3_test_() ->
  {spawn,{setup,
  fun() -> gen_cache:start_link([{local,some_cache4}]) end,
  fun({ok, Pid}) -> erlang:exit(Pid, kill) end,
  ?_assertEqual(v1, gen_cache:get(some_cache4, k, v1))
  }}.

set_expire_and_get3_test_() ->
  {spawn,{setup,
  fun() -> 
    {ok, Pid} = gen_cache:start_link([{local,some_cache5}]),
    gen_cache:set(some_cache5, k, v2, 1),
    timer:sleep(2),
    {ok, Pid}
  end,
  fun({ok, Pid}) -> erlang:exit(Pid, kill) end,
  ?_assertEqual(v2, gen_cache:get(some_cache5, k)) % Value must still return, because sweeper hasn't swept value
  }}.

set_real_expire_and_get3_test_() ->
  {spawn,{setup,
  fun() -> 
    {ok, Pid} = gen_cache:start_link([{local,some_cache6},{timeout,1}]),
    gen_cache:set(some_cache6, k, v2, 1),
    timer:sleep(2),
    some_cache6 ! timeout,
    {ok, Pid}
  end,
  fun({ok, Pid}) -> erlang:exit(Pid, kill) end,
  ?_assertEqual(undefined, gen_cache:get(some_cache6, k))
  }}.


nonset_and_fetch4_test_() ->
  {spawn,{setup,
  fun() -> gen_cache:start_link([{local,some_cache7}]) end,
  fun({ok, Pid}) -> erlang:exit(Pid, kill) end,
  fun() ->
    ?assertEqual(v, gen_cache:fetch(some_cache7, k, v)),
    ?assertEqual(v, gen_cache:get(some_cache7, k))
  end
  }}.

nonset_and_fetch4_fun_test_() ->
  {spawn,{setup,
  fun() -> gen_cache:start_link([{local,some_cache7}]) end,
  fun({ok, Pid}) -> erlang:exit(Pid, kill) end,
  fun() ->
    ?assertEqual(v, gen_cache:fetch(some_cache7, k, fun() -> v end)),
    ?assertEqual(v, gen_cache:get(some_cache7, k))
  end
  }}.











