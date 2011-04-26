%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTMP socket monitor. Shutdown slow clients
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information.
%%% @end
%%%
%%% This file is part of erlang-rtmp.
%%% 
%%% erlang-rtmp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtmp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtmp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(ems_network_lag_monitor).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../log.hrl").

-define(TIMEOUT, 1000).
-behaviour(gen_server).

-export([start_link/0, start_link/1, watch/1, set_threshold/1]).

-record(network_monitor, {
  threshold,
  timeout,
  clients = []
}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


watch(Pid) -> 
  gen_server:cast(?MODULE, {watch, Pid}).
  
set_threshold(Threshold) ->
  gen_server:call(?MODULE, {set_threshold, Threshold}).

%%--------------------------------------------------------------------
%% @spec () -> {ok, Pid} | {error, Reason}
%%
%% @doc Called by a supervisor to start the listening process.
%% @end
%%----------------------------------------------------------------------
start_link() -> start_link([]).
start_link(Options) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).

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
  % ?D({start,Options}),
  Timeout = proplists:get_value(timeout,Options,?TIMEOUT),
  Threshold = proplists:get_value(threshold,Options,5000),
  timer:send_interval(Timeout, timeout),
  {ok, #network_monitor{timeout = Timeout, threshold = Threshold}}.


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
handle_call({set_threshold, Threshold}, _From, State) ->
  {reply, ok, State#network_monitor{threshold = Threshold}};

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
handle_cast({watch, Pid}, #network_monitor{clients = Clients} = State) ->
  erlang:monitor(process, Pid),
  {noreply, State#network_monitor{clients = [Pid|Clients]}};
  
handle_cast(_Msg, State) ->
  {stop, {unhandled_cast, _Msg}, State}.

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
% 

handle_info(timeout, #network_monitor{threshold = Threshold, clients = Clients} = State) ->
  {BrutalKill, Alive} = lists:partition(fun(Pid) ->
    % ?D({Pid,process_info(Pid, message_queue_len),Threshold}),
    try process_info(Pid, message_queue_len) of
      {_, Length} when is_number(Length) andalso is_number(Threshold) andalso Length > Threshold -> true;
      _Length -> false
    catch
      _Class:_Error -> false
    end
  end, Clients),
  % ?D({killing,BrutalKill,Alive}),
  report_brutal_kill(BrutalKill),
  brutal_kill(BrutalKill),
  {noreply, State#network_monitor{clients = Alive}};

handle_info({'DOWN', _, process, Pid, _Reason}, #network_monitor{clients = Clients} = State) ->
  % ?D({deleting,Pid}),
  {noreply, State#network_monitor{clients = lists:delete(Pid, Clients)}};

handle_info(Message, State) ->
  {stop, {unhandled, Message}, State}.


report_brutal_kill([]) -> ok;
report_brutal_kill(BrutalKill) -> error_logger:error_msg("[NETWORK_MON] Brutal kill due to lag: ~p~n", [BrutalKill]).

brutal_kill([]) -> ok;
brutal_kill(BrutalKill) -> [erlang:exit(Pid,kill) || Pid <- BrutalKill].


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



-include_lib("eunit/include/eunit.hrl").

-define(assertAlive(Pid), ?assert(lists:member(Pid, processes()))).
-define(assertDead(Pid), ?assertNot(lists:member(Pid, processes()))).


kill_lagger_test_() ->
  {spawn, {setup,
    fun() -> 
      (catch erlang:exit(whereis(ems_network_lag_test), kill)),
      gen_server:start_link({local, ems_network_lag_test}, ?MODULE, [[{timeout,10},{threshold,20}]], [])
    end,
    fun({ok, Pid}) -> erlang:exit(Pid, shutdown) end,
    [fun() ->
      Lagger = spawn_link(fun() ->
        receive
          stop -> ok
        end  
      end),
      timer:sleep(60),
      ?assertAlive(Lagger),

      gen_server:cast(ems_network_lag_test, {watch, Lagger}),
      timer:sleep(60),
      
      ?assertAlive(Lagger),
      [Lagger ! test || _N <- lists:seq(1,100)],
      ems_network_lag_test ! timeout,
      timer:sleep(60),
      
      ?assertDead(Lagger)
    end]
  }}.

