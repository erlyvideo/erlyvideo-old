%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTMP statistics collector
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
-module(rtmp_stat_collector).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).


%% External API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([out_bytes/2, stats/0]).

start_link(Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).

-record(state, {
  total_bytes,
  stats,
  depth,
  current_speed,
  previous_time,
  current_bytes
}).

-record(entry, {
  time,
  bytes,
  speed
}).


%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

out_bytes(_Client, Bytes) when is_number(Bytes) ->
  ?MODULE ! {out_bytes, Bytes}.

stats() ->
  gen_server:call(?MODULE, stats).


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
  Depth = proplists:get_value(depth, Options, 100),
  Timer = proplists:get_value(timer, Options, 5000),
  timer:send_interval(Timer, flush),
  {ok, #state{
    total_bytes = 0,
    stats = queue:new(),
    depth = Depth,
    current_speed = 0,
    previous_time = erlang:now(),
    current_bytes = 0
  }}.

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
handle_call(stats, _From, #state{stats = Stats} = State) ->
  {reply, dump_stats(Stats), State};

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
handle_info({out_bytes, Bytes}, #state{} = State) ->
  {noreply, append_bytes(Bytes, State)};

handle_info(flush, #state{} = State) ->
  {noreply, flush_bytes(State)};

handle_info(_Info, State) ->
  {stop, {unknown_message, _Info}, State}.

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



append_bytes(Bytes, #state{current_bytes = Current} = State) ->
  State#state{current_bytes = Current + Bytes}.


flush_bytes(#state{current_bytes = Bytes, previous_time = Prev, stats = Stats, 
                   total_bytes = Total, depth = Depth} = State) ->
  Now = erlang:now(),
  Delta = timer:now_diff(Now, Prev),
  Speed = Bytes * 8000 div Delta, % Bytes per millisecond * 8 = kbits / second
  {M,S,_Mu} = Now,
  Time = M*10000000+S,
  Stats1 = queue:in(#entry{time = Time, bytes = Bytes, speed = Speed}, Stats),
  Stats2 = case queue:len(Stats1) of
    Len when Len > Depth -> queue:drop(Stats1);
    _ -> Stats1
  end,
  State#state{current_bytes = 0, previous_time = Now, stats = Stats2, total_bytes = Total + Bytes}.

dump_stats(Stats) ->
  [[{time,Time},{bytes,Bytes},{speed,Speed}] || 
    #entry{time = Time, bytes = Bytes, speed = Speed} <- lists:reverse(queue:to_list(Stats))].


