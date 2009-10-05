-module(ems_fsm).
-author('max@maxidoors.ru').
-include("../include/ems.hrl").
-behaviour(gen_fsm).

-export([play/2]).
-behaviour(gen_fsm).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([ready/2]).

play(URL, StreamId) ->
  gen_fsm:start_link(?MODULE, [URL], []).
  
  
-record(mpeg_ts_player, {

}).
  
init(URL) ->
  % Make HTTP request here.
  {ok, ready, #mpeg_ts_player{}}.
  
ready({start}, State) ->
  {next_state, ready, State};
  
ready({pause}, State) ->
  {next_state, ready, State};

ready({resume}, State) ->
  {next_state, ready, State};

ready({seek, Timestamp}, State) ->
  {next_state, ready, State};







handle_event(Event, StateName, StateData) ->
  ?D({"Unknown event in mpeg ts player", Event, StateName}),
    {stop, {StateName, undefined_event, Event}, StateData}.


handle_sync_event(Event, _From, StateName, StateData) ->
     io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, got_sync_request2]),
    {stop, {StateName, undefined_event, Event}, StateData}.

handle_info({tcp_closed, _Socket}, _StateName, StateData) ->
    error_logger:info_msg("~p MPEG TS player lost connection.\n", [self()]),
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
  ?D({"Unknown info in player", _Info, StateName}),
  {noreply, StateName, StateData}.

terminate(_Reason, _StateName, _State) ->
  ?D("MPEG TS player exit"),
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
