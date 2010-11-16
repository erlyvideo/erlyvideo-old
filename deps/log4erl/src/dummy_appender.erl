-module(dummy_appender).

-include("../include/log4erl.hrl").

-behaviour(gen_event).
%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

init(_Args) ->
    io:format("Initializing dummy_appender with args =  ~p~n",[_Args]),
    {ok, #state{}}.

handle_event(_Event, State) ->
    io:format("dummy_appender received event ~p~n",[_Event]),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

