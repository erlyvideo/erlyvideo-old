-module(console_appender).

-include("../include/log4erl.hrl").

-behaviour(gen_event).
%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

init({Level}) ->
    init({Level, ?DEFAULT_FORMAT});
init({Level, Format} = _Args) ->
    ?LOG2("Initializing console_appender with args =  ~p~n",[_Args]),
    {ok, Toks} = log_formatter:parse(Format),
    ?LOG2("Tokens received is ~p",[Toks]),
    State = #console_appender{level = Level, format = Toks},
    ?LOG2("State is ~p",[State]),
    {ok, State}.

handle_event({change_level, Level}, State) ->
    State2 = State#console_appender{level = Level},
    ?LOG2("Changed level to ~p~n",[Level]),
    {ok, State2};
handle_event({log,LLog}, State) ->
    ?LOG2("handl_event:log = ~p~n",[LLog]),
    do_log(LLog, State),
    {ok, State}.

handle_call({change_format, Format}, State) ->
    ?LOG2("Old State in console_appender is ~p~n",[State]),
    {ok, Tokens} = log_formatter:parse(Format),
    ?LOG2("Adding format of ~p~n",[Tokens]),
    State1 = State#console_appender{format=Tokens},
    {ok, ok, State1};
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_log(#log{level = L} = Log,#console_appender{level=Level, format=Format}) ->
    ToLog = log4erl_utils:to_log(L, Level),
    case ToLog of
	true ->
	    M = log_formatter:format(Log, Format),
	    ?LOG2("console_appender result message is ~s~n",[M]),
	    io:format(M);
	false ->
	    ok
    end.
