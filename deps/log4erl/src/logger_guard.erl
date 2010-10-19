-module(logger_guard).

-author("Ahmed Al-Issaei").
-license("MPL-1.1").

-behaviour(gen_server).

-include("../include/log4erl.hrl").

%% API
-export([start_link/4, add_sup_handler/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

start_link(Logger, Appender, Name, Conf) ->
    %?LOG2("starting guard for logger ~p~n",[Logger]),
    {ok, Pid} = gen_server:start_link(?MODULE, [Appender, Name], []),
    case add_sup_handler(Pid, Logger, Conf) of
	{error, E} ->
	    gen_server:call(Pid, stop),
	    {error, E};
	_R ->
	    {ok, Pid}
    end.

add_sup_handler(G_pid, Logger, Conf) ->
    ?LOG("add_sup()~n"),
    gen_server:call(G_pid, {add_sup_handler, Logger, Conf}).

%%=========================================
%% gen_server callback functions
%%=========================================
init([Appender, Name]) ->
    ?LOG2("Starting guard ~n",[]),
    {ok, [{appender, Appender, Name}]}.

handle_call({add_sup_handler, Logger, Conf}, _From, [{appender, Appender, Name}] = State) ->
    ?LOG2("Adding handler ~p with name ~p for ~p From ~p~n",[Appender, Name, Logger, _From]),
    try
	Res = gen_event:add_sup_handler(Logger, {Appender, Name}, Conf),
	{reply, Res, State}
    catch
	E:R ->
	    {reply, {error, {E,R}}, State}
    end;
handle_call(stop, _From, State) ->
    {stop, stop, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gen_event_EXIT, _Mod, R}, State) ->
    ?LOG2("logger_guard received exit message. Terminating ~p~n",[now]),
    {stop, {appender_died, R}, State};
handle_info(_Info, [{appender, _Appender, _Name}] = State) ->
    ?LOG2("logger_guard received msg ~p for appender ~p with name ~p~n ", [_Info, _Appender, _Name]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

