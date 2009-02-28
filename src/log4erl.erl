-module(log4erl).

-author("Ahmed Al-Issaei").
-license("MPL-1.1").

-behaviour(gen_server).
-behaviour(application).

-include("../include/log4erl.hrl").

%% API
-export([start_link/1]).

-export([change_log_level/1, change_log_level/2]).
-export([add_logger/1]).
-export([add_appender/2, add_appender/3]).
-export([add_file_appender/2, add_file_appender/3]).
-export([add_console_appender/2, add_console_appender/3]).
-export([add_dummy_appender/2, add_dummy_appender/3]).
-export([get_appenders/0, get_appenders/1]).
-export([change_format/2, change_format/3]).

-export([log/2, log/3, log/4]).

-export([warn/1, warn/2, warn/3]).
-export([info/1, info/2, info/3]).
-export([error/1, error/2, error/3]).
-export([fatal/1, fatal/2, fatal/3]).
-export([debug/1, debug/2, debug/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Application callbacks
-export([start/2, stop/1]).

start_link(Default_logger) ->
    %log4erl_sup:add_logger(Default_logger),
    ?LOG("Starting process log4erl"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Default_logger], []).

add_logger(Logger) ->
    try_msg({add_logger, Logger}).

%% Appender = {Appender, Name}
add_appender(Appender, Conf) ->
    try_msg({add_appender, Appender, Conf}).

%% Appender = {Appender, Name}
add_appender(Logger, Appender, Conf) ->
    try_msg({add_appender, Logger, Appender, Conf}).

add_console_appender(AName, Conf) ->
    add_appender({console_appender, AName}, Conf).

add_console_appender(Logger, AName, Conf) ->
    add_appender(Logger, {console_appender, AName}, Conf).

add_file_appender(AName, Conf) ->
    add_appender({file_appender, AName}, Conf).

add_file_appender(Logger, AName, Conf) ->
    add_appender(Logger, {file_appender, AName}, Conf).

add_dummy_appender(AName, Conf) ->
    add_appender({dummy_appender, AName}, Conf).

add_dummy_appender(Logger, AName, Conf) ->
    add_appender(Logger, {dummy_appender, AName}, Conf).

get_appenders() ->
    try_msg(get_appenders).

get_appenders(Logger) ->
    try_msg({get_appenders, Logger}).

change_format(Appender, Format) ->
    try_msg({change_format, Appender, Format}).
change_format(Logger, Appender, Format) ->
    try_msg({change_format, Logger, Appender, Format}).

%% For default logger
change_log_level(Level) ->
    try_msg({change_level, Level}).
change_log_level(Logger, Level) ->
    try_msg({change_level, Logger, Level}).

try_msg(Msg) ->
     try
 	gen_server:call(?MODULE, Msg)
     catch
  	exit:{noproc, _M} ->
 	    io:format("log4erl has not been initialized yet. To do so, please run~n"),
 	    io:format("> application:start(log4erl).~n"),
 	    {error, log4erl_not_started};
 	  E:M ->
 	    ?LOG2("Error message received by log4erl is ~p:~p~n",[E, M]),
 	    {E, M}
     end.

log(Level, Log) ->
    log(Level, Log, []).
log(Level, Log, Data) ->
    try_msg({log, Level, Log, Data}).
log(Logger, Level, Log, Data) ->
    try_msg({log, Logger, Level, Log, Data}).

warn(Log) ->
    log(warn, Log).
%% If 1st parameter is atom, then it is Logger
warn(Logger, Log) when is_atom(Logger) ->
    log(Logger, warn, Log, []);
warn(Log, Data) ->
    log(warn, Log, Data).
warn(Logger, Log, Data) ->
    log(Logger, warn , Log, Data).

info(Log) ->
    log(info, Log).
info(Logger, Log) when is_atom(Logger) ->
    log(Logger, info, Log, []);
info(Log, Data) ->
    log(info, Log, Data).
info(Logger, Log, Data) ->
    log(Logger, info, Log, Data).

error(Log) ->
    log(error, Log).
error(Logger, Log) when is_atom(Logger) ->
    log(Logger, error, Log, []);
error(Log, Data) ->
    log(error, Log, Data).
error(Logger, Log, Data) ->
    log(Logger, error, Log, Data).

fatal(Log) ->
    log(fatal, Log).
fatal(Logger, Log) when is_atom(Logger) ->
    log(Logger, fatal, Log, []);
fatal(Log, Data) ->
    log(fatal, Log, Data).
fatal(Logger, Log, Data) ->
    log(Logger, fatal, Log, Data).

debug(Log) ->
    log(debug, Log).
debug(Logger, Log) when is_atom(Logger) ->
    log(Logger, debug, Log, []);
debug(Log, Data) ->
    log(debug, Log, Data).
debug(Logger, Log, Data) ->
    log(Logger, debug, Log, Data).

%%======================================
%% gen_server callback functions
%%======================================
init([Default_logger]) ->
    ?LOG2("starting log4erl server with default_logger ~p~n",[Default_logger]),
    {ok, {default_logger, Default_logger}}.

%% No logger specified? use default logger
handle_call({add_logger, Logger}, _From, State) ->
    log_manager:add_logger(Logger),
    {reply, ok, State};
handle_call({add_appender, Appender, Conf}, _From, {default_logger, DL} = State) ->
    log_manager:add_appender(DL, Appender, Conf),
    {reply, ok, State};
handle_call({add_appender, Logger, Appender, Conf}, _From, State) ->
    log_manager:add_appender(Logger, Appender, Conf),
    {reply, ok, State};
handle_call(get_appenders, _From, {default_logger, DL} = State) ->
    Reply = gen_event:which_handlers(DL),
    {reply, Reply, State};
handle_call({get_appenders, Logger}, _From, State) ->
    Reply = gen_event:which_handlers(Logger),
    {reply, Reply, State};
handle_call({change_level, Level}, _From, {default_logger, DL} = State) ->
    log_manager:change_level(DL, Level),
    {reply, ok, State};
handle_call({change_level, Logger, Level}, _From, State) ->
    log_manager:change_level(Logger, Level),    
    {reply, ok, State};
handle_call({change_format, Appender, Format}, _From, {default_logger,DL} = State) ->
    log_manager:change_format(DL, Appender, Format),
    {reply, ok, State};
handle_call({change_format, Logger, Appender, Format}, _From, State) ->
    log_manager:change_format(Logger, Appender, Format),
    {reply, ok, State};
handle_call({log, Level, Log, Data}, _From, {default_logger, Logger} = State) ->
    log_manager:log(Logger, Level, Log, Data), 
    {reply, ok, State};
handle_call({log, Logger, Level, Log, Data} , _From, State) ->
    log_manager:log(Logger, Level, Log, Data),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%======================================
%% application callback functions
%%======================================
start(_Type, [Arg]) ->
    ?LOG("Starting log4erl app~n"),
    log4erl_sup:start_link(Arg).

stop(_State) ->
    ok.


