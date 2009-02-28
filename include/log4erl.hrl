-define(ROTATION_CHECK, 10).

-define(DEFAULT_CONF,"log4erl.conf").

-define(DEFAULT_FORMAT, "[%L] %l%n").

-define(DEFAULT_LEVEL, warn).

-define(DEFAULT_LOGGER, default_logger).
-define(DEFAULT_LOGGER_GUARD, default_logger_guard).

-define(FILE_OPTIONS,[write, raw, binary, append]).
-define(FILE_OPTIONS_ROTATE,[write, raw, binary]).

%-define(DEBUG, true).

-ifdef(DEBUG).
-define(LOG(X), io:format("~p: " ++ X,[?MODULE])).
-define(LOG2(X,D), io:format("~p: " ++ X,[?MODULE | D])).
-else.
-define(LOG(_X), ok).
-define(LOG2(_X,_D), ok).
-endif.


%% type = time | size
%% max = seconds (for time) | or kiloBytes (for size)
-record(log_type,{type, max, timer}).

%% file_name = the name of the file without counter 
%% fd = the descriptior for the file
%% counter = current counter, used for appending to file_name in case of rotation
%% log_type is a log_type record
%% rotation = number of rotation before the logger wraps around the coutner (>1)
%% suffix = suffix of file name (e.g. txt)
%% The filename of a log is file_name ++ "_" ++ counter ++ "." ++ suffix
%% e.g. log_1.txt
%% tokens = format tokens generated from log_formatter:parse/1
-record(file_appender, {dir, file_name, fd, counter, log_type, rotation, suffix, level, format}).

-record(console_appender, {level, format}).

-record(rotation_state, {state, timer}).

%% log record
-record(log, {level, msg, data, time, millis}).
