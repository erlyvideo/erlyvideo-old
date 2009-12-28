-module(ems_log).
-author(max@maxidoors.ru).

-export([start/0, stop/0, access/2, error/2, access/3, error/3]).


start() ->
  application:start(log4erl),
  [init_vhost_loggers(Host, Options) || {Host, Options} <- ems:get_var(vhosts, [])].
  
stop() ->
	application:stop(log4erl),
	application:unload(log4erl).
  
  
init_vhost_loggers(Host, Options) ->
  log4erl:add_logger(access_name(Host)),
  log4erl:add_console_appender(access_name(Host), access_name(Host, <<"_console">>), {info, "%j %T %l%n"}),
  case proplists:get_value(access_log, Options) of
    undefined -> ok;
    AccessSpec -> 
      log4erl:add_file_appender(access_name(Host), access_name(Host, <<"_file">>), AccessSpec)
  end,
  log4erl:add_logger(error_name(Host)),
  log4erl:add_console_appender(error_name(Host), error_name(Host, <<"_console">>), {info, "%j %T %l%n"}),
  case proplists:get_value(error_log, Options) of
    undefined -> ok;
    ErrorSpec -> 
      log4erl:add_file_appender(error_name(Host), error_name(Host, <<"_file">>), ErrorSpec)
  end,
  ok.
  
access_name(Host) ->
  access_name(Host, <<>>).
  
access_name(Host, Suffix) ->
  binary_to_atom(<<"access_log_", (atom_to_binary(Host, latin1))/binary, Suffix/binary>>, latin1).

error_name(Host) ->
  error_name(Host, <<>>).
  
error_name(Host, Suffix) ->
  binary_to_atom(<<"error_log_", (atom_to_binary(Host, latin1))/binary, Suffix/binary>>, latin1).
  
  
access(Host, Message) ->
  log4erl:info(access_name(Host), Message).
  
access(Host, Message, Format) ->
  log4erl:info(access_name(Host), Message, Format).

error(Host, Message) ->
  log4erl:error(error_name(Host), Message).

error(Host, Message, Format) ->
  log4erl:error(error_name(Host), Message, Format).
