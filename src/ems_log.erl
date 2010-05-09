%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        Logging module
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
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
-module(ems_log).
-author('Max Lapshin <max@maxidoors.ru>').

-export([start/0, stop/0, access/2, error/2, access/3, error/3]).


%%-------------------------------------------------------------------------
%% @spec start() -> any()
%% @doc Starts all preconfigured loggers for all vhosts.
%% @end
%%-------------------------------------------------------------------------
start() ->
  [init_vhost_loggers(Host, Options) || {Host, Options} <- ems:get_var(vhosts, [])].
  
%%-------------------------------------------------------------------------
%% @spec stop() -> any()
%% @doc Stops all preconfigured loggers for all vhosts.
%% @end
%%-------------------------------------------------------------------------
stop() ->
  ok.
  
  
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
  
  
%%-------------------------------------------------------------------------
%% @spec access(Host,Message) -> any()
%% @doc Log message at access level
%% @end
%%-------------------------------------------------------------------------
access(Host, Message) ->
  log4erl:info(access_name(Host), Message).
  
%%-------------------------------------------------------------------------
%% @spec access(Host,Message,Format) -> any()
%% @doc Log formatted message at access level
%% @end
%%-------------------------------------------------------------------------
access(Host, Message, Format) ->
  log4erl:info(access_name(Host), Message, Format).

%%-------------------------------------------------------------------------
%% @spec error(Host,Message) -> any()
%% @doc Log message at error level
%% @end
%%-------------------------------------------------------------------------
error(Host, Message) ->
  log4erl:error(error_name(Host), Message).

%%-------------------------------------------------------------------------
%% @spec error(Host,Message,Format) -> any()
%% @doc Log format message at error level
%% @end
%%-------------------------------------------------------------------------
error(Host, Message, Format) ->
  log4erl:error(error_name(Host), Message, Format).
