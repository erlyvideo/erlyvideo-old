%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        Logging module
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% This file is part of erlmedia.
%%% 
%%% erlmedia is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlmedia is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlmedia.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(ems_log).
-author('Max Lapshin <max@maxidoors.ru>').

-export([start/0, stop/0, access/2, error/2, access/3, error/3, debug/1, debug/2, debug/3, debug/4]).

%%-------------------------------------------------------------------------
%% @spec start() -> any()
%% @doc Starts all preconfigured loggers for all vhosts.
%% @end
%%-------------------------------------------------------------------------
start() ->
	application:start(log4erl),
  ConfigPath = case file:read_file_info("priv/log4erl.conf") of
    {ok, _} -> 
      log4erl:conf("priv/log4erl.conf"),
      "priv/log4erl.conf";
    _ ->
      case file:read_file_info("/etc/erlyvideo/log4erl.conf") of
        {ok, _} -> 
          log4erl:conf("/etc/erlyvideo/log4erl.conf"),
          "/etc/erlyvideo/log4erl.conf";
        _ -> 
          log4erl:add_logger(default_logger),
          log4erl:add_console_appender(default_logger, app1, {info, "%j %T %l%n"}),
          "none"
      end
  end,
  log4erl:error_logger_handler(), %% to get all error_logger
  error_logger:delete_report_handler(error_logger), %% to disable error_logger file output
  error_logger:tty(false), %% to disable console output
  error_logger:info_msg("Loading config from ~s~n", [ConfigPath]),
  ok.
  
%%-------------------------------------------------------------------------
%% @spec stop() -> any()
%% @doc Stops all preconfigured loggers for all vhosts.
%% @end
%%-------------------------------------------------------------------------
stop() ->
  application:stop(log4erl),
	application:unload(log4erl),
	error_logger:tty(true),
	error_logger:add_report_handler(error_logger),
  ok.
  
  
access_name(_Host) ->
  default_logger.
  
error_name(_Host) ->
  default_logger.
  
  
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

debug(Message) ->
  debug(3, all, Message).

debug(Message, Args) ->
  debug(3, all, Message, Args).

debug(Level, Message, Args) when is_number(Level) ->
  debug(Level, all, Message, Args);

debug(Facility, Message, Args) when is_atom(Facility) ->
  debug(3, Facility, Message, Args).

debug(_Level, all, Message, Args) ->
  log4erl:debug(default_logger, Message, Args);

debug(_Level, Facility, Message, Args) ->
  log4erl:debug(default_logger, "["++atom_to_list(Facility) ++"] " ++ Message, Args).



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
