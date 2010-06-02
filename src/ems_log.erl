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

-export([start/0, stop/0, access/2, error/2, access/3, error/3, debug/1, debug/2, debug/3, debug/4]).


%%-------------------------------------------------------------------------
%% @spec start() -> any()
%% @doc Starts all preconfigured loggers for all vhosts.
%% @end
%%-------------------------------------------------------------------------
start() ->
	application:start(log4erl),
  case file:read_file_info("priv/log4erl.conf") of
    {ok, _} -> 
      error_logger:info_msg("Loading logger configuration from priv/log4erl.conf~n"),
      log4erl:conf("priv/log4erl.conf");
    _ ->
      case file:read_file_info("/etc/erlyvideo/log4erl.conf") of
        {ok, _} -> 
          error_logger:info_msg("Loading logger configuration from /etc/erlyvideo/log4erl.conf~n"),
          log4erl:conf("/etc/erlyvideo/log4erl.conf");
        _ -> 
          log4erl:add_logger(default_logger),
          log4erl:add_console_appender(default_logger, app1, {info, "%j %T %l%n"})
      end
  end.
  
%%-------------------------------------------------------------------------
%% @spec stop() -> any()
%% @doc Stops all preconfigured loggers for all vhosts.
%% @end
%%-------------------------------------------------------------------------
stop() ->
  application:stop(log4erl),
	application:unload(log4erl),
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
