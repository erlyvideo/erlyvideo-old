%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        Special TCP driver
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(microtcp).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([listen/2, listen/1, controlling_process/2, active_once/1, send/2, close/1, peername/1]).

listen(Port) -> listen(Port, []).

-define(CMD_LISTEN, 1).
-define(CMD_ACTIVE_ONCE, 2).



listen(Port, Options) ->
  case erl_ddll:load_driver(code:lib_dir(microtcp,ebin), microtcp_drv) of
  	ok -> ok;
  	{error, already_loaded} -> ok;
  	{error, Error} -> exit({error, {could_not_load_driver,erl_ddll:format_error(Error)}})
  end,
  Socket = open_port({spawn, microtcp_drv}, [binary]),
  Reuseaddr = case proplists:get_value(reuseaddr, Options, true) of
    true -> 1;
    _ -> 0
  end,
  Keepalive = case proplists:get_value(keepalive, Options) of
    true -> 1;
    _ -> 0
  end,
  Timeout = proplists:get_value(timeout, Options, 60000),
  UpperLimit = proplists:get_value(upper_limit, Options, 1000000),
  LowerLimit = proplists:get_value(lower_limit, Options, 0),
  Backlog = proplists:get_value(backlog, Options, 30),
  
  <<"ok">> = port_control(Socket, ?CMD_LISTEN, <<Port:16, Backlog:16/little, Reuseaddr, Keepalive, Timeout:16/little, UpperLimit:32/little, LowerLimit:32/little>>),
  {ok, Socket}.


controlling_process(Socket, NewOwner) when is_port(Socket), is_pid(NewOwner) ->
  case erlang:port_info(Socket, connected) of
	  {connected, Pid} when Pid =/= self() ->
	    {error, not_owner};
	  undefined ->
	    {error, einval};
	_ ->
		try erlang:port_connect(Socket, NewOwner) of
		  true -> 
			  unlink(Socket), %% unlink from port
				ok
		catch
			error:Reason -> 
				{error, Reason}
		end
  end.

peername(Socket) when is_port(Socket) ->
  {ok, {{0,0,0,0}, 4000}}.

close(Socket) when is_port(Socket) ->
  erlang:port_close(Socket).

active_once(Socket) ->
  port_control(Socket, ?CMD_ACTIVE_ONCE, <<>>).


send(Socket, Bin) when is_port(Socket) ->
  port_command(Socket, Bin).


