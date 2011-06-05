-module(microtcp).
-include("log.hrl").

-export([listen/2, listen/1, controlling_process/2, active_once/1]).

listen(Port) -> listen(Port, []).

-define(CMD_LISTEN, 1).
-define(CMD_ACTIVE_ONCE, 2).



listen(Port, Options) ->
  case erl_ddll:load_driver(code:lib_dir(microtcp,ebin), microtcp_drv) of
  	ok -> ok;
  	{error, already_loaded} -> ok;
  	{error, Error} -> exit({error, {could_not_load_driver,erl_ddll:format_error(Error)}})
  end,
  ?D({loaded,microtcp}),
  Socket = open_port({spawn, microtcp_drv}, [binary]),
  ?D({open_socket, Socket}),
  <<"ok">> = port_control(Socket, ?CMD_LISTEN, list_to_binary(integer_to_list(Port))),
  ?D({all_great}),
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


active_once(Socket) ->
  port_control(Socket, ?CMD_ACTIVE_ONCE, <<>>).


