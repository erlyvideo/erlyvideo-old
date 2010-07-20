%%% @private
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Main server behavior module
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information
%%% @end
%%%
%%% This file is part of erlang-rtmp.
%%% 
%%% erlang-rtmp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtmp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtmp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtmp_listener).
-author('Max Lapshin <max@maxidoors.ru>').
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-version(1.1).

-record(rtmp_listener, {
	listener, % Listening socket
	acceptor,  % Asynchronous acceptor's internal reference
	callback
	}).


-behaviour(gen_server).

%% External API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



%%--------------------------------------------------------------------
%% @spec (Port::integer(), Name::atom(), Callback::atom()) -> {ok, Pid} | {error, Reason}
%%
%% @doc Called by a supervisor to start the listening process.
%% @end
%%----------------------------------------------------------------------
start_link(Port, Name, Callback) when is_integer(Port) ->
    gen_server:start_link({local, Name}, ?MODULE, [Port, Callback], []).



%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------
init([Port, Callback]) ->
  process_flag(trap_exit, true),
  Opts = [binary, {packet, raw}, {reuseaddr, true},
          {keepalive, true}, {backlog, 30}, {active, false}],
  case gen_tcp:listen(Port, Opts) of
    {ok, Listen_socket} ->
      %%Create first accepting process
      {ok, Ref} = prim_inet:async_accept(Listen_socket, -1),
      {ok, #rtmp_listener{listener = Listen_socket, acceptor = Ref, callback = Callback}};
    {error, eaccess} ->
      error_logger:error_msg("Error connecting to port ~p. Try to open it in firewall or run with sudo.\n", [Port]),
      {stop, eaccess};
    {error, Reason} ->
      {stop, Reason}
  end.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------

handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------

handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info({inet_async, ListSock, Ref, {ok, CliSocket}},
            #rtmp_listener{listener=ListSock, acceptor=Ref, callback=Callback} = State) ->
  case set_sockopt(ListSock, CliSocket) of
  ok ->
    %% New client connected - spawn a new process using the simple_one_for_one
    %% supervisor.
    {ok, RTMP} = rtmp_sup:start_rtmp_socket(accept),
    gen_tcp:controlling_process(CliSocket, RTMP),
    {ok, Pid} = Callback:create_client(RTMP),
    rtmp_socket:setopts(RTMP, [{consumer, Pid}]),
    rtmp_socket:set_socket(RTMP, CliSocket),
    %% Signal the network driver that we are ready to accept another connection
    {ok, NewRef} = prim_inet:async_accept(ListSock, -1),
    {noreply, State#rtmp_listener{acceptor=NewRef}};
  {error, Reason} ->
    error_logger:error_msg("Error setting socket options: ~p.\n", [Reason]),
    {stop, Reason, State}
  end;
    
handle_info({inet_async, ListSock, Ref, Error}, #rtmp_listener{listener=ListSock, acceptor=Ref} = State) ->
  error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
  {stop, Error, State};
    
handle_info(_Info, State) ->
  {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, State) ->
  gen_tcp:close(State#rtmp_listener.listener),
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new client socket.
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
    {ok, Opts} ->
        case prim_inet:setopts(CliSocket, Opts) of
        ok    -> ok;
        Error -> gen_tcp:close(CliSocket), Error
        end;
    Error ->
        gen_tcp:close(CliSocket), Error
    end.
