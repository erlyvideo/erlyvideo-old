%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
%%% @doc        Main server behavior module
%%% @reference  See <a href="http://erlyvideo.googlecode.com" target="_top">http://erlyvideo.googlecode.com</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
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
-module(rtmp_listener).
-author(max@maxidoors.ru).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-include("../../../include/ems.hrl").

-record(rtmp_listener, {
	listener, % Listening socket
	acceptor,  % Asynchronous acceptor's internal reference
	clients,
	user_ids,
	channels,
	session_id = 0
	}).


-record(client_entry, {
  session_id,
  user_id,
  channels,
  client
}).

-record(user_id_entry, {
  user_id,
  client
}).

-record(channel_entry, {
  channel,
  client
}).

-behaviour(gen_server).

%% External API
-export([start_link/1, clients/0, login/2, logout/0, send_to_user/2, send_to_channel/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


%%--------------------------------------------------------------------
%% @spec () -> [Ip::tuple()]
%%
%% @doc Show list of clients
%% @end
%%----------------------------------------------------------------------
clients() ->
  Timeout = 10,
  lists:map(fun({_, Pid, _, _}) -> {Pid, gen_fsm:sync_send_event(Pid, {info}, Timeout)} end, supervisor:which_children(rtmp_client_sup)).


%%--------------------------------------------------------------------
%% @spec (UserId::integer(), Channels::[integer()]) -> {ok, SessionId::Integer()}
%%
%% @doc Register logged user in server tables
%% @end
%%----------------------------------------------------------------------
login(undefined, _) -> undefined;
login(_, undefined) -> undefined;

login(UserId, Channels) ->
  gen_server:call(?MODULE, {login, UserId, Channels}).
  
%%--------------------------------------------------------------------
%% @spec () -> {ok}
%%
%% @doc Logout user from server tables
%% @end
%%----------------------------------------------------------------------
logout() ->
  gen_server:call(?MODULE, logout).

%%--------------------------------------------------------------------
%% @spec (UserId::integer(), Message::text) -> {ok}
%%
%% @doc Send message to all logged instances of userId
%% @end
%%----------------------------------------------------------------------
send_to_user(UserId, Message) ->
  gen_server:cast(?MODULE, {send_to_user, UserId, Message}).

%%--------------------------------------------------------------------
%% @spec (Channel::integer(), Message::text) -> {ok}
%%
%% @doc Send message to all, subscribed on channel
%% @end
%%----------------------------------------------------------------------
send_to_channel(Channel, Message) ->
  gen_server:cast(?MODULE, {send_to_channel, Channel, Message}).


%%--------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, Pid} | {error, Reason}
%%
%% @doc Called by a supervisor to start the listening process.
%% @end
%%----------------------------------------------------------------------
start_link(Port) when is_integer(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

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
init([Port]) ->
    process_flag(trap_exit, true),
    Opts = [binary, {packet, raw}, {reuseaddr, true},
            {keepalive, true}, {backlog, 30}, {active, false}],
    case gen_tcp:listen(Port, Opts) of
        {ok, Listen_socket} ->
            %%Create first accepting process
            {ok, Ref} = prim_inet:async_accept(Listen_socket, -1),
            Clients = ets:new(clients, [set, {keypos, #client_entry.session_id}]),
            UserIds = ets:new(clients, [bag, {keypos, #user_id_entry.user_id}]),
            Channels = ets:new(clients, [bag, {keypos, #channel_entry.channel}]),
            {ok, #rtmp_listener{listener = Listen_socket, acceptor = Ref, 
                              clients = Clients, user_ids = UserIds, channels = Channels}};
        {error, eacces} ->
            error_logger:error_msg("Error connecting to port ~p. Try to open it in firewall or run with sudo.\n", [Port]),
            {stop, eacces};
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

handle_call({start}, {From, _Ref}, State) ->
  {ok, Pid} = ems_sup:start_rtmp_client(),
  gen_fsm:sync_send_event(Pid, {socket_ready, From}),
  {reply, {ok, Pid}, State};

handle_call({login, UserId, UserChannels}, {Client, _Ref}, 
  #rtmp_listener{clients = Clients, user_ids = UserIds, channels = Channels, session_id = LastSessionId} = Server) ->
  SessionId = LastSessionId + 1,
  ets:insert(Clients, #client_entry{session_id = SessionId, client = Client, user_id = UserId, channels = Channels}),
  ets:insert(UserIds, #user_id_entry{user_id = UserId, client = Client}),
  lists:foreach(fun(Channel) -> ets:insert(Channels, #channel_entry{channel = Channel, client = Client}) end, UserChannels),
  {reply, {ok, SessionId}, Server#rtmp_listener{session_id = SessionId}};


handle_call(logout, {Client, _Ref}, #rtmp_listener{clients = Clients, user_ids = UserIds, channels = Channels} = Server) ->
  ets:match_delete(Clients, #client_entry{session_id = '_', user_id = '_', channels = '_', client = Client}),
  ets:match_delete(UserIds, #user_id_entry{user_id = '_', client = Client}),
  ets:match_delete(Channels, #channel_entry{channel = '_', client = Client}),
  {reply, {ok}, Server};

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
handle_cast({send_to_user, UserId, Message}, #rtmp_listener{user_ids = UserIds} = Server) ->
  Clients = ets:lookup(UserIds, UserId),
  F = fun(#user_id_entry{client = Client}) -> 
    gen_fsm:send_event(Client, {message, Message}) 
  end,
  lists:foreach(F, Clients),
  {noreply, Server};

handle_cast({send_to_channel, Channel, Message}, #rtmp_listener{channels = Channels} = Server) ->
  Clients = ets:lookup(Channels, Channel),
  F = fun(#channel_entry{client = Client}) -> 
    gen_fsm:send_event(Client, {message, Message}) 
  end,
  lists:foreach(F, Clients),
  {noreply, Server};

handle_cast(_Msg, State) ->
    {noreply, State}.

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
            #rtmp_listener{listener=ListSock, acceptor=Ref} = State) ->
    case set_sockopt(ListSock, CliSocket) of
    ok ->
        %% New client connected - spawn a new process using the simple_one_for_one
        %% supervisor.
        {ok, Pid} = ems_sup:start_rtmp_client(),
        gen_tcp:controlling_process(CliSocket, Pid),
        %% Instruct the new FSM that it owns the socket.
        rtmp_client:set_socket(Pid, CliSocket),
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
    
handle_info({clients, _From}, #rtmp_listener{} = State) ->
  ?D("Asked for clients list"),
  {noreply, State};

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
