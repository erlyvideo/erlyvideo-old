%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Supervisor module
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
-module(ems_users).
-author(max@maxidoors.ru).
-behaviour(gen_server).

-record(user_id_entry, {
  user_id,
  client
}).

-record(channel_entry, {
  channel,
  client
}).

-record(client_entry, {
  session_id,
  user_id,
  channels,
  client
}).

-record(ems_users, {
  clients,
	user_ids,
	channels,
	session_id = 0
}).

%% External API
-export([start_link/0, clients/0, login/2, logout/0, send_to_user/2, send_to_channel/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).



start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @spec () -> [Ip::tuple()]
%%
%% @doc Show list of clients
%% @end
%%----------------------------------------------------------------------
clients() ->
 Timeout = 10,
 lists:map(fun({_, Pid, _, _}) -> {Pid, gen_fsm:sync_send_event(Pid, {info}, Timeout)} end, supervisor:which_children(rtmp_session_sup)).


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


init([]) ->
  Clients = ets:new(clients, [set, {keypos, #client_entry.session_id}]),
  UserIds = ets:new(clients, [bag, {keypos, #user_id_entry.user_id}]),
  Channels = ets:new(clients, [bag, {keypos, #channel_entry.channel}]),
  {ok, #ems_users{clients = Clients, user_ids = UserIds, channels = Channels}}.

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

handle_call({login, UserId, UserChannels}, {Client, _Ref}, 
  #ems_users{clients = Clients, user_ids = UserIds, channels = Channels, session_id = LastSessionId} = Server) ->
  SessionId = LastSessionId + 1,
  ets:insert(Clients, #client_entry{session_id = SessionId, client = Client, user_id = UserId, channels = Channels}),
  ets:insert(UserIds, #user_id_entry{user_id = UserId, client = Client}),
  lists:foreach(fun(Channel) -> ets:insert(Channels, #channel_entry{channel = Channel, client = Client}) end, UserChannels),
  {reply, {ok, SessionId}, Server#ems_users{session_id = SessionId}};

handle_call(logout, {Client, _Ref}, #ems_users{clients = Clients, user_ids = UserIds, channels = Channels} = Server) ->
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
handle_cast({send_to_user, UserId, Message}, #ems_users{user_ids = UserIds} = Server) ->
  Clients = ets:lookup(UserIds, UserId),
  F = fun(#user_id_entry{client = Client}) -> 
    gen_fsm:send_event(Client, {message, Message}) 
  end,
  lists:foreach(F, Clients),
  {noreply, Server};

handle_cast({send_to_channel, Channel, Message}, #ems_users{channels = Channels} = Server) ->
  Clients = ets:lookup(Channels, Channel),
  F = fun(#channel_entry{client = Client}) -> 
    gen_fsm:send_event(Client, {message, Message}) 
  end,
  lists:foreach(F, Clients),
  {noreply, Server};

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
terminate(_Reason, _State) ->
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
