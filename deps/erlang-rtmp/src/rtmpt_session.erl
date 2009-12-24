%% @author Max Lapshin <max@maxidoors.ru>

%% @doc RTMP client, that connects to rtmp backend.

-module(rtmpt_session).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(gen_server).
-define(RTMPT_TIMEOUT, 12000).

-record(rtmpt, {
	consumer = undefined,   % backend process
	session_id = undefined,
	buffer = <<>>,
	bytes_count = 0,
	sequence_number = 0,
	watchdog = undefined
	}).

-export([start_link/2, set_consumer/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% FSM States
-export([watcher/2]).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

start_link(SessionId, IP) ->
  gen_server:start_link(?MODULE, [SessionId, IP], []).

set_consumer(RTMPT, Consumer) ->
  gen_server:call(RTMPT, {set_consumer, Consumer}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------


%%-------------------------------------------------------------------------
%% Func: watcher/2
%% Returns: ignore
%% @private
%% @doc Looks after RTMPT timeout
%%-------------------------------------------------------------------------

watcher(Rtmp, Timeout) ->
    receive
        {rtmpt} ->
            ?MODULE:watcher(Rtmp, Timeout);
        {rtmpt, NewTimeout} ->
            ?MODULE:watcher(Rtmp, NewTimeout);
        {exit} ->
            ok
    after 
        Timeout ->
            Rtmp ! {rtmpt_timeout}
    end.


%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([SessionId]) ->
  % process_flag(trap_exit, true),
  {ok, RTMP} = gen_server:call(rtmp_listener, start, ?RTMPT_TIMEOUT),
  rtmp_session:set_socket(RTMP, self()),
  link(RTMP),
  Watchdog = spawn_link(?MODULE, watcher, [self(), ?RTMPT_TIMEOUT*5]),
  ets:insert(rtmp_sessions, {SessionId, self()}),
  {ok, #rtmpt{session_id = SessionId, watchdog = Watchdog, consumer = RTMP}, ?RTMPT_TIMEOUT}.
        

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

handle_call({server_data, Bin}, _From, #rtmpt{buffer = Buffer, bytes_count = BytesCount} = State) ->
  {noreply, State#rtmpt{buffer = <<Buffer/binary, Bin/binary>>, bytes_count = BytesCount + size(Bin)}, ?RTMPT_TIMEOUT};


handle_call({client_data, Bin}, _From, #rtmpt{consumer = Upstream} = State) when is_pid(Upstream)  ->
  % io:format("Received client bytes: ~p~n", [size(Bin)]),
  Upstream ! {rtmpt, self(), Bin},
  {noreply, State, ?RTMPT_TIMEOUT};

handle_call({set_consumer, Upstream}, _From, #rtmpt{consumer = undefined} = State) ->
  {noreply, State#rtmpt{consumer = Upstream}, ?RTMPT_TIMEOUT};

handle_call(timeout, _From, #rtmpt{} = State) ->
  error_logger:error_msg("Client ~p connection timeout.\n", [self()]),
  {stop, normal, State};

handle_call({info}, _From, #rtmpt{sequence_number = SequenceNumber, session_id = SessionId, buffer = Buffer, bytes_count = BytesCount} = State) ->
  Info = {self(), [{session_id, SessionId}, {sequence_number, SequenceNumber}, {total_bytes, BytesCount}, {unread_data, size(Buffer)} | process_info(self(), [message_queue_len, heap_size])]},
  {reply, Info, State, ?RTMPT_TIMEOUT};

handle_call({recv, SequenceNumber}, _From, #rtmpt{buffer = Buffer, watchdog = Watchdog} = State) ->
  Watchdog ! {rtmpt},
  % io:format("Recv ~p ~p bytes~n", [SequenceNumber, size(Buffer)]),
  {reply, {Buffer}, State#rtmpt{buffer = <<>>, sequence_number = SequenceNumber}, ?RTMPT_TIMEOUT}.



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


