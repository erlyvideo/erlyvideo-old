%% @author Max Lapshin <max@maxidoors.ru>

%% @doc RTMP client, that connects to rtmp backend.

-module(rtmpt).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(gen_server).
-define(RTMPT_TIMEOUT, 12000).

-record(rtmpt, {
	consumer = undefined,   % backend process
	session_id = undefined,
	ip,
	buffer = <<>>,
	bytes_count = 0,
	sequence_number = 0
	}).


-export([open/2, idle/3, send/4, close/2, write/2]).


-export([start_link/2, set_consumer/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------


open(IP, Consumer) ->
  {ok, RTMPT, SessionID} = rtmpt_sessions:create(IP),
  {ok, RTMP} = rtmp_socket:start_socket(Consumer, accept, RTMPT),
  rtmpt:set_consumer(RTMPT, RTMP),
  {ok, RTMP, SessionID}.


idle(SessionID, IP, Sequence) ->
  case rtmpt_sessions:find(SessionID, IP) of
    {error, Reason} -> {error, Reason};
    {ok, RTMPT} -> gen_server:call(RTMPT, {recv, Sequence})
  end.

send(SessionID, IP, Sequence, Data) ->
  case rtmpt_sessions:find(SessionID, IP) of
    {error, Reason} -> {error, Reason};
    {ok, RTMPT} -> 
      gen_server:call(RTMPT, {client_data, Data}),
      gen_server:call(RTMPT, {recv, Sequence})
  end.

close(SessionID, IP) ->
  case rtmpt_sessions:find(SessionID, IP) of
    {error, Reason} -> {error, Reason};
    {ok, RTMPT} -> gen_server:cast(RTMPT, close)
  end.

write(RTMPT, Data) ->
  gen_server:call(RTMPT, {server_data, Data}).

start_link(SessionId, IP) ->
  gen_server:start_link(?MODULE, [SessionId, IP], []).

set_consumer(RTMPT, Consumer) ->
  gen_server:call(RTMPT, {set_consumer, Consumer}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------


%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([SessionId, IP]) ->
  % process_flag(trap_exit, true),
  {ok, #rtmpt{session_id = SessionId, ip = IP}, ?RTMPT_TIMEOUT}.
        

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
  Data = iolist_to_binary(Bin),
  {reply, ok, State#rtmpt{buffer = <<Buffer/binary, Data/binary>>, bytes_count = BytesCount + size(Data)}, ?RTMPT_TIMEOUT};


handle_call({client_data, Bin}, _From, #rtmpt{consumer = Upstream} = State) when is_pid(Upstream)  ->
  Upstream ! {rtmpt, self(), Bin},
  {reply, ok, State, ?RTMPT_TIMEOUT};

handle_call({set_consumer, Upstream}, _From, #rtmpt{consumer = undefined} = State) ->
  {reply, ok, State#rtmpt{consumer = Upstream}, ?RTMPT_TIMEOUT};

handle_call(timeout, _From, #rtmpt{consumer = _Consumer} = State) ->
  % gen_fsm:send_event(Consumer, timeout),
  % {stop, normal, State};
  {reply, ok, State, ?RTMPT_TIMEOUT};


handle_call({info}, _From, #rtmpt{sequence_number = SequenceNumber, session_id = SessionId, buffer = Buffer, bytes_count = BytesCount} = State) ->
  Info = {self(), [{session_id, SessionId}, {sequence_number, SequenceNumber}, {total_bytes, BytesCount}, {unread_data, size(Buffer)} | process_info(self(), [message_queue_len, heap_size])]},
  {reply, Info, State, ?RTMPT_TIMEOUT};

handle_call({recv, SequenceNumber}, _From, #rtmpt{buffer = Buffer, consumer = Consumer} = State) ->
  Consumer ! {rtmpt, self(), alive},
  {reply, {ok, Buffer}, State#rtmpt{buffer = <<>>, sequence_number = SequenceNumber}, ?RTMPT_TIMEOUT}.



%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(close, #rtmpt{} = State) ->
  {stop, normal, State};

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

handle_info(timeout, State) ->
  {noreply, State};

handle_info(_Info, State) ->
  io:format("RTMPT unknown message: ~p~n", [_Info]),
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


