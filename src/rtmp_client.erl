%% @author Max Lapshin <max@maxidoors.ru>

%% @doc RTMP client, that connects to rtmp backend.

-module(rtmp_client).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(gen_fsm).
-include("../include/ems.hrl").

-record(rtmp_fsm, {
	upstream = undefined,   % backend process
	session_id = undefined,
	buffer = <<>>,
	bytes_count = 0,
	sequence_number = 0,
	watchdog = undefined
	}).

-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export(['READY'/2, 'READY'/3]).
-export([watcher/2]).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

start_link(SessionId) ->
    gen_fsm:start(?MODULE, [SessionId], []).


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
            watcher(Rtmp, Timeout);
        {rtmpt, NewTimeout} ->
            watcher(Rtmp, NewTimeout);
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
    {ok, Rtmp} = gen_server:call(ems_server, {start}, ?RTMPT_TIMEOUT),
    link(Rtmp),
    io:format("Received upstream ~p~n", [Rtmp]),
    Watchdog = spawn_link(rtmp_client, watcher, [self(), ?RTMPT_TIMEOUT*5]),
    ets:insert(rtmp_sessions, {SessionId, self()}),
    {ok, 'READY', #rtmp_fsm{session_id = SessionId, watchdog = Watchdog, upstream = Rtmp}, ?RTMP_TIMEOUT}.
        

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------

%% Notification event coming from client

'READY'({server_data, Bin}, #rtmp_fsm{buffer = Buffer, bytes_count = BytesCount} = State) ->
    % io:format("Received server bytes: ~p/~p~n", [size(Bin), BytesCount + size(Bin)]),
    {next_state, 'READY', State#rtmp_fsm{buffer = <<Buffer/binary, Bin/binary>>, bytes_count = BytesCount + size(Bin)}, ?RTMP_TIMEOUT};


'READY'({client_data, Bin}, #rtmp_fsm{upstream = Upstream} = State) when is_pid(Upstream)  ->
    % io:format("Received client bytes: ~p~n", [size(Bin)]),
    gen_fsm:send_event(Upstream, {data, Bin}),
    {next_state, 'READY', State, ?RTMP_TIMEOUT};


'READY'({upstream, Upstream}, #rtmp_fsm{upstream = undefined} = State) ->
    {next_state, 'READY', State#rtmp_fsm{upstream = Upstream}, ?RTMP_TIMEOUT};


'READY'(timeout, #rtmp_fsm{} = State) ->
    error_logger:error_msg("Client ~p connection timeout.\n", [self()]),
    {stop, normal, State}.

'READY'({info}, _From, #rtmp_fsm{sequence_number = SequenceNumber, session_id = SessionId, buffer = Buffer, bytes_count = BytesCount} = State) ->
    Info = {self(), [{session_id, SessionId}, {sequence_number, SequenceNumber}, {total_bytes, BytesCount}, {unread_data, size(Buffer)} | process_info(self(), [message_queue_len, heap_size])]},
    {reply, Info, 'READY', State, ?RTMP_TIMEOUT};

'READY'({recv, SequenceNumber}, _From, #rtmp_fsm{buffer = Buffer, watchdog = Watchdog} = State) ->
    Watchdog ! {rtmpt},
    io:format("Recv ~p ~p bytes~n", [SequenceNumber, size(Buffer)]),
    {reply, {Buffer}, 'READY', State#rtmp_fsm{buffer = <<>>, sequence_number = SequenceNumber}, ?RTMP_TIMEOUT}.



%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    error_logger:error_msg("TRACE ~p:~p ~p, ~p~n",[?MODULE, ?LINE, handle_event, Event]),
    {stop, {StateName, undefined_event, Event}, StateData}.


%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------

handle_sync_event(Event, _From, StateName, StateData) ->
    error_logger:error_msg("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, got_sync_request2]),
    {stop, {StateName, undefined_event, Event}, StateData}.


%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------

% handle_info({tcp_closed, Socket}, _StateName, #rtmp_fsm{socket=Socket, session_id = SessionId} = StateData) ->
%     error_logger:info_msg("Server disconnected us: ~p, ~p\n", [self(), SessionId]),
%     {stop, normal, StateData#rtmp_fsm{buffer = <<>>}};

handle_info({rtmpt_timeout}, _StateName, #rtmp_fsm{} = StateData) ->
    error_logger:error_msg("RTMPT ~p connection timeout.\n", [self()]),
    {stop, normal, StateData#rtmp_fsm{buffer = <<>>}};

handle_info({'EXIT', _Pid, killed}, _StateName, State) ->
    {stop, normal, State};

handle_info(_Info, StateName, StateData) ->
    io:format("Unknown info ~p~n", [_Info]),
    {noreply, StateName, StateData}.


%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(Reason, _StateName, #rtmp_fsm{upstream = Upstream, session_id = SessionId, bytes_count = BytesCount, watchdog = Watchdog}) ->
    io:format("Disconnected ~p ~p because of ~p, Bytes received ~p~n", [self(), SessionId, Reason, BytesCount]),
    ets:delete(rtmp_sessions, SessionId),
    gen_fsm:send_event(Upstream, {exit}),
    Watchdog ! {exit},
    ok.


%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, #rtmp_fsm{} = StateData, _Extra) ->
    {ok, StateName, StateData}.
