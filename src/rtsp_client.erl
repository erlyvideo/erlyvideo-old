-module(rtsp_client).
-author('max@maxidoors.ru').

-behaviour(gen_fsm).
-include("../include/ems.hrl").

-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
  'WAIT_FOR_SOCKET'/2,
	'WAIT_FOR_REQUEST'/2,
	'WAIT_FOR_HEADERS'/2,
  'WAIT_FOR_DATA'/2]).

-record(rtsp_client, {
  socket,
  addr,
  port
}).


start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit, true),
  random:seed(now()),
  {ok, 'WAIT_FOR_SOCKET', #rtsp_client{}}.



%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) ->
  inet:setopts(Socket, [{active, once}, {packet, line}, binary]),
  {ok, {IP, Port}} = inet:peername(Socket),
  {next_state, 'WAIT_FOR_REQUEST', State#rtsp_client{socket=Socket, addr=IP, port = Port}, ?TIMEOUT}.


'WAIT_FOR_REQUEST'({data, Request}, State) ->
  ?D({"Request", Request}),
  {next_state, 'WAIT_FOR_HEADERS', State, ?TIMEOUT}.

'WAIT_FOR_HEADERS'({header, Name, Value}, #rtsp_client{socket = Socket} = State) ->
  ?D({"Header", Name, Value}),
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_HEADERS', State, ?TIMEOUT};

'WAIT_FOR_HEADERS'(end_of_headers, #rtsp_client{socket = Socket} = State) ->
  ?D("Headers finished"),
  inet:setopts(Socket, [{packet, raw}]),
  inet:setopts(Socket, [{active, false}]),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.


'WAIT_FOR_DATA'(Message, State) ->
  ?D({"Message", Message}),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.


%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
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
   io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, got_sync_request]),
  {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, 'WAIT_FOR_REQUEST' = StateName, State) ->
  inet:setopts(Socket, [{active, once}, {packet, httph_bin}, binary]),
  ?MODULE:StateName({data, Bin}, State);

handle_info({http, Socket, {http_header, _, Name, _, Value}}, StateName, State) ->
  inet:setopts(Socket, [{active, once}]),
  ?MODULE:StateName({header, Name, Value}, State);

handle_info({http, Socket, http_eoh}, StateName, State) ->
  inet:setopts(Socket, [{active, false}, {packet, raw}, binary]),
  ?MODULE:StateName(end_of_headers, State);


handle_info({tcp_closed, Socket}, _StateName, #rtsp_client{socket=Socket, addr=Addr, port = Port} = StateData) ->
  error_logger:info_msg("~p Client ~p:~p disconnected.\n", [self(), Addr, Port]),
  {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
  ?D({"Some info handled", _Info, StateName, StateData}),
  {noreply, StateName, StateData}.


%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #rtsp_client{socket=Socket}) ->
  ?D({"RTSP stopping", _StateName, _Reason}),
  (catch gen_tcp:close(Socket)),
  ok.


%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

