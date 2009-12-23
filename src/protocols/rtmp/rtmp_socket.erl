-module(rtmp_socket).
-author(max@maxidoors.ru).
-include("../include/rtmp.hrl").
-include("../include/rtmp_private.hrl").
-include("../include/debug.hrl").

-export([accept/1, client_buffer/1, window_size/1, setopts/2]).


%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([wait_for_socket/2]).


-spec(accept(Socket::port()) -> RTMPSocket::pid()).
accept(Socket) ->
  Pid = gen_fsm:start_link(?MODULE, [self()], []),
  gen_tcp:controlling_process(Socket, Pid),
  gen_fsm:send_event(Pid, {socket, Socket}),
  Pid.
  

-spec(client_buffer(RTMP::rtmp_socket_pid()) -> integer()).
client_buffer(RTMP) ->
  gen_fsm:sync_send_event(RTMP, client_buffer, ?RTMP_TIMEOUT).

-spec(window_size(RTMP::rtmp_socket_pid()) -> integer()).
window_size(RTMP) ->
  gen_fsm:sync_send_event(RTMP, window_size, ?RTMP_TIMEOUT).
  

-spec(setopts(RTMP::rtmp_socket_pid(), Options::[{Key::atom(), Value::any()}]) -> ok).
setopts(RTMP, Options) ->
  gen_fsm:send_event(RTMP, {setopts, Options}, ?RTMP_TIMEOUT).

  
  
init([Consumer]) ->
  link(Consumer),
  {ok, wait_for_socket, #rtmp_socket{consumer = Consumer, channels = array:new()}, ?RTMP_TIMEOUT}.

wait_for_socket({socket, Socket}, #rtmp_socket{} = State) ->
  inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
  {ok, {IP, Port}} = inet:peername(Socket),
  {next_state, handshake_c1, State#rtmp_socket{socket = Socket, address = IP, port = Port}}.
  
% , previous_ack = erlang:now()
  
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

handle_sync_event(client_buffer, _From, loop, #rtmp_socket{client_buffer = ClientBuffer} = State) ->
  {reply, ClientBuffer, loop, State};

handle_sync_event(window_size, _From, loop, #rtmp_socket{window_size = WindowAckSize} = State) ->
  {reply, WindowAckSize, loop, State};

handle_sync_event(Event, _From, StateName, StateData) ->
  io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, got_sync_request2]),
  {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Data}, handshake_c1, #rtmp_socket{socket=Socket, buffer = Buffer} = State) when size(Buffer) + size(Data) < ?HS_BODY_LEN + 1 ->
  inet:setopts(Socket, [{active, once}]),
  {next_state, handshake_c1, State#rtmp_socket{buffer = <<Buffer/binary, Data/binary>>}, ?RTMP_TIMEOUT};
  
handle_info({tcp, Socket, Data}, handshake_c1, #rtmp_socket{socket=Socket, buffer = Buffer} = State) ->
  inet:setopts(Socket, [{active, once}]),
  <<?HS_HEADER, HandShake:?HS_BODY_LEN/binary, Rest/binary>> = <<Buffer/binary, Data/binary>>,
	Reply = rtmp:handshake(HandShake),
	send_data(State, [?HS_HEADER, Reply]),
	{next_state, 'handshake_c3', State#rtmp_socket{buffer = Rest}, ?RTMP_TIMEOUT};


handle_info({tcp, Socket, Data}, handshake_c3, #rtmp_socket{socket=Socket, buffer = Buffer} = State) when size(Buffer) + size(Data) < ?HS_BODY_LEN ->
  inet:setopts(Socket, [{active, once}]),
  {next_state, handshake_c3, State#rtmp_socket{buffer = <<Buffer/binary, Data/binary>>}, ?RTMP_TIMEOUT};
  
handle_info({tcp, Socket, Data}, handshake_c3, #rtmp_socket{socket=Socket, consumer = Consumer, buffer = Buffer} = State) ->
  inet:setopts(Socket, [{active, once}]),
  <<_HandShakeC3:?HS_BODY_LEN/binary, Rest/binary>> = <<Buffer/binary, Data/binary>>,
  Consumer ! {rtmp, self(), connected},
  {next_state, loop, handle_rtmp_data(State, Rest), ?RTMP_TIMEOUT};

handle_info({tcp, Socket, Data}, loop, #rtmp_socket{socket=Socket, buffer = Buffer} = State) ->
  inet:setopts(Socket, [{active, once}]),
  {next_state, loop, handle_rtmp_data(State, <<Buffer/binary, Data/binary>>), ?RTMP_TIMEOUT};

handle_info({tcp_closed, Socket}, _StateName, #rtmp_socket{socket = Socket, consumer = Consumer} = StateData) ->
  Consumer ! {rtmp, self(), disconnect},
  {stop, normal, StateData};

handle_info({server_data, Data}, StateName, State) ->
  send_data(State, Data),
  {next_state, StateName, State};

handle_info(_Info, StateName, StateData) ->
  ?D({"Some info handled", _Info, StateName, StateData}),
  {next_state, StateName, StateData, ?RTMP_TIMEOUT}.


send_data(#rtmp_socket{socket = Socket}, Data) when is_port(Socket) ->
  gen_tcp:send(Socket, Data);

send_data(#rtmp_socket{socket = Socket}, Data) when is_pid(Socket) ->
  gen_fsm:send_event(Socket, {server_data, Data}).


handle_rtmp_data(State, Data) ->
  handle_rtmp_message(rtmp:decode(Data, State)).

handle_rtmp_message({#rtmp_socket{consumer = Consumer} = State, Message, Rest}) ->
  Consumer ! Message,
  handle_rtmp_message(rtmp:decode(Rest, State));

handle_rtmp_message({State, Rest}) -> State#rtmp_socket{buffer = Rest}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #rtmp_socket{socket=Socket}) ->
  (catch gen_tcp:close(Socket)),
  ok.


%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVersion, _StateName, _State, _Extra) ->
  ok.
