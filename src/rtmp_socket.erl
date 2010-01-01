%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        RTMP socket module
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information
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
-module(rtmp_socket).
-author(max@maxidoors.ru).
-include("../include/rtmp.hrl").
-include("rtmp_private.hrl").
-version(1.0).

-export([accept/1, connect/1, start_link/2, getopts/2, setopts/2, send/2]).
-export([status/3, status/4, invoke/2]).

-export([start_socket/3]).


-export([start_server/3]).
  


%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([wait_for_socket_on_server/2, wait_for_socket_on_server/3, wait_for_socket_on_client/2, handshake_c1/2, handshake_c3/2, handshake_s1/2, loop/2, loop/3]).

%% @spec (Port::integer(), Name::atom(), Callback::atom()) -> {ok, Pid::pid()}
%% @doc Starts RTMP listener on port Port, registered under name Name with callback module Callback.
%% Callback must export one function: create_client/1
%% create_client(RTMPSocket::pid()) -> {ok, Pid::pid()}
%%
%% This function receives RTMPSocket, that is ready to send messages and after this callback function returns, this socket
%% will send rtmp_message as it is defined in overview.
%% @end
-spec(start_server(Port::integer(), Name::atom(), Callback::atom()) -> {ok, Pid::pid()}).
start_server(Port, Name, Callback) ->
  rtmp_sup:start_rtmp_listener(Port, Name, Callback).


%% @spec (Socket::port()) -> {ok, RTMP::pid()}
%% @doc Accepts client connection on socket Socket, starts RTMP decoder, passes socket to it
%% and returns pid of newly created RTMP socket.
%% @end
-spec(accept(Socket::port()) -> RTMPSocket::pid()).
accept(Socket) ->
  start_socket(self(), accept, Socket).
  
%% @spec (Socket::port()) -> {ok, RTMP::pid()}
%% @doc Accepts client connection on socket Socket, starts RTMP decoder, passes socket to it
%% and returns pid of newly created RTMP socket.
%% @end
-spec(connect(Socket::port()) -> RTMPSocket::pid()).
connect(Socket) ->
  start_socket(self(), connect, Socket).

%% @spec (Consumer::pid(), Type::accept|connect, Socket::port) -> {ok, RTMP::pid()}
%% @doc Starts RTMP socket with provided consumer and inititiate server or client connection
%% @end
-spec(start_socket(Consumer::pid(), Type::connect|accept, Socket::port()) -> {ok, RTMP::pid()}).
start_socket(Consumer, Type, Socket) ->
  {ok, RTMP} = rtmp_sup:start_rtmp_socket(Consumer, Type),
  case Socket of
    _ when is_port(Socket) -> gen_tcp:controlling_process(Socket, RTMP);
    _ -> ok
  end,
  gen_fsm:send_event(RTMP, {socket, Socket}),
  {ok, RTMP}.

  
%% @private
start_link(Consumer, Type) ->
  gen_fsm:start_link(?MODULE, [Consumer, Type], []).
  
  
%% @spec (RTMP::pid(), Options::[{Key, Value}]|{Key, Value}) -> ok
%% @doc Just the same as {@link inet:setopts/2. inet:setopts/2} this function changes state of 
%% rtmp socket.<br/>
%%  Available options:
%%  <ul><li><code>chunk_size</code> - change outgoing chunk size</li>
%%  <li><code>window_size</code> - ask remote client to send read acknowlegement after WindowSize bytes</li>
%%  <li><code>amf_version</code> - change AMF0 to AMF3, but only before first call</li>
%%  <li><code>consumer</code> - change messages consumer</li>
%% </ul>
%% @end
-spec(setopts(RTMP::rtmp_socket_pid(), Options::[{Key::atom(), Value::any()}]) -> ok).
setopts(RTMP, Options) ->
  gen_fsm:send_event(RTMP, {setopts, Options}).

-spec(getopts(RTMP::rtmp_socket_pid(), Options::[Key::atom()]) -> ok).
getopts(RTMP, Options) ->
  gen_fsm:sync_send_event(RTMP, {getopts, Options}).

  
-spec(send(RTMP::rtmp_socket_pid(), Message::rtmp_message()) -> ok).
send(RTMP, Message) ->
  RTMP ! Message,
  ok.
  

-spec(status(RTMP::rtmp_socket_pid(), StreamId::integer(), Code::string()) -> ok).
status(RTMP, StreamId, Code) when is_list(Code)->
  status(RTMP, StreamId, list_to_binary(Code), <<"-">>).


-spec(status(RTMP::rtmp_socket_pid(), StreamId::integer(), Code::string(), Description::string()) -> ok).
status(RTMP, StreamId, Code, Description) when is_list(Code)->
  status(RTMP, StreamId, list_to_binary(Code), Description);

status(RTMP, StreamId, Code, Description) when is_list(Description)->
  status(RTMP, StreamId, Code, list_to_binary(Description));
  
  
status(RTMP, StreamId, Code, Description) ->
  Arg = {object, [
    {code, Code}, 
    {level, <<"status">>}, 
    {description, Description}
  ]},
  invoke(RTMP, StreamId, onStatus, [Arg]).
  
invoke(RTMP, StreamId, Command, Args) ->
  AMF = #rtmp_funcall{
      command = Command,
        type = invoke,
        id = 0,
        stream_id = StreamId,
        args = [null | Args ]},
  send(RTMP, #rtmp_message{stream_id = StreamId, type = invoke, body = AMF}).

invoke(RTMP, #rtmp_funcall{stream_id = StreamId} = AMF) ->
  send(RTMP, #rtmp_message{stream_id = StreamId, type = invoke, body = AMF}).
  
%% @private  
init([Consumer, accept]) ->
  (catch link(Consumer)),
  {ok, wait_for_socket_on_server, #rtmp_socket{consumer = Consumer, channels = array:new()}, ?RTMP_TIMEOUT};

init([Consumer, connect]) ->
  (catch link(Consumer)),
  {ok, wait_for_socket_on_client, #rtmp_socket{consumer = Consumer, channels = array:new()}, ?RTMP_TIMEOUT}.

%% @private  
wait_for_socket_on_server({socket, Socket}, #rtmp_socket{} = State) when is_port(Socket) ->
  inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
  {ok, {IP, Port}} = inet:peername(Socket),
  {next_state, handshake_c1, State#rtmp_socket{socket = Socket, address = IP, port = Port}, ?RTMP_TIMEOUT};

wait_for_socket_on_server({socket, Socket}, #rtmp_socket{} = State) when is_pid(Socket) ->
  link(Socket),
  {next_state, handshake_c1, State#rtmp_socket{socket = Socket}, ?RTMP_TIMEOUT};

wait_for_socket_on_server({setopts, Options}, State) ->
  NewState = set_options(State, Options),
  {next_state, wait_for_socket_on_server, NewState, ?RTMP_TIMEOUT}.
% , previous_ack = erlang:now()


%% @private  
wait_for_socket_on_server({getopts, Options}, _From, State) ->
  {reply, get_options(State, Options), wait_for_socket_on_server, State}.


%% @private  
wait_for_socket_on_client({socket, Socket}, #rtmp_socket{} = State) ->
  inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
  {ok, {IP, Port}} = inet:peername(Socket),
  State1 = State#rtmp_socket{socket = Socket, address = IP, port = Port},
  send_data(State1, [?HS_HEADER, rtmp_handshake:c1()]),
  {next_state, handshake_s1, State1, ?RTMP_TIMEOUT}.

%% @private  
handshake_c1(timeout, State) ->
  {stop, normal, State}.

%% @private  
handshake_c3(timeout, State) ->
  {stop, normal, State}.

%% @private  
handshake_s1(timeout, State) ->
  {stop, normal, State}.
  
%% @private  
loop(timeout, #rtmp_socket{pinged = false} = State) ->
  send_data(State, #rtmp_message{type = ping}),
  {next_state, loop, State#rtmp_socket{pinged = true}, ?RTMP_TIMEOUT};
  
loop(timeout, #rtmp_socket{consumer = Consumer} = State) ->
  Consumer ! {rtmp, self(), timeout},
  {stop, normal, State};

loop({setopts, Options}, State) ->
  NewState = set_options(State, Options),
  {next_state, loop, NewState, ?RTMP_TIMEOUT}.
% , previous_ack = erlang:now()


%% @private  
loop({getopts, Options}, _From, State) ->
  {reply, get_options(State, Options), loop, State}.



get_options(State, amf_version) ->
  {amf_version, State#rtmp_socket.amf_version};

get_options(State, chunk_size) ->
  {chunk_size, State#rtmp_socket.server_chunk_size};

get_options(State, window_size) ->
  {window_size, State#rtmp_socket.window_size};

get_options(State, client_buffer) ->
  {client_buffer, State#rtmp_socket.client_buffer};
  
get_options(State, [Key | Options]) ->
  [get_options(State, Key) | get_options(State, Options)].

set_options(State, [{amf_version, Version} | Options]) ->
  set_options(State#rtmp_socket{amf_version = Version}, Options);

set_options(#rtmp_socket{consumer = PrevConsumer} = State, [{consumer, Consumer} | Options]) ->
  (catch unlink(PrevConsumer)),  
  (catch link(Consumer)),  
  set_options(State#rtmp_socket{consumer = Consumer}, Options);

set_options(State, [{chunk_size, ChunkSize} | Options]) ->
  send_data(State, #rtmp_message{type = chunk_size, body = ChunkSize}),
  set_options(State#rtmp_socket{server_chunk_size = ChunkSize}, Options);

set_options(State, []) -> State.
  
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
  io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, got_sync_request2]),
  {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Data}, handshake_c1, #rtmp_socket{socket=Socket, buffer = Buffer, bytes_read = BytesRead} = State) when size(Buffer) + size(Data) < ?HS_BODY_LEN + 1 ->
  activate_socket(Socket),
  {next_state, handshake_c1, State#rtmp_socket{buffer = <<Buffer/binary, Data/binary>>, bytes_read = BytesRead + size(Data)}, ?RTMP_TIMEOUT};
  
handle_info({tcp, Socket, Data}, handshake_c1, #rtmp_socket{socket=Socket, buffer = Buffer, bytes_read = BytesRead} = State) ->
  activate_socket(Socket),
  <<?HS_HEADER, Handshake:?HS_BODY_LEN/binary, Rest/binary>> = <<Buffer/binary, Data/binary>>,
	send_data(State, [?HS_HEADER, rtmp_handshake:s1(), rtmp_handshake:s2(Handshake)]),
	{next_state, 'handshake_c3', State#rtmp_socket{buffer = Rest, bytes_read = BytesRead + size(Data)}, ?RTMP_TIMEOUT};


handle_info({tcp, Socket, Data}, handshake_c3, #rtmp_socket{socket=Socket, buffer = Buffer, bytes_read = BytesRead} = State) when size(Buffer) + size(Data) < ?HS_BODY_LEN ->
  activate_socket(Socket),
  {next_state, handshake_c3, State#rtmp_socket{buffer = <<Buffer/binary, Data/binary>>, bytes_read = BytesRead + size(Data)}, ?RTMP_TIMEOUT};
  
handle_info({tcp, Socket, Data}, handshake_c3, #rtmp_socket{socket=Socket, consumer = Consumer, buffer = Buffer, bytes_read = BytesRead} = State) ->
  activate_socket(Socket),
  <<_HandShakeC3:?HS_BODY_LEN/binary, Rest/binary>> = <<Buffer/binary, Data/binary>>,
  Consumer ! {rtmp, self(), connected},
  {next_state, loop, handle_rtmp_data(State#rtmp_socket{bytes_read = BytesRead + size(Data)}, Rest), ?RTMP_TIMEOUT};

handle_info({tcp, Socket, Data}, handshake_s1, #rtmp_socket{socket=Socket, buffer = Buffer, bytes_read = BytesRead} = State) when size(Buffer) + size(Data) < ?HS_BODY_LEN*2 + 1 ->
  activate_socket(Socket),
  {next_state, handshake_s1, State#rtmp_socket{buffer = <<Buffer/binary, Data/binary>>, bytes_read = BytesRead + size(Data)}, ?RTMP_TIMEOUT};

handle_info({tcp, Socket, Data}, handshake_s1, #rtmp_socket{socket=Socket, consumer = Consumer, buffer = Buffer, bytes_read = BytesRead} = State) ->
  activate_socket(Socket),
  <<?HS_HEADER, _S1:?HS_BODY_LEN/binary, S2:?HS_BODY_LEN/binary, Rest/binary>> = <<Buffer/binary, Data/binary>>,
  send_data(State, rtmp_handshake:c2(S2)),
  Consumer ! {rtmp, self(), connected},
  {next_state, loop, handle_rtmp_data(State#rtmp_socket{bytes_read = BytesRead + size(Data)}, Rest), ?RTMP_TIMEOUT};



handle_info({tcp, Socket, Data}, loop, #rtmp_socket{socket=Socket, buffer = Buffer, bytes_read = BytesRead} = State) ->
  activate_socket(Socket),
  {next_state, loop, handle_rtmp_data(State#rtmp_socket{bytes_read = BytesRead + size(Data)}, <<Buffer/binary, Data/binary>>), ?RTMP_TIMEOUT};

handle_info({tcp_closed, Socket}, _StateName, #rtmp_socket{socket = Socket, consumer = Consumer} = StateData) ->
  Consumer ! {rtmp, self(), disconnect},
  {stop, normal, StateData};

handle_info(#rtmp_message{} = Message, loop, State) ->
  {next_state, loop, send_data(State, Message), ?RTMP_TIMEOUT};

handle_info({rtmpt, RTMPT, alive}, StateName, #rtmp_socket{socket = RTMPT} = State) ->
  {next_state, StateName, State#rtmp_socket{pinged = false}, ?RTMP_TIMEOUT};

handle_info({rtmpt, RTMPT, Data}, StateName, State) ->
  handle_info({tcp, RTMPT, Data}, StateName, State);

handle_info(_Info, StateName, StateData) ->
  error_logger:error_msg("Unknown message to rtmp socket: ~p~n", [_Info]),
  {next_state, StateName, StateData, ?RTMP_TIMEOUT}.


activate_socket(Socket) when is_port(Socket) ->
  inet:setopts(Socket, [{active, once}]);
activate_socket(_Socket) -> ok.


send_data(State, #rtmp_message{} = Message) ->
  {NewState, Data} = rtmp:encode(State, Message),
  send_data(NewState, Data);
  
send_data(#rtmp_socket{socket = Socket} = State, Data) when is_port(Socket) ->
  gen_tcp:send(Socket, Data),
  State;

send_data(#rtmp_socket{socket = Socket} = State, Data) when is_pid(Socket) ->
  rtmpt:write(Socket, Data),
  State.


handle_rtmp_data(State, Data) ->
  handle_rtmp_message(rtmp:decode(State, Data)).

handle_rtmp_message({#rtmp_socket{consumer = Consumer, pinged = true} = State, #rtmp_message{type = pong} = Message, Rest}) ->
  Consumer ! {rtmp, self(), Message},
  handle_rtmp_message(rtmp:decode(State#rtmp_socket{pinged = false}, Rest));

handle_rtmp_message({#rtmp_socket{consumer = Consumer} = State, Message, Rest}) ->
  Consumer ! {rtmp, self(), Message},
  handle_rtmp_message(rtmp:decode(State, Rest));

handle_rtmp_message({State, Rest}) -> State#rtmp_socket{buffer = Rest}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #rtmp_socket{socket=Socket}) when is_pid(Socket) ->
  gen_server:cast(Socket, close),
  ok;
  
terminate(_Reason, _StateName, #rtmp_socket{socket=Socket}) when is_port(Socket) ->
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
