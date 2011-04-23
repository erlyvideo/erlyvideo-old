%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTMP socket module.
%%% Designed to look like rtmp mode of usual TCP socket. If you have used {packet, http}, you will
%%% find many common behaviours.
%%% 
%%% When working on server side, you should accept Socket::port() with:
%%% <pre><code>
%%% {ok, RTMP} = rtmp_socket:accept(Socket).
%%% receive
%%%   {rtmp, RTMP, connected} ->
%%%     rtmp_socket:setopts(RTMP, [{active, once}]),
%%%     loop(RTMP)
%%% end
%%% loop(RTMP) ->
%%%   receive
%%%     {rtmp, RTMP, disconnect, Statistics} -> 
%%%       ok;
%%%     {rtmp, RTMP, #rtmp_message{} = Message} ->
%%%       io:format("Message: ~p~n", [Message]),
%%%       loop(RTMP)
%%%   end.
%%% </code></pre>
%%%
%%% You are strongly advised to use {active, once} mode, because in any other case it is very easy
%%% to crash whole your server with OOM killer.
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
-module(rtmp_socket).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/rtmp.hrl").
-include("rtmp_private.hrl").
-version(1.1).

-export([accept/1, connect/1, start_link/1, getopts/2, setopts/2, getstat/2, getstat/1, send/2, get_socket/1]).
-export([status/3, status/4, prepare_status/2, prepare_status/3, invoke/2, invoke/4, prepare_invoke/3, notify/4, prepare_notify/3]).

-export([start_socket/2, start_server/3, start_server/4, set_socket/2]).
  


%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([wait_for_socket_on_server/2, wait_for_socket_on_client/2, handshake_c1/2, handshake_c3/2, handshake_s1/2, loop/2, loop/3]).

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
  rtmp_sup:start_rtmp_listener(Port, Name, Callback, []).

start_server(Port, Name, Callback, Args) ->
  rtmp_sup:start_rtmp_listener(Port, Name, Callback, Args).


%% @spec (Socket::port()) -> {ok, RTMP::pid()}
%% @doc Accepts client connection on socket Socket, starts RTMP decoder, passes socket to it
%% and returns pid of newly created RTMP socket.
%% @end
-spec(accept(Socket::port()) -> {ok, RTMPSocket::pid()}).
accept(Socket) ->
  {ok, Pid} = start_socket(accept, Socket),
  setopts(Pid, [{consumer,self()}]),
  {ok,Pid}.
  
%% @spec (Socket::port()) -> {ok, RTMP::pid()}
%% @doc Accepts client connection on socket Socket, starts RTMP decoder, passes socket to it
%% and returns pid of newly created RTMP socket.
%% @end
-spec(connect(Socket::port()|string()) -> {ok, RTMPSocket::pid()}).
connect(ServerSpec) when is_list(ServerSpec) ->
  {_Proto,_Auth,Host,Port,_Path,_Query} = http_uri2:parse(ServerSpec),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}, {packet, raw}]),
  connect(Socket);

connect(Socket) when is_port(Socket) ->
  {ok, Pid} = start_socket(connect, Socket),
  setopts(Pid, [{consumer,self()}]),
  {ok,Pid}.

%% @spec (Type::accept|connect, Socket::port) -> {ok, RTMP::pid()}
%% @doc Starts RTMP socket with provided consumer and inititiate server or client connection
%% @end
-spec(start_socket(Type::connect|accept, Socket::port()) -> {ok, RTMP::pid()}).
start_socket(Type, Socket) ->
  {ok, RTMP} = rtmp_sup:start_rtmp_socket(Type),
  case Socket of
    _ when is_port(Socket) -> gen_tcp:controlling_process(Socket, RTMP);
    _ -> ok
  end,
  set_socket(RTMP, Socket),
  {ok, RTMP}.


set_socket(RTMP, Socket) ->
  gen_fsm:send_event(RTMP, {socket, Socket}).

get_socket(RTMP) ->
  gen_fsm:sync_send_all_state_event(RTMP, get_socket, ?RTMP_TIMEOUT).
  
  
%% @private
start_link(Type) ->
  gen_fsm:start_link(?MODULE, [Type], []).
  
  
%% @spec (RTMP::pid(), Options::[{Key, Value}]|{Key, Value}) -> ok
%% @doc Just the same as {@link inet:setopts/2. inet:setopts/2} this function changes state of 
%% rtmp socket.<br/>
%%  Available options:
%%  <ul><li><code>chunk_size</code> - change outgoing chunk size</li>
%%  <li><code>window_size</code> - ask remote client to send read acknowlegement after WindowSize bytes</li>
%%  <li><code>amf_version</code> - change AMF0 to AMF3, but only before first call</li>
%%  <li><code>consumer</code> - change messages consumer</li>
%%  <li><code>debug = true|false</code> - dump all important packets or no</li>
%% </ul>
%% @end
-spec(setopts(RTMP::rtmp_socket_pid(), Options::[{Key::atom(), Value::any()}]) -> ok).
setopts(RTMP, Options) ->
  gen_fsm:sync_send_all_state_event(RTMP, {setopts, Options}).

%% @spec (RTMP::pid(), Options::[{Key, Value}]|{Key, Value}) -> ok
%% @doc Just the same as {@link inet:getopts/2. inet:getopts/2} this function gets state of 
%% rtmp socket.<br/>
%%  Available options:
%%  <ul>
%%  <li><code>chunk_size</code> - get outgoing chunk size</li>
%%  <li><code>window_size</code> - get remote client read acknowlegement window size bytes</li>
%%  <li><code>amf_version</code> - get AMF0 or AMF3</li>
%%  <li><code>consumer</code> - get messages consumer</li>
%%  <li><code>address</code> - get remote client IP and port</li>
%% </ul>
%% @end
-spec(getopts(RTMP::rtmp_socket_pid(), Options::[Key::atom()]) -> ok).
getopts(RTMP, Options) ->
  gen_fsm:sync_send_all_state_event(RTMP, {getopts, Options}, ?RTMP_TIMEOUT).

%% @spec (RTMP::pid(), Stats::[Key]) -> Values::[{Key,Value}]
%% @doc Just the same as {@link inet:getstats/2. inet:getstats/2} this function gets statistics of 
%% rtmp socket.<br/>
%%  Available options:
%%  <ul>
%%  <li><code>recv_oct</code> - number of bytes received to the socket.</li>
%%  <li><code>send_oct</code> - number of bytes sent from the socket</li>
%% </ul>
%% @end
-spec(getstat(RTMP::rtmp_socket_pid(), Options::[Key::atom()]) -> ok).
getstat(RTMP, Options) ->
  gen_fsm:sync_send_all_state_event(RTMP, {getstat, Options}, ?RTMP_TIMEOUT).

%% @spec (RTMP::pid()) -> Values::[{Key,Value}]
%% @doc Just the same as {@link inet:getstats/1. inet:getstats/1} this function gets statistics of 
%% rtmp socket.<br/>
-spec(getstat(RTMP::rtmp_socket_pid()) -> ok).
getstat(RTMP) ->
  gen_fsm:sync_send_all_state_event(RTMP, getstat, ?RTMP_TIMEOUT).

  
%% @spec (RTMP::pid(), Message::rtmp_message()) -> ok
%% @doc Sends message to client.
%% @end
-spec(send(RTMP::rtmp_socket_pid(), Message::rtmp_message()) -> ok).
send(RTMP, Message) ->
  % io:format("Message ~p ~p ~p~n", [Message#rtmp_message.type, Message#rtmp_message.timestamp, Message#rtmp_message.stream_id]),
  % case process_info(RTMP, message_queue_len) of
  %   {message_queue_len, Length} when Length < 20 -> RTMP ! Message;
  %   {message_queue_len, Length} -> gen_fsm:sync_send_event(RTMP, Message, ?RTMP_TIMEOUT);
  %   _ -> ok
  % end,
  RTMP ! Message,
  ok.
  


notify(RTMP, StreamId, Name, Args) ->
  send(RTMP, prepare_notify(StreamId, Name, Args)).

prepare_notify(StreamId, Name, Args) ->
  Arg = {object, lists:ukeymerge(1, [{level, <<"status">>}], lists:keysort(1, Args))},
  #rtmp_message{type = metadata, channel_id = rtmp_lib:channel_id(audio, StreamId), stream_id = StreamId, body = [Name, Arg], timestamp = same}.


prepare_status(StreamId, Code) when is_list(Code) ->
  prepare_status(StreamId, list_to_binary(Code), <<"-">>);

prepare_status(StreamId, Code) when is_binary(Code) ->
  prepare_status(StreamId, Code, <<"-">>).


-spec(prepare_status(StreamId::non_neg_integer(), Code::any_string(), Description::any_string()) -> rtmp_message()).
prepare_status(StreamId, Code, Description) ->
  Arg = {object, [
    {code, Code}, 
    {level, <<"status">>}, 
    {description, Description}
  ]},
  prepare_invoke(StreamId, onStatus, [Arg]).


status(RTMP, StreamId, Code, Description) ->
  send(RTMP, prepare_status(StreamId, Code, Description)).

-spec(status(RTMP::rtmp_socket_pid(), StreamId::integer(), Code::any_string()) -> ok).
status(RTMP, StreamId, Code) when is_list(Code) ->
  status(RTMP, StreamId, list_to_binary(Code), <<"-">>);

status(RTMP, StreamId, Code) when is_binary(Code) ->
  status(RTMP, StreamId, Code, <<"-">>).

  
prepare_invoke(StreamId, Command, Args) when is_integer(StreamId) ->
  AMF = #rtmp_funcall{
      command = Command,
        type = invoke,
        id = 0,
        stream_id = StreamId,
        args = [null | Args ]},
  #rtmp_message{stream_id = StreamId, type = invoke, body = AMF}.


invoke(RTMP, StreamId, Command, Args) ->
  send(RTMP, prepare_invoke(StreamId, Command, Args)).

invoke(RTMP, #rtmp_funcall{stream_id = StreamId} = AMF) ->
  send(RTMP, #rtmp_message{stream_id = StreamId, type = invoke, body = AMF}).


  
%% @private  
init([accept]) ->
  {ok, wait_for_socket_on_server, #rtmp_socket{channels = {}, out_channels = {}, active = false}, ?RTMP_TIMEOUT};

init([connect]) ->
  {ok, wait_for_socket_on_client, #rtmp_socket{channels = {}, out_channels = {}, active = false}, ?RTMP_TIMEOUT}.



%% @private 

wait_for_socket_on_server(timeout, State) ->
  {stop, normal, State};

wait_for_socket_on_server({socket, Socket}, #rtmp_socket{} = State) when is_port(Socket) ->
  case inet:peername(Socket) of
    {ok, {IP, Port}} ->
      ok =  inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
      {next_state, handshake_c1, State#rtmp_socket{socket = Socket, address = IP, port = Port}, ?RTMP_TIMEOUT};
    {error, _Error} ->
      {stop, normal, State}
  end;
      
wait_for_socket_on_server({socket, Socket}, #rtmp_socket{} = State) when is_pid(Socket) ->
  erlang:monitor(process, Socket),
  {next_state, handshake_c1, State#rtmp_socket{socket = Socket}, ?RTMP_TIMEOUT}.


%% @private  
-spec(wait_for_socket_on_client(Message::any(), Socket::rtmp_socket()) -> no_return()).

wait_for_socket_on_client(timeout, State) ->
  {stop, normal, State};

wait_for_socket_on_client({socket, Socket}, #rtmp_socket{} = State) ->
  inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
  {ok, {IP, Port}} = inet:peername(Socket),
  State1 = State#rtmp_socket{socket = Socket, address = IP, port = Port},
  send_data(State1, rtmp_handshake:c1()),
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
  State1 = send_data(State, #rtmp_message{type = ping}),
  {next_state, loop, State1#rtmp_socket{pinged = true}, ?RTMP_TIMEOUT};
  
loop(timeout, #rtmp_socket{consumer = Consumer} = State) ->
  Consumer ! {rtmp, self(), timeout},
  {stop, normal, State}.


%% @private
loop(#rtmp_message{} = Message, _From, State) ->
  State1 = send_data(State, Message),
  {reply, ok, loop, State1, ?RTMP_TIMEOUT}.


-type(rtmp_option() ::active|amf_version|chunk_size|window_size|client_buffer|address).
% -type(rtmp_option_value() ::{rtmp_option(), any()}).
-spec(get_options(State::rtmp_socket(), Key::rtmp_option()|[rtmp_option()]) -> term()).

get_options(State, active) ->
  {active, State#rtmp_socket.active};

get_options(State, amf_version) ->
  {amf_version, State#rtmp_socket.amf_version};

get_options(State, chunk_size) ->
  {chunk_size, State#rtmp_socket.server_chunk_size};

get_options(State, window_size) ->
  {window_size, State#rtmp_socket.window_size};

get_options(State, client_buffer) ->
  {client_buffer, State#rtmp_socket.client_buffer};

get_options(State, debug) ->
  {debug, State#rtmp_socket.debug};

get_options(State, address) ->
  {address, {State#rtmp_socket.address, State#rtmp_socket.port}};

get_options(_State, []) ->
  [];
  
get_options(State, [Key | Options]) ->
  [get_options(State, Key) | get_options(State, Options)].

get_stat(State) ->
  get_stat(State, [recv_oct, send_oct]).

get_stat(State, recv_oct) ->
  {recv_oct, State#rtmp_socket.bytes_read};

get_stat(State, send_oct) ->
  {send_oct, State#rtmp_socket.bytes_sent};

get_stat(_State, []) ->
  [];

get_stat(State, [Key | Options]) ->
  [get_stat(State, Key) | get_stat(State, Options)].

set_options(State, [{amf_version, Version} | Options]) ->
  set_options(State#rtmp_socket{amf_version = Version}, Options);

set_options(#rtmp_socket{socket = Socket, buffer = Data} = State, [{active, Active} | Options]) ->
  State1 = flush_send(State#rtmp_socket{active = Active}),
  State2 = case Active of
    false -> 
      inet:setopts(Socket, [{active, false}]),
      State1;
    true ->
      inet:setopts(Socket, [{active, true}]),
      State1;
    once when size(Data) > 0 ->
      handle_rtmp_data(State1);
    once ->
      activate_socket(Socket),
      State1
  end,
  set_options(State2, Options);

set_options(#rtmp_socket{} = State, [{debug, Debug} | Options]) ->
  io:format("Set debug to ~p~n", [Debug]),
  set_options(State#rtmp_socket{debug = Debug}, Options);

set_options(#rtmp_socket{consumer = undefined} = State, [{consumer, Consumer} | Options]) ->
  erlang:monitor(process, Consumer),
  set_options(State#rtmp_socket{consumer = Consumer}, Options);

set_options(State, [{chunk_size, ChunkSize} | Options]) ->
  State1 = send_data(State, #rtmp_message{type = chunk_size, body = ChunkSize}),
  set_options(State1#rtmp_socket{server_chunk_size = ChunkSize}, Options);

set_options(State, []) -> State.
  
%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event({setopts, Options}, StateName, State) ->
  NewState = set_options(State, Options),
  {next_state, StateName, NewState, ?RTMP_TIMEOUT};

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

handle_sync_event({getopts, Options}, _From, StateName, State) ->
  {reply, get_options(State, Options), StateName, State, ?RTMP_TIMEOUT};

handle_sync_event({getstat, Options}, _From, StateName, State) ->
  {reply, get_stat(State, Options), StateName, State, ?RTMP_TIMEOUT};

handle_sync_event(getstat, _From, StateName, State) ->
  {reply, get_stat(State), StateName, State, ?RTMP_TIMEOUT};

handle_sync_event(get_socket, _From, StateName, #rtmp_socket{socket = Socket} = State) ->
  {reply, Socket, StateName, State, ?RTMP_TIMEOUT};

handle_sync_event({setopts, Options}, _From, StateName, State) ->
  NewState = set_options(State, Options),
  {reply, ok, StateName, NewState, ?RTMP_TIMEOUT};

handle_sync_event(Event, _From, StateName, StateData) ->
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
  <<Handshake:(?HS_BODY_LEN+1)/binary, Rest/binary>> = <<Buffer/binary, Data/binary>>,
  State1 = case rtmp_handshake:server(Handshake) of
    {uncrypted, Reply} -> 
  	  send_data(State, Reply);
    {crypted, Reply, KeyIn, KeyOut} ->
  	  NewState = send_data(State, Reply),
  	  ?D({"Established RTMPE connection"}),
      NewState#rtmp_socket{key_in = KeyIn, key_out = KeyOut}
  end,
	{next_state, 'handshake_c3', State1#rtmp_socket{buffer = Rest, bytes_read = BytesRead + size(Data)}, ?RTMP_TIMEOUT};


handle_info({tcp, Socket, Data}, handshake_c3, #rtmp_socket{socket=Socket, buffer = Buffer, bytes_read = BytesRead} = State) when size(Buffer) + size(Data) < ?HS_BODY_LEN ->
  activate_socket(Socket),
  {next_state, handshake_c3, State#rtmp_socket{buffer = <<Buffer/binary, Data/binary>>, bytes_read = BytesRead + size(Data)}, ?RTMP_TIMEOUT};
  
handle_info({tcp, Socket, Data}, handshake_c3, #rtmp_socket{socket=Socket, consumer = Consumer, buffer = Buffer, bytes_read = BytesRead, active = Active, key_in = KeyIn} = State) ->
  <<_HandShakeC3:?HS_BODY_LEN/binary, CryptedData/binary>> = <<Buffer/binary, Data/binary>>,
  Consumer ! {rtmp, self(), connected},
  case Active of
    true -> inet:setopts(Socket, [{active, true}]);
    _ -> ok
  end,

  {NewKeyIn, Rest} = case KeyIn of
    undefined -> {undefined, CryptedData};
    _ -> rtmpe:crypt(KeyIn, CryptedData)
  end,
  % ?D({decrypt, Rest, CryptedData == element(2, rtmpe:crypt(NewKeyIn, Rest))}),
  
  State1 = State#rtmp_socket{bytes_read = BytesRead + size(Data), key_in = NewKeyIn, buffer = Rest},
  case Rest of
    <<>> -> {next_state, loop, State1, ?RTMP_TIMEOUT};
    _ -> {next_state, loop, handle_rtmp_data(State1), ?RTMP_TIMEOUT}
  end;

handle_info({tcp, Socket, Data}, handshake_s1, #rtmp_socket{socket=Socket, buffer = Buffer, bytes_read = BytesRead} = State) when size(Buffer) + size(Data) < ?HS_BODY_LEN*2 + 1 ->
  activate_socket(Socket),
  {next_state, handshake_s1, State#rtmp_socket{buffer = <<Buffer/binary, Data/binary>>, bytes_read = BytesRead + size(Data)}, ?RTMP_TIMEOUT};

handle_info({tcp, Socket, Data}, handshake_s1, #rtmp_socket{socket=Socket, consumer = Consumer, buffer = Buffer, bytes_read = BytesRead, active = Active} = State) ->
  <<?HS_UNCRYPTED, S1:?HS_BODY_LEN/binary, _S2:?HS_BODY_LEN/binary, Rest/binary>> = <<Buffer/binary, Data/binary>>,
  send_data(State, rtmp_handshake:c2(S1)),
  Consumer ! {rtmp, self(), connected},
  case Active of
    true -> inet:setopts(Socket, [{active, true}]);
    _ -> ok
  end,
  
  {next_state, loop, handle_rtmp_data(State#rtmp_socket{bytes_read = BytesRead + size(Data), buffer = Rest}), ?RTMP_TIMEOUT};



handle_info({tcp, Socket, CryptedData}, loop, #rtmp_socket{socket=Socket, buffer = Buffer, bytes_read = BytesRead, bytes_unack = BytesUnack, key_in = KeyIn} = State) ->
  State1 = flush_send(State),
  {NewKeyIn, Data} = case KeyIn of
    undefined -> {undefined, CryptedData};
    _ -> rtmpe:crypt(KeyIn, CryptedData)
  end,
  {next_state, loop, handle_rtmp_data(State1#rtmp_socket{bytes_read = BytesRead + size(Data), bytes_unack = BytesUnack + size(Data), key_in = NewKeyIn, buffer = <<Buffer/binary, Data/binary>>}), ?RTMP_TIMEOUT};

handle_info({tcp_closed, Socket}, _StateName, #rtmp_socket{socket = Socket, consumer = Consumer} = StateData) ->
  Consumer ! {rtmp, self(), disconnect, get_stat(StateData)},
  {stop, normal, StateData};

handle_info(#rtmp_message{} = Message, loop, State) ->
  State1 = send_data(State, Message),
  State2 = flush_send(State1),
  {next_state, loop, State2, ?RTMP_TIMEOUT};

handle_info({rtmpt, RTMPT, alive}, StateName, #rtmp_socket{socket = RTMPT} = State) ->
  {next_state, StateName, State#rtmp_socket{pinged = false}, ?RTMP_TIMEOUT};

handle_info({rtmpt, RTMPT, Data}, StateName, State) ->
  handle_info({tcp, RTMPT, Data}, StateName, State);

handle_info({'DOWN', _, process, _Client, _Reason}, _StateName, State) ->
  {stop, normal, State};

handle_info(_Info, StateName, StateData) ->
  error_logger:error_msg("Unknown message to rtmp socket: ~p ~p ~p~n", [_Info, StateName, StateData]),
  {next_state, StateName, StateData, ?RTMP_TIMEOUT}.

% flush_send(State) -> flush_send([], State).

flush_send(State) ->
  receive
    #rtmp_message{} = Message ->
      NewState = send_data(State, Message),
      % ?D({out, Message}),
      % {NewState, Data} = rtmp:encode(State, Message),
      % flush_send([Data | Packet], NewState)
      flush_send(NewState)
  after
    0 -> State
  end.
  
activate_socket(Socket) when is_port(Socket) ->
  inet:setopts(Socket, [{active, once}]);
activate_socket(Socket) when is_pid(Socket) ->
  ok.
  

send_data(#rtmp_socket{sent_audio_notify = false} = Socket, 
          #rtmp_message{type = audio, timestamp = DTS, stream_id = StreamId, body = Body} = Message) when size(Body) > 0 ->
  State1 = send_data(Socket#rtmp_socket{sent_audio_notify = true}, rtmp_lib:empty_audio(StreamId, DTS)),
  send_data(State1, Message#rtmp_message{ts_type = new});

send_data(#rtmp_socket{sent_video_notify = false} = Socket, #rtmp_message{type = video, timestamp = DTS, stream_id = StreamId} = Message) ->
  Msg = [
    #rtmp_message{type = video, channel_id = rtmp_lib:channel_id(video, StreamId), timestamp = DTS, stream_id = StreamId, body = <<87,0>>, ts_type = new},
    #rtmp_message{type = video, channel_id = rtmp_lib:channel_id(video, StreamId), timestamp = DTS, stream_id = StreamId, body = <<23,2,0,0,0>>, ts_type = new},
    #rtmp_message{type = video, channel_id = rtmp_lib:channel_id(video, StreamId), timestamp = DTS, stream_id = StreamId, body = <<87,1>>, ts_type = delta}
  ],
  
  State2 = lists:foldl(fun(M, State1) ->
    send_data(State1, M)
  end, Socket#rtmp_socket{sent_video_notify = true}, Msg),
  send_data(State2, Message);
  
send_data(#rtmp_socket{sent_audio_notify = true} = Socket, #rtmp_message{type = stream_end} = Message) ->
  send_data(Socket#rtmp_socket{sent_audio_notify = false}, Message);

send_data(#rtmp_socket{sent_video_notify = true} = Socket, #rtmp_message{type = stream_end} = Message) ->
  send_data(Socket#rtmp_socket{sent_video_notify = false}, Message);

send_data(#rtmp_socket{socket = Socket, key_out = KeyOut} = State, Message) ->
  case State#rtmp_socket.debug of
    true -> print_rtmp_message(out, Message);
    _ -> ok
  end,
  {NewState, Data} = case Message of
    #rtmp_message{} ->
      % ?D({Socket,Message#rtmp_message.type, Message#rtmp_message.timestamp, Message#rtmp_message.ts_type}),
      rtmp:encode(State, Message);
    _ -> 
      {State, Message}
  end,
  {NewKeyOut, Crypt} = case KeyOut of
    undefined -> {undefined, Data};
    _ -> rtmpe:crypt(KeyOut, Data)
  end,
  % (catch rtmp_stat_collector:out_bytes(self(), iolist_size(Crypt))),
  if
    is_port(Socket) ->
      gen_tcp:send(Socket, Crypt);
    is_pid(Socket) ->
      rtmpt:write(Socket, Crypt)
  end,
  NewState#rtmp_socket{key_out = NewKeyOut}.



handle_rtmp_data(#rtmp_socket{buffer = <<>>} = State) ->
  % ?D({empty_input, erlang:get_stacktrace()}),
  State;

handle_rtmp_data(#rtmp_socket{bytes_unack = Bytes, window_size = Window} = State) when Bytes >= Window ->
  State1 = send_data(State, #rtmp_message{type = ack_read}),
  handle_rtmp_data(State1);

handle_rtmp_data(#rtmp_socket{buffer = Data} = State) ->
  got_rtmp_message(rtmp:decode(State, Data)).


print_rtmp_message(InOut, #rtmp_message{channel_id = Channel, ts_type = TSType, timestamp = TS, type = Type, stream_id = StreamId, body = Body}) ->
  DecodedBody = case Type of
    video when size(Body) > 10 -> erlang:setelement(5, flv:decode_video_tag(Body), size(Body));
    audio when size(Body) > 0 -> erlang:setelement(7, flv:decode_audio_tag(Body), size(Body));
    _ -> Body
  end,
  io:format("~p ~p ~p ~p ~p ~p ~p~n", [InOut, Channel, TSType, TS, Type, StreamId, DecodedBody]),
  ok;

print_rtmp_message(InOut, Msg) ->
  io:format("~p ~s~n", [InOut, io_lib_pretty_limited:print(Msg, 20)]),
  ok.
  


got_rtmp_message({#rtmp_socket{debug = true}, Msg, _} = Message) ->
  print_rtmp_message(in, Msg),
  handle_rtmp_message(Message);

got_rtmp_message(Decoded) ->
  handle_rtmp_message(Decoded).


handle_rtmp_message({#rtmp_socket{consumer = Consumer} = State, #rtmp_message{type = window_size, body = Size} = Message, Rest}) ->
  Consumer ! {rtmp, self(), Message},
  rtmp_message_sent(State#rtmp_socket{window_size = Size, buffer = Rest});

handle_rtmp_message({#rtmp_socket{consumer = Consumer, pinged = true} = State, #rtmp_message{type = pong} = Message, Rest}) ->
  Consumer ! {rtmp, self(), Message},
  rtmp_message_sent(State#rtmp_socket{pinged = false, buffer = Rest});

handle_rtmp_message({#rtmp_socket{consumer = Consumer} = State, #rtmp_message{type = ping, body = Timestamp} = Message, Rest}) ->
  Consumer ! {rtmp, self(), Message},
  send_data(State, #rtmp_message{type = pong, body = Timestamp}),
  rtmp_message_sent(State#rtmp_socket{buffer = Rest});

handle_rtmp_message({#rtmp_socket{consumer = Consumer} = State, Message, Rest}) ->
  Consumer ! {rtmp, self(), Message},
  rtmp_message_sent(State#rtmp_socket{buffer = Rest});

handle_rtmp_message({#rtmp_socket{socket=Socket} = State, Rest}) -> 
  activate_socket(Socket),
  State#rtmp_socket{buffer = Rest}.

rtmp_message_sent(#rtmp_socket{active = true,socket = Socket} = State) ->
  activate_socket(Socket),
  handle_rtmp_data(State);

rtmp_message_sent(#rtmp_socket{active = _} = State) ->
  handle_rtmp_data(State);

rtmp_message_sent(State) -> 
  State.


%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #rtmp_socket{socket=Socket}) when is_pid(Socket) ->
  gen_server:cast(Socket, close),
  ok;
  
terminate(_Reason, _StateName, _State) ->
  ok.


%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVersion, _StateName, _State, _Extra) ->
  ok.
