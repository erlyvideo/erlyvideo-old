%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
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
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/rtmp.hrl").
-include("rtmp_private.hrl").
-version(1.1).

-export([accept/1, connect/1, start_link/1, getopts/2, setopts/2, getstat/2, getstat/1, send/2]).
-export([status/3, status/4, prepare_status/2, prepare_status/3, invoke/2, invoke/4, prepare_invoke/3, notify/4, prepare_notify/3]).

-export([start_socket/2, start_server/3, set_socket/2]).
  


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
  rtmp_sup:start_rtmp_listener(Port, Name, Callback).


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
-spec(connect(Socket::port()) -> {ok, RTMPSocket::pid()}).
connect(Socket) ->
  {ok, Pid} = start_socket(connect, Socket),
  setopts(Pid, [{consumer,self()}]),
  {ok,Pid}.

%% @spec (Consumer::pid(), Type::accept|connect, Socket::port) -> {ok, RTMP::pid()}
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
  gen_fsm:send_all_state_event(RTMP, {setopts, Options}).

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




init_codec() ->
  Path = code:lib_dir(rtmp,ebin) ++ "/rtmp_codec_drv.so",
  case filelib:is_file(Path) of
    true ->
      case erl_ddll:load(code:lib_dir(rtmp,ebin), "rtmp_codec_drv") of
        ok -> 
          open_port({spawn, rtmp_codec_drv}, [binary]);
        {error, Error} ->
          error_logger:error_msg("Error loading ~p: ~p", [rtmp_codec_drv, erl_ddll:format_error(Error)]),
          undefined
      end;
    false ->
      undefined
  end.
  
%% @private  
init([accept]) ->
  {ok, wait_for_socket_on_server, #rtmp_socket{codec = init_codec(), channels = {}, out_channels = {}, active = false}, ?RTMP_TIMEOUT};

init([connect]) ->
  {ok, wait_for_socket_on_client, #rtmp_socket{codec = init_codec(), channels = {}, out_channels = {}, active = false}, ?RTMP_TIMEOUT}.



%% @private 

wait_for_socket_on_server(timeout, State) ->
  {stop, normal, State};

wait_for_socket_on_server({socket, Socket}, #rtmp_socket{} = State) when is_port(Socket) ->
  inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
  {ok, {IP, Port}} = inet:peername(Socket),
  {next_state, handshake_c1, State#rtmp_socket{socket = Socket, address = IP, port = Port}, ?RTMP_TIMEOUT};

wait_for_socket_on_server({socket, Socket}, #rtmp_socket{} = State) when is_pid(Socket) ->
  link(Socket),
  {next_state, handshake_c1, State#rtmp_socket{socket = Socket}, ?RTMP_TIMEOUT}.


%% @private  
-spec(wait_for_socket_on_client(Message::any(), Socket::rtmp_socket()) -> no_return()).

wait_for_socket_on_client({socket, Socket}, #rtmp_socket{} = State) ->
  inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
  {ok, {IP, Port}} = inet:peername(Socket),
  State1 = State#rtmp_socket{socket = Socket, address = IP, port = Port},
  send_data(State1, [?HS_UNCRYPTED, rtmp_handshake:c1()]),
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
  {stop, normal, State}.


%% @private
loop(#rtmp_message{} = Message, _From, State) ->
  State1 = send_data(State, Message),
  {reply, ok, loop, State1}.


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
    once ->
      handle_rtmp_data(State1, Data)
  end,
  set_options(State2, Options);

set_options(#rtmp_socket{} = State, [{debug, Debug} | Options]) ->
  io:format("Set debug to ~p~n", [Debug]),
  set_options(State#rtmp_socket{debug = Debug}, Options);

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
  {reply, get_options(State, Options), StateName, State};

handle_sync_event({getstat, Options}, _From, StateName, State) ->
  {reply, get_stat(State, Options), StateName, State};

handle_sync_event(getstat, _From, StateName, State) ->
  {reply, get_stat(State), StateName, State};

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
  
handle_info({tcp, Socket, Data}, handshake_c1, #rtmp_socket{socket=Socket, buffer = Buffer, bytes_read = BytesRead, codec = Codec} = State) ->
  activate_socket(Socket),
  <<Handshake:(?HS_BODY_LEN+1)/binary, Rest/binary>> = <<Buffer/binary, Data/binary>>,
  State1 = case rtmp_handshake:server(Handshake) of
    {uncrypted, Reply} -> 
  	  send_data(State, Reply),
      State;
    {crypted, Reply, KeyIn, KeyOut} ->
  	  send_data(State, Reply),
  	  ?D({"Established RTMPE connection:", [KeyIn, KeyOut]}),
  	  case Codec of
  	    undefined -> ok;
  	    _ -> 
  	      erlang:port_control(Codec, ?SET_KEY_IN, KeyIn),
  	      erlang:port_control(Codec, ?SET_KEY_OUT, KeyOut)
  	  end,
      State#rtmp_socket{key_in = KeyIn, key_out = KeyOut}
  end,
	{next_state, 'handshake_c3', State1#rtmp_socket{buffer = Rest, bytes_read = BytesRead + size(Data)}, ?RTMP_TIMEOUT};


handle_info({tcp, Socket, Data}, handshake_c3, #rtmp_socket{socket=Socket, buffer = Buffer, bytes_read = BytesRead} = State) when size(Buffer) + size(Data) < ?HS_BODY_LEN ->
  activate_socket(Socket),
  {next_state, handshake_c3, State#rtmp_socket{buffer = <<Buffer/binary, Data/binary>>, bytes_read = BytesRead + size(Data)}, ?RTMP_TIMEOUT};
  
handle_info({tcp, Socket, Data}, handshake_c3, #rtmp_socket{socket=Socket, consumer = Consumer, buffer = Buffer, bytes_read = BytesRead, active = Active} = State) ->
  <<_HandShakeC3:?HS_BODY_LEN/binary, Rest/binary>> = <<Buffer/binary, Data/binary>>,
  Consumer ! {rtmp, self(), connected},
  case Active of
    true -> inet:setopts(Socket, [{active, true}]);
    _ -> ok
  end,
  {next_state, loop, handle_rtmp_data(State#rtmp_socket{bytes_read = BytesRead + size(Data)}, Rest), ?RTMP_TIMEOUT};

handle_info({tcp, Socket, Data}, handshake_s1, #rtmp_socket{socket=Socket, buffer = Buffer, bytes_read = BytesRead} = State) when size(Buffer) + size(Data) < ?HS_BODY_LEN*2 + 1 ->
  activate_socket(Socket),
  {next_state, handshake_s1, State#rtmp_socket{buffer = <<Buffer/binary, Data/binary>>, bytes_read = BytesRead + size(Data)}, ?RTMP_TIMEOUT};

handle_info({tcp, Socket, Data}, handshake_s1, #rtmp_socket{socket=Socket, consumer = Consumer, buffer = Buffer, bytes_read = BytesRead, active = Active} = State) ->
  <<?HS_UNCRYPTED, _S1:?HS_BODY_LEN/binary, S2:?HS_BODY_LEN/binary, Rest/binary>> = <<Buffer/binary, Data/binary>>,
  send_data(State, rtmp_handshake:c2(S2)),
  Consumer ! {rtmp, self(), connected},
  case Active of
    true -> inet:setopts(Socket, [{active, true}]);
    _ -> ok
  end,
  {next_state, loop, handle_rtmp_data(State#rtmp_socket{bytes_read = BytesRead + size(Data)}, Rest), ?RTMP_TIMEOUT};



handle_info({tcp, Socket, Data}, loop, #rtmp_socket{socket=Socket, buffer = Buffer, bytes_read = BytesRead, bytes_unack = BytesUnack} = State) ->
  State1 = flush_send(State),
  {next_state, loop, handle_rtmp_data(State1#rtmp_socket{bytes_read = BytesRead + size(Data), bytes_unack = BytesUnack + size(Data)}, <<Buffer/binary, Data/binary>>), ?RTMP_TIMEOUT};

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

handle_info(_Info, StateName, StateData) ->
  error_logger:error_msg("Unknown message to rtmp socket: ~p ~p~n", [_Info, StateData]),
  {next_state, StateName, StateData, ?RTMP_TIMEOUT}.

flush_send(State) -> flush_send([], State).

flush_send(Packet, State) ->
  receive
    #rtmp_message{} = Message ->
      {NewState, Data} = rtmp:encode(State, Message),
      flush_send([Data | Packet], NewState)
  after
    0 -> 
      send_data(State, lists:reverse(Packet))
  end.
  
activate_socket(Socket) when is_port(Socket) ->
  inet:setopts(Socket, [{active, once}]);
activate_socket(Socket) when is_pid(Socket) ->
  ok.

% FIXME: make here proper handling of flushing message queue
send_data(#rtmp_socket{socket = Socket, key_out = KeyOut, codec = Codec} = State, Message) ->
  {NewState, Data} = case Message of
    #rtmp_message{} -> rtmp:encode(State, Message);
    _ -> {State, Message}
  end,
  Crypt = case {Codec, KeyOut} of
    {undefined,undefined} -> Data;
    {undefined,_} -> crypto:rc4_encrypt(KeyOut,Data);
    _ -> Data
  end,
  if
    is_port(Socket) ->
      gen_tcp:send(Socket, Crypt);
    is_pid(Socket) ->
      rtmpt:write(Socket, Crypt)
  end,
  NewState.    



handle_rtmp_data(#rtmp_socket{bytes_unack = Bytes, window_size = Window} = State, Data) when Bytes >= Window ->
  State1 = send_data(State, #rtmp_message{type = ack_read}),
  handle_rtmp_data(State1#rtmp_socket{}, Data);

handle_rtmp_data(#rtmp_socket{key_in = undefined} = State, Data) ->
  got_rtmp_message(rtmp:decode(State, Data));

handle_rtmp_data(#rtmp_socket{key_in = KeyIn} = State, CryptedData) ->
  Data = crypto:rc4_encrypt(KeyIn, CryptedData),
  got_rtmp_message(rtmp:decode(State, Data)).



got_rtmp_message({#rtmp_socket{debug = true} = State, #rtmp_message{channel_id = Channel, ts_type = TSType, timestamp = TS, type = Type, stream_id = StreamId, body = Body}, Rest} = Message) ->
  DecodedBody = case Type of
    video when size(Body) > 10 -> flv:decode_video_tag(Body);
    audio when size(Body) > 0 -> flv:decode_audio_tag(Body);
    _ -> Body
  end,
  io:format("~p ~p ~p ~p ~p ~p~n", [Channel, TSType, TS, Type, StreamId, DecodedBody]),
  handle_rtmp_message({State, Message, Rest});

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

rtmp_message_sent(#rtmp_socket{active = true, buffer = Data} = State) ->
  handle_rtmp_data(State, Data);

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
