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
-export([status/3, status/4, invoke/2, invoke/4, notify/4]).

-export([start_socket/2, start_server/3, set_socket/2]).
  


%% gen_fsm callbacks
-export([init/1, handle_info/2]).

-export([wait_for_socket_on_server/1, wait_for_socket_on_client/1, handshake_c1/1, handshake_c3/1, handshake_s1/1, loop/1]).

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
  RTMP ! {socket, Socket},
  {ok, RTMP}.

set_socket(RTMP, Socket) ->
  RTMP ! {socket, Socket}.

  
%% @private
start_link(Type) ->
  Pid = spawn_link(?MODULE, init, [Type]),
  {ok, Pid}.
  
  
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
  RTMP ! {setopts, Options}.

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
  gen_server:call(RTMP, {getopts, Options}, ?RTMP_TIMEOUT).

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
  gen_server:call(RTMP, {getstat, Options}, ?RTMP_TIMEOUT).

%% @spec (RTMP::pid()) -> Values::[{Key,Value}]
%% @doc Just the same as {@link inet:getstats/1. inet:getstats/1} this function gets statistics of 
%% rtmp socket.<br/>
-spec(getstat(RTMP::rtmp_socket_pid()) -> ok).
getstat(RTMP) ->
  gen_server:call(RTMP, getstat, ?RTMP_TIMEOUT).

  
%% @spec (RTMP::pid(), Message::rtmp_message()) -> ok
%% @doc Sends message to client.
%% @end
-spec(send(RTMP::rtmp_socket_pid(), Message::rtmp_message()) -> ok).
send(RTMP, Message) ->
  % case process_info(RTMP, message_queue_len) of
  %   {message_queue_len, Length} when Length < 20 -> RTMP ! Message;
  %   {message_queue_len, Length} -> gen_fsm:sync_send_event(RTMP, Message, ?RTMP_TIMEOUT);
  %   _ -> ok
  % end,
  RTMP ! Message,
  ok.
  

channel_id(metadata, StreamId) -> 3 + StreamId;
channel_id(video, StreamId) -> 4 + StreamId;
channel_id(audio, StreamId) -> 5 + StreamId.



notify(RTMP, StreamId, Name, Args) ->
  Arg = {object, lists:ukeymerge(1, [{level, <<"status">>}], lists:keysort(1, Args))},
  send(RTMP, #rtmp_message{type = metadata, channel_id = channel_id(video, StreamId), stream_id = StreamId, body = [Name, Arg], timestamp = same}).


-spec(status(RTMP::rtmp_socket_pid(), StreamId::integer(), Code::any_string()) -> ok).
status(RTMP, StreamId, Code) when is_list(Code) ->
  status(RTMP, StreamId, list_to_binary(Code), <<"-">>);

status(RTMP, StreamId, Code) when is_binary(Code) ->
  status(RTMP, StreamId, Code, <<"-">>).


-spec(status(RTMP::rtmp_socket_pid(), StreamId::integer(), Code::any_string(), Description::any_string()) -> ok).
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
init(accept) ->
  ?MODULE:wait_for_socket_on_server(#rtmp_socket{codec = init_codec(), channels = {}, out_channels = {}, active = false});
  
init(connect) ->
  ?MODULE:wait_for_socket_on_client(#rtmp_socket{codec = init_codec(), channels = {}, out_channels = {}, active = false}). 
%% @private 

wait_for_socket_on_server(#rtmp_socket{consumer = C, socket = S} = State) when C == undefined orelse S == undefined ->
  receive
    {socket, Socket} when is_port(Socket) ->
      inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
      {ok, {IP, Port}} = inet:peername(Socket),
      ?MODULE:wait_for_socket_on_server(State#rtmp_socket{socket = Socket, address = IP, port = Port});
    {socket, Socket} when is_pid(Socket) ->
      link(Socket),
      ?MODULE:wait_for_socket_on_server(State#rtmp_socket{socket = Socket});
    {setopts, Options} ->
      ?MODULE:wait_for_socket_on_server(set_options(State,Options))
  after
    ?RTMP_TIMEOUT -> ok
  end;
  
wait_for_socket_on_server(State) ->
  ?MODULE:handshake_c1(State).
      

%% @private  
-spec(wait_for_socket_on_client(Socket::rtmp_socket()) -> no_return()|ok).

wait_for_socket_on_client(#rtmp_socket{consumer = C, socket = S} = State) when C == undefined orelse S == undefined ->
  receive
    {socket, Socket} when is_port(Socket) ->
      inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
      {ok, {IP, Port}} = inet:peername(Socket),
      State1 = State#rtmp_socket{socket = Socket, address = IP, port = Port},
      send_data(State1, [?HS_HEADER, rtmp_handshake:c1()]),
      ?MODULE:wait_for_socket_on_client(State1);
    {setopts, Options} ->
      ?MODULE:wait_for_socket_on_client(set_options(State,Options))
  after
    ?RTMP_TIMEOUT -> ok
  end;
  
wait_for_socket_on_client(State) ->
  ?MODULE:handshake_s1(State).

%% @private  
handshake_c1(#rtmp_socket{socket=Socket, buffer = Buffer, bytes_read = BytesRead} = State) ->
  receive
    {tcp, Socket, Data} when size(Buffer) + size(Data) < ?HS_BODY_LEN + 1 ->
      activate_socket(Socket),
      handshake_c1(State#rtmp_socket{buffer = <<Buffer/binary, Data/binary>>, bytes_read = BytesRead + size(Data)});
    {tcp, Socket, Data} ->
      activate_socket(Socket),
      <<?HS_HEADER, Handshake:?HS_BODY_LEN/binary, Rest/binary>> = <<Buffer/binary, Data/binary>>,
  	  send_data(State, [?HS_HEADER, rtmp_handshake:s1(), rtmp_handshake:s2(Handshake)]),
  	  ?MODULE:handshake_c3(State#rtmp_socket{buffer = Rest, bytes_read = BytesRead + size(Data)})
  after
    ?RTMP_TIMEOUT -> ok
  end.

%% @private  
handshake_c3(#rtmp_socket{socket=Socket, consumer = Consumer, buffer = Buffer, active = Active, bytes_read = BytesRead} = State) ->
  receive
    {tcp, Socket, Data} when size(Buffer) + size(Data) < ?HS_BODY_LEN ->
      activate_socket(Socket),
      ?MODULE:handshake_c3(State#rtmp_socket{buffer = <<Buffer/binary, Data/binary>>, bytes_read = BytesRead + size(Data)});
    {tcp, Socket, Data} ->
      <<_HandShakeC3:?HS_BODY_LEN/binary, Rest/binary>> = <<Buffer/binary, Data/binary>>,
      Consumer ! {rtmp, self(), connected},
      case Active of
        true -> inet:setopts(Socket, [{active, true}]);
        _ -> ok
      end,
      ?MODULE:loop(handle_rtmp_data(State#rtmp_socket{bytes_read = BytesRead + size(Data)}, Rest))
  after
    ?RTMP_TIMEOUT -> ok
  end.

%% @private  
handshake_s1(#rtmp_socket{socket=Socket, buffer = Buffer, consumer = Consumer, bytes_read = BytesRead, active = Active} = State) ->
  receive
    {tcp, Socket, Data} when size(Buffer) + size(Data) < ?HS_BODY_LEN*2 + 1 ->
      activate_socket(Socket),
      handshake_s1(State#rtmp_socket{buffer = <<Buffer/binary, Data/binary>>, bytes_read = BytesRead + size(Data)});

    {tcp, Socket, Data} ->
      <<?HS_HEADER, _S1:?HS_BODY_LEN/binary, S2:?HS_BODY_LEN/binary, Rest/binary>> = <<Buffer/binary, Data/binary>>,
      send_data(State, rtmp_handshake:c2(S2)),
      Consumer ! {rtmp, self(), connected},
      case Active of
        true -> inet:setopts(Socket, [{active, true}]);
        _ -> ok
      end,
      loop(handle_rtmp_data(State#rtmp_socket{bytes_read = BytesRead + size(Data)}, Rest))
  after
    ?RTMP_TIMEOUT -> ok
  end.
    

%% @private

loop(#rtmp_socket{consumer = Consumer, pinged = Pinged} = State) ->
  receive
    Message ->
      case handle_info(Message, State) of
        stop -> ok;
        {ok, NewState} -> ?MODULE:loop(NewState)
      end
  after
    ?RTMP_TIMEOUT -> 
      case Pinged of
        false ->
          send_data(State, #rtmp_message{type = ping}),
          ?MODULE:loop(State#rtmp_socket{pinged = true});
        true ->
          Consumer ! {rtmp, self(), timeout, get_stat(State)},
          ok
      end
  end.


handle_info({tcp, Socket, Data}, #rtmp_socket{socket=Socket, buffer = Buffer, bytes_read = BytesRead} = State) ->
  State1 = flush_send(State),
  {ok, handle_rtmp_data(State1#rtmp_socket{bytes_read = BytesRead + size(Data)}, <<Buffer/binary, Data/binary>>)};

handle_info({tcp_closed, Socket}, #rtmp_socket{socket = Socket, consumer = Consumer} = StateData) ->
  Consumer ! {rtmp, self(), disconnect, get_stat(StateData)},
  stop;

handle_info(#rtmp_message{} = Message, State) ->
  State1 = send_data(State, Message),
  State2 = flush_send(State1),
  {ok, State2};

handle_info({rtmpt, RTMPT, alive}, #rtmp_socket{socket = RTMPT} = State) ->
  {ok, State#rtmp_socket{pinged = false}};

handle_info({rtmpt, RTMPT, Data}, State) ->
  handle_info({tcp, RTMPT, Data}, State);
    
handle_info({'$gen_call', Client, Call}, State) ->
  {ok, Reply, NewState} = handle_call(Call, State),
  gen_server:reply(Client, Reply),
  {ok, NewState};
    
handle_info({setopts, Options}, State) ->
  {ok, set_options(State,Options)};
    
handle_info(Message, State) ->
  io:format("Unknown message to RTMP socket: ~p~n", [Message]),
  {ok, State}.


handle_call({getopts,Options}, State) ->
  {ok, get_options(State, Options), State};

handle_call({getstat,Options}, State) ->
  {ok, get_stat(State, Options), State}.

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

set_options(#rtmp_socket{consumer = PrevConsumer} = State, [{consumer, Consumer} | Options]) ->
  (catch unlink(PrevConsumer)),  
  (catch link(Consumer)),  
  set_options(State#rtmp_socket{consumer = Consumer}, Options);

set_options(State, [{chunk_size, ChunkSize} | Options]) ->
  send_data(State, #rtmp_message{type = chunk_size, body = ChunkSize}),
  set_options(State#rtmp_socket{server_chunk_size = ChunkSize}, Options);

set_options(State, []) -> State.
  



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
send_data(#rtmp_socket{socket = Socket} = State, Message) ->
  {NewState, Data} = case Message of
    #rtmp_message{} -> rtmp:encode(State, Message);
    _ -> {State, Message}
  end,
  if
    is_port(Socket) ->
      erlang:port_command(Socket, Data, []),
      receive
    	  {inet_reply,Socket,Status} -> Status
    	end;
    is_pid(Socket) ->
      rtmpt:write(Socket, Data)
  end,
  NewState.    


handle_rtmp_data(State, Data) ->
  handle_rtmp_message(rtmp:decode(State, Data)).

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


