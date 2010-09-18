%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        RTMP session
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtmp_session).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/video_frame.hrl").
-include("../ems.hrl").
-include("../../include/rtmp_session.hrl").

-behaviour(gen_fsm).

-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([send/2]).

%% FSM States
-export([
  'WAIT_FOR_SOCKET'/2,
  'WAIT_FOR_HANDSHAKE'/2,
  'WAIT_FOR_DATA'/2,
  'WAIT_FOR_DATA'/3]).


-export([create_client/1]).
-export([accept_connection/1, reject_connection/1]).
-export([message/4, collect_stats/1]).

-export([reply/2, fail/2]).

%%-------------------------------------------------------------------------
%% @spec create_client(Socket)  -> {ok, Pid}
%% @doc Very important function. rtmp_listener calls it to
%% create new process, that will accept socket.
%% @end
%%-------------------------------------------------------------------------
create_client(Socket) ->
  {ok, Pid} = ems_sup:start_rtmp_session(Socket),
  {ok, Pid}.


%%-------------------------------------------------------------------------
%% @spec collect_stats(Host)  -> Stats::proplist
%% @doc Asynchronously asks all rtmp clients to tell about their info
%% @end
%%-------------------------------------------------------------------------
collect_stats(_Host) ->
  Pids = [Pid || {_, Pid, _, _} <- supervisor:which_children(rtmp_session_sup), Pid =/= self()],
  ems:multicall(Pids, info, 1000).
  



accept_connection(#rtmp_session{host = Host, socket = Socket, amf_ver = AMFVersion, user_id = UserId, session_id = SessionId} = Session) ->
  Message = #rtmp_message{channel_id = 2, timestamp = 0, body = <<>>},
  % gen_fsm:send_event(self(), {invoke, AMF#rtmp_funcall{command = 'onBWDone', type = invoke, id = 2, stream_id = 0, args = [null]}}),
  rtmp_socket:send(Socket, Message#rtmp_message{type = window_size, body = ?RTMP_WINDOW_SIZE}),
  rtmp_socket:send(Socket, Message#rtmp_message{type = bw_peer, body = ?RTMP_WINDOW_SIZE}),
  rtmp_socket:send(Socket, Message#rtmp_message{type = stream_begin, stream_id = 0}),
  % rtmp_socket:send(Socket, Message#rtmp_message{type = stream_begin}),
  % rtmp_socket:setopts(Socket, [{chunk_size, ?RTMP_PREF_CHUNK_SIZE}]),
  rtmp_socket:setopts(Socket, [{chunk_size, 128}]),
  
  ConnectObj = [{fmsVer, <<"FMS/3,5,2,654">>}, {capabilities, 31}, {mode, 1}],
  StatusObj = [{level, <<"status">>}, 
               {code, <<"NetConnection.Connect.Success">>},
               {description, <<"Connection succeeded.">>},
               {data,[<<"version">>, <<"3,5,2,654">>]},
               {objectEncoding, AMFVersion}],
  reply(Socket, #rtmp_funcall{id = 1, args = [{object, ConnectObj}, {object, StatusObj}]}),
  rtmp_socket:setopts(Socket, [{amf_version, AMFVersion}]),
  ems_event:user_connected(Host, self(), [{user_id,UserId}, {session_id,SessionId}]),
  Session;
  
accept_connection(Session) when is_pid(Session) ->
  gen_fsm:send_event(Session, accept_connection).


reject_connection(#rtmp_session{socket = Socket} = Session) ->
  ConnectObj = [{fmsVer, <<"FMS/3,5,2,654">>}, {capabilities, 31}, {mode, 1}],
  StatusObj = [{level, <<"status">>}, 
               {code, <<?NC_CONNECT_REJECTED>>},
               {description, <<"Connection rejected.">>}],
  fail(Socket, #rtmp_funcall{id = 1, args = [{object, ConnectObj}, {object, StatusObj}]}),
  gen_fsm:send_event(self(), exit),
  Session;
  
reject_connection(Session) when is_pid(Session) ->
  gen_fsm:send_event(Session, reject_connection).

  
  
reply(#rtmp_session{socket = Socket}, AMF) ->
  rtmp_socket:invoke(Socket, AMF#rtmp_funcall{command = '_result', type = invoke});
reply(Socket, AMF) when is_pid(Socket) ->
  rtmp_socket:invoke(Socket, AMF#rtmp_funcall{command = '_result', type = invoke}).


fail(#rtmp_session{socket = Socket}, AMF) ->
  rtmp_socket:invoke(Socket, AMF#rtmp_funcall{command = '_error', type = invoke});
fail(Socket, AMF) when is_pid(Socket) ->
  rtmp_socket:invoke(Socket, AMF#rtmp_funcall{command = '_error', type = invoke}).


message(Pid, Stream, Code, Body) ->
  gen_fsm:send_event(Pid, {message, Stream, Code, Body}).

  
%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

start_link() ->
  gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid) ->
  gen_fsm:send_event(Pid, {socket_ready, Socket}).

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
init([]) ->
  random:seed(now()),
  {ok, 'WAIT_FOR_SOCKET', #rtmp_session{}}.


send(Session, Message) ->
  % case process_info(Session, message_queue_len) of
  %   {message_queue_len, Length} when Length > 100 ->
  %     % ?D({"Session is too slow in consuming messages", Session, Length}),
  %     ok;
  %   _ -> ok
  % end,
  Session ! Message.
  


%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, RTMP}, State) when is_pid(RTMP) ->
  {address, {IP, Port}} = rtmp_socket:getopts(RTMP, address),
  Addr = case IP of
    undefined -> "0.0.0.0";
    _ -> lists:flatten(io_lib:format("~p.~p.~p.~p", erlang:tuple_to_list(IP)))
  end,
  erlang:monitor(process, RTMP),
  {next_state, 'WAIT_FOR_HANDSHAKE', State#rtmp_session{socket = RTMP, addr = Addr, port = Port}};


    
'WAIT_FOR_SOCKET'(Other, State) ->
  error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
  {next_state, 'WAIT_FOR_SOCKET', State}.

'WAIT_FOR_HANDSHAKE'(timeout, #rtmp_session{host = Host, user_id = UserId, addr = IP} = State) ->
  ems_log:error(Host, "TIMEOUT ~p ~p", [UserId, IP]),
  {stop, normal, State}.

%% Notification event coming from client

'WAIT_FOR_DATA'(exit, State) ->
  {stop, normal, State};

'WAIT_FOR_DATA'(#rtmp_message{} = Message, State) ->
  rtmp_socket:send(State#rtmp_session.socket, Message),
  {next_state, 'WAIT_FOR_DATA', State};

'WAIT_FOR_DATA'(accept_connection, Session) ->
  {next_state, 'WAIT_FOR_DATA', accept_connection(Session)};

'WAIT_FOR_DATA'(reject_connection, Session) ->
  reject_connection(Session),
  {stop, normal, Session};

'WAIT_FOR_DATA'({message, Stream, Code, Body}, #rtmp_session{socket = Socket} = State) ->
  rtmp_socket:status(Socket, Stream, Code, Body),
  {next_state, 'WAIT_FOR_DATA', State};

'WAIT_FOR_DATA'(Message, #rtmp_session{host = Host} = State) ->
  case ems:try_method_chain(Host, 'WAIT_FOR_DATA', [Message, State]) of
    {unhandled} -> {next_state, 'WAIT_FOR_DATA', State};
    unhandled -> {next_state, 'WAIT_FOR_DATA', State};
    Reply -> Reply
  end.

%% Sync event

'WAIT_FOR_DATA'(info, _From, #rtmp_session{} = State) ->
  {reply, session_stats(State), 'WAIT_FOR_DATA', State};

  
'WAIT_FOR_DATA'(Data, _From, State) ->
	io:format("~p Ignoring data: ~p\n", [self(), Data]),
  {next_state, 'WAIT_FOR_DATA', State}.
    
    
% send(#rtmp_session{server_chunk_size = ChunkSize} = State, {#channel{} = Channel, Data}) ->
%   Packet = rtmp:encode(Channel#channel{chunk_size = ChunkSize}, Data),
%   % ?D({"Channel", Channel#channel.type, Channel#channel.timestamp, Channel#channel.length}),
%   send_data(State, Packet).
	

handle_rtmp_message(State, #rtmp_message{type = invoke, body = AMF}) ->
  #rtmp_funcall{command = CommandBin} = AMF,
  Command = binary_to_atom(CommandBin, utf8),
  call_function(State, AMF#rtmp_funcall{command = Command});
  
handle_rtmp_message(#rtmp_session{streams = Streams} = State, 
   #rtmp_message{type = Type, stream_id = StreamId, body = Body, timestamp = Timestamp}) when (Type == video) or (Type == audio) or (Type == metadata) or (Type == metadata3) ->
  Recorder = ems:element(StreamId, Streams),
  
  Frame = flv_video_frame:decode(#video_frame{dts = Timestamp, pts = Timestamp, content = Type}, Body),
  ems_media:publish(Recorder, Frame),
  State;

handle_rtmp_message(State, #rtmp_message{type = shared_object, body = SOEvent}) ->
  #so_message{name = Name, persistent = Persistent} = SOEvent,
  ?D({"Shared object", Name}),
  {NewState, Object} = find_shared_object(State, Name, Persistent),
  shared_object:message(Object, SOEvent),
  NewState;

handle_rtmp_message(#rtmp_session{streams = Streams} = State, #rtmp_message{stream_id = StreamId, type = buffer_size, body = BufferSize}) ->
  case ems:element(StreamId, Streams) of
    Player when is_pid(Player) -> ems_media:setopts(Player, [{client_buffer, BufferSize}]);
    _ -> ok
  end,
  State;

handle_rtmp_message(State, #rtmp_message{type = pong}) -> State;
handle_rtmp_message(State, #rtmp_message{type = ping}) -> State;
handle_rtmp_message(State, #rtmp_message{type = ack_read}) -> State;
handle_rtmp_message(State, #rtmp_message{type = window_size}) -> State;
handle_rtmp_message(State, #rtmp_message{type = chunk_size}) -> State;
handle_rtmp_message(State, #rtmp_message{type = broken_meta}) -> State;

handle_rtmp_message(State, Message) ->
  ?D({"RTMP", Message#rtmp_message.type}),
  State.


find_shared_object(#rtmp_session{host = Host, cached_shared_objects = Objects} = State, Name, Persistent) ->
  case lists:keysearch(Name, 1, Objects) of
    false ->
      Object = shared_objects:open(Host, Name, Persistent),
      NewObjects = lists:keystore(Name, 1, Objects, {Name, Object}),
      {State#rtmp_session{cached_shared_objects = NewObjects}, Object};
    {value, {Name, Object}} ->
      {State, Object}
  end.

call_function(#rtmp_session{} = State, #rtmp_funcall{command = connect, args = [{object, PlayerInfo} | _]} = AMF) ->
  URL = proplists:get_value(tcUrl, PlayerInfo),
  {ok, UrlRe} = re:compile("(.*)://([^/]+)/?(.*)$"),
  {match, [_, _Proto, HostName, Path]} = re:run(URL, UrlRe, [{capture, all, binary}]),
  Host = ems:host(HostName),
  
  ?D({"Client connecting", HostName, Host, AMF#rtmp_funcall.args}),

  AMFVersion = case lists:keyfind(objectEncoding, 1, PlayerInfo) of
    {objectEncoding, 0.0} -> 0;
    {objectEncoding, 3.0} -> 3;
    {objectEncoding, _N} -> 
      error_logger:error_msg("Warning! Cannot work with clients, using not AMF0/AMF3 encoding.
      Assume _connection.objectEncoding = ObjectEncoding.AMF0; in your flash code is used version ~p~n", [_N]),
      throw(invalid_amf3_encoding);
    _ -> 0
  end,
  
  SessionId = timer:now_diff(erlang:now(),{0,0,0}),

	NewState1 =	State#rtmp_session{player_info = PlayerInfo, host = Host, path = Path, amf_ver = AMFVersion, session_id = SessionId},
	
	call_function(Host, NewState1, AMF);


call_function(#rtmp_session{host = Host} = State, AMF) ->
  call_function(Host, State, AMF).
  
  
call_function([], State, AMF) ->
  ?D({"Failed funcall", AMF#rtmp_funcall.command}),
  rtmp_session:fail(State, AMF),
  State;
  
call_function([Module|Modules], State, #rtmp_funcall{command = Command} = AMF) ->
  case code:is_loaded(mod_name(Module)) of
    false -> code:load_file(mod_name(Module));
    _ -> ok
  end,
  % ?D({"Checking", Module, Command, ems:respond_to(Module, Command, 2)}),
  case ems:respond_to(Module, Command, 2) of
    true ->
      case Module:Command(State, AMF) of
        unhandled ->
          call_function(Modules, State, AMF);
        NewState ->
          NewState
      end;
    false ->
      call_function(Modules, State, AMF)
  end;
  
call_function(Host, State, AMF) ->
  call_function(ems:get_var(modules, Host, [trusted_login]), State, AMF).


mod_name(Mod) when is_tuple(Mod) -> element(1, Mod);
mod_name(Mod) -> Mod.


	
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
handle_info({rtmp, _Socket, disconnect, Stats}, _StateName, #rtmp_session{} = StateData) ->
  BytesSent = proplists:get_value(send_oct, Stats, 0),
  BytesRecv = proplists:get_value(recv_oct, Stats, 0),
  {stop, normal, StateData#rtmp_session{bytes_sent = BytesSent, bytes_recv = BytesRecv}};

handle_info({rtmp, Socket, #rtmp_message{} = Message}, StateName, State) ->
  State1 = handle_rtmp_message(State, Message),
  State2 = flush_reply(State1),
  % [{message_queue_len, Messages}, {memory, Memory}] = process_info(self(), [message_queue_len, memory]),
  % io:format("messages=~p,memory=~p~n", [Messages, Memory]),
  rtmp_socket:setopts(Socket, [{active, once}]),
  {next_state, StateName, State2};
  
handle_info({rtmp, Socket, connected}, 'WAIT_FOR_HANDSHAKE', State) ->
  rtmp_socket:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_DATA', State};

handle_info({rtmp, _Socket, timeout}, _StateName, #rtmp_session{host = Host, user_id = UserId, addr = IP} = State) ->
  ems_log:error(Host, "TIMEOUT ~p ~p ~p", [_Socket, UserId, IP]),
  {stop, normal, State};

handle_info({'DOWN', _Ref, process, Socket, _Reason}, _StateName, #rtmp_session{socket = Socket} = State) ->
  {stop,normal,State};
  
handle_info({'DOWN', _Ref, process, PlayerPid, _Reason}, StateName, #rtmp_session{socket = Socket, streams = Streams} = State) ->
  case ems:tuple_find(PlayerPid, Streams) of
    false -> 
      ?D({"Unknown linked pid failed", PlayerPid, _Reason}),
      {next_state, StateName, State};
    {StreamId, PlayerPid} ->
      rtmp_lib:play_complete(Socket, StreamId, [{duration, 0}]),
      NewStreams = setelement(StreamId, Streams, undefined),
      {next_state, StateName, State#rtmp_session{streams = NewStreams}}
  end;

handle_info({Port, {data, _Line}}, StateName, State) when is_port(Port) ->
  % No-op. Just child program
  {next_state, StateName, State};

handle_info({ems_stream, StreamId, play_complete, LastDTS}, StateName, #rtmp_session{socket = Socket} = State) ->
  rtmp_lib:play_complete(Socket, StreamId, [{duration, LastDTS}]),
  {next_state, StateName, State};

handle_info({ems_stream, StreamId, play_failed}, StateName, #rtmp_session{socket = Socket} = State) ->
  rtmp_lib:play_failed(Socket, StreamId),
  {next_state, StateName, State};
  
handle_info(#video_frame{} = Frame, 'WAIT_FOR_DATA', #rtmp_session{} = State) ->
  {next_state, 'WAIT_FOR_DATA', handle_frame(Frame, State)};

handle_info(#rtmp_message{} = Message, StateName, State) ->
  rtmp_socket:send(State#rtmp_session.socket, Message),
  {next_state, StateName, State};

handle_info(exit, _StateName, StateData) ->
  {stop, normal, StateData};
  
handle_info({'$gen_call', From, Request}, StateName, StateData) ->
  case ?MODULE:StateName(Request, From, StateData) of
    {reply, Reply, NewStateName, NewState} ->
      gen:reply(From, Reply),
      {next_state, NewStateName, NewState};
    {next_state, NewStateName, NewState} ->
      {next_state, NewStateName, NewState}
  end;


handle_info(Message, 'WAIT_FOR_DATA', #rtmp_session{host = Host} = State) ->
  case ems:try_method_chain(Host, handle_info, [Message, State]) of
    {unhandled} -> {next_state, 'WAIT_FOR_DATA', State};
    unhandled -> {next_state, 'WAIT_FOR_DATA', State};
    Reply -> Reply
  end;

handle_info(_Info, StateName, StateData) ->
  ?D({"Some info handled", _Info, StateName, StateData}),
  {next_state, StateName, StateData}.


handle_frame(#video_frame{content = Type, stream_id = StreamId, dts = DTS, pts = PTS} = Frame, 
             #rtmp_session{socket = Socket, streams_dts = StreamsDTS, streams_started = Started} = State) ->
  {State1, BaseDts, Starting} = case ems:element(StreamId, Started) of
    undefined ->
      rtmp_lib:play_start(Socket, StreamId, 0),
      {State#rtmp_session{
        streams_started = ems:setelement(StreamId, Started, true),
        streams_dts = ems:setelement(StreamId, StreamsDTS, DTS)}, DTS, true};
    _ ->
      {State, ems:element(StreamId, StreamsDTS), false}
  end,
    
  % ?D({Frame#video_frame.codec,Frame#video_frame.flavor,round(DTS)}),
  Message = #rtmp_message{
    channel_id = rtmp_lib:channel_id(Type, StreamId), 
    timestamp = DTS - BaseDts,
    type = Type,
    stream_id = StreamId,
    body = flv_video_frame:encode(Frame#video_frame{dts = DTS - BaseDts, pts = PTS - BaseDts})},
	rtmp_socket:send(Socket, Message),
	case {Starting, Frame} of
	  {true, #video_frame{content = video, flavor = config}} -> rtmp_socket:send(Socket, Message);
	  _ -> ok
	end,
  State1.

flush_reply(#rtmp_session{socket = Socket} = State) ->
  receive
    #rtmp_message{} = Message ->
      rtmp_socket:send(Socket, Message),
      flush_reply(State)
    after
      0 -> State
  end.


% collect_statistics(#rtmp_session{socket = Socket}) ->
%   Stats = rtmp_socket:getstat(Socket),
%   Stats.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #rtmp_session{host = Host, addr = Addr, user_id = UserId, session_id = SessionId, bytes_recv = Recv, bytes_sent = Sent} = State) ->
  ems_log:access(Host, "DISCONNECT ~s ~s ~p ~p ~p ~p", [Addr, Host, UserId, SessionId, Recv, Sent]),
  ems_event:user_disconnected(State#rtmp_session.host, self(), session_stats(State)),
  (catch erlyvideo:call_modules(logout, [State])),
  ok.



session_stats(#rtmp_session{host = Host, addr = Addr, bytes_recv = Recv, bytes_sent = Sent, play_stats = PlayStats, user_id = UserId, session_id = SessionId}) ->
  [{host,Host},{recv_oct,Recv},{sent_oct,Sent},{addr,Addr},{user_id,UserId},{session_id,SessionId}|PlayStats].


%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(OldVersion, StateName, #rtmp_session{host = Host} = State, Extra) ->
  plugins_code_change(OldVersion, StateName, State, Extra, ems:get_var(applications, Host, [])).

plugins_code_change(_OldVersion, StateName, State, _Extra, []) -> {ok, StateName, State};

plugins_code_change(OldVersion, StateName, State, Extra, [Module | Modules]) -> 
  case ems:respond_to(Module, code_change, 4) of
    true ->
      error_logger:info_msg("Code change in module ~p~n", [Module]),
      {ok, NewStateName, NewState} = Module:code_change(OldVersion, StateName, State, Extra);
    _ ->
      {NewStateName, NewState} = {StateName, State}
  end,
  plugins_code_change(OldVersion, NewStateName, NewState, Extra, Modules).




