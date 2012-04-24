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
-include("log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("../include/rtmp.hrl").
-include("rtmp_session.hrl").

-behaviour(gen_server).


-export([start_link/1, set_socket/2]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([send/2, send_frame/2, flush_stream/1, send_rtmp_frame/2]).


-export([accept_connection/1, reject_connection/1, close_connection/1]).
-export([message/4]).

-export([reply/2, fail/2, stop/1]).
-export([get_stream/2, set_stream/2, alloc_stream/1, delete_stream/2]).
-export([get_socket/1]).
-export([call_function/2]).


-export([connect/2]).
-export([createStream/2, deleteStream/2, closeStream/2, releaseStream/2]).
-export([receiveAudio/2, receiveVideo/2]).

-include("meta_access.hrl").







stop(Pid) when is_pid(Pid) ->
  gen_server:call(Pid, exit).


accept_connection(#rtmp_session{socket = Socket, amf_ver = AMFVersion, user_id = UserId, session_id = SessionId, module = M} = Session) ->
  rtmp_lib:accept_connection(Socket, [{amf_version, AMFVersion}]),
  {ok, Session1} = M:handle_control({connected, UserId, SessionId}, Session),
  Session1;

accept_connection(Session) when is_pid(Session) ->
  gen_server:call(Session, accept_connection).


reject_connection(#rtmp_session{socket = Socket} = Session) ->
  rtmp_lib:reject_connection(Socket),
  self() ! exit,
  Session;

reject_connection(Session) when is_pid(Session) ->
  gen_server:call(Session, reject_connection).


close_connection(#rtmp_session{} = Session) ->
  self() ! exit,
  Session;

close_connection(Session) when is_pid(Session) ->
  gen_server:call(Session, close_connection).



reply(#rtmp_session{socket = Socket}, AMF) -> reply(Socket, AMF);
reply(Socket, AMF) when is_pid(Socket) -> rtmp_lib:reply(Socket, AMF).


fail(#rtmp_session{socket = Socket} = State, AMF) -> fail(Socket, AMF), State;
fail(Socket, AMF) when is_pid(Socket) -> rtmp_lib:fail(Socket, AMF).


message(Pid, Stream, Code, Body) ->
  gen_server:call(Pid, {message, Stream, Code, Body}).





%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

start_link(Callback) ->
  gen_server:start_link(?MODULE, [Callback], []).

set_socket(Pid, Socket) when is_pid(Pid) ->
  gen_server:call(Pid, {socket_ready, Socket}).

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
init([Callback]) ->
  random:seed(now()),
  {ok, Session} = Callback:init(#rtmp_session{module = Callback}),
  {ok, Session}.


send(Session, Message) ->
  % case process_info(Session, message_queue_len) of
  %   {message_queue_len, Length} when Length > 100 ->
  %     % ?D({"Session is too slow in consuming messages", Session, Length}),
  %     ok;
  %   _ -> ok
  % end,
  Session ! Message.



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
handle_call({socket_ready, RTMP}, _From, State) ->
  {address, {IP, Port}} = rtmp_socket:getopts(RTMP, address),
  Addr = case IP of
    undefined -> "0.0.0.0";
    _ -> lists:flatten(io_lib:format("~p.~p.~p.~p", erlang:tuple_to_list(IP)))
  end,
  erlang:monitor(process, RTMP),
  (catch ems_network_lag_monitor:watch(RTMP)),
  % rtmp_socket:setopts(RTMP, [{debug,true}]),
  {reply, ok, State#rtmp_session{socket = RTMP, addr = Addr, port = Port}};

handle_call({get_field, Field}, _From, State) ->
  {reply, get(State, Field), State};


handle_call(accept_connection, _From, Session) ->
  {reply, ok, accept_connection(Session)};

handle_call(reject_connection, _From, Session) ->
  reject_connection(Session),
  {stop, normal, Session};

handle_call(close_connection, _From, Session) ->
  close_connection(Session),
  {stop, normal, Session};

handle_call(Call, _From, StateData) ->
  {stop, {unknown_call, Call}, StateData}.


handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({rtmp, _Socket, disconnect, Stats}, #rtmp_session{} = StateData) ->
  BytesSent = proplists:get_value(send_oct, Stats, 0),
  BytesRecv = proplists:get_value(recv_oct, Stats, 0),
  {stop, normal, StateData#rtmp_session{bytes_sent = BytesSent, bytes_recv = BytesRecv}};

handle_info({rtmp, Socket, #rtmp_message{} = Message}, State) ->
  State1 = handle_rtmp_message(State, Message),
  State2 = flush_reply(State1),
  % [{message_queue_len, Messages}, {memory, Memory}] = process_info(self(), [message_queue_len, memory]),
  % io:format("messages=~p,memory=~p~n", [Messages, Memory]),
  rtmp_socket:setopts(Socket, [{active, once}]),
  {noreply, State2};

handle_info({rtmp, Socket, connected}, State) ->
  rtmp_socket:setopts(Socket, [{active, once}]),
  {noreply, State};

handle_info({rtmp, _Socket, timeout}, #rtmp_session{module = M} = State) ->
  {ok, State1} = M:handle_control(timeout, State),
  {stop, normal, State1};

handle_info({'DOWN', _Ref, process, Socket, _Reason}, #rtmp_session{socket = Socket} = State) ->
  {stop, normal, State};

handle_info({'DOWN', _Ref, process, PlayerPid, _Reason}, #rtmp_session{socket = Socket} = State) ->
  %FIXME: add passing down this message to plugins
  case find_stream_by_pid(PlayerPid, State) of
    false ->
      ?D({"Unknown linked pid failed", PlayerPid, _Reason}),
      {noreply, State};
    #rtmp_stream{stream_id = StreamId} ->
      ?D({"Failed played stream", StreamId, PlayerPid}),
      rtmp_lib:play_failed(Socket, StreamId),
      flush_stream(StreamId),
      {noreply, delete_stream(StreamId, State)}
  end;

handle_info({Port, {data, _Line}}, State) when is_port(Port) ->
  % No-op. Just child program
  {noreply, State};

handle_info(#video_frame{} = Frame, #rtmp_session{} = State) ->
  {noreply, handle_frame(Frame, State)};

handle_info(#rtmp_message{} = Message, State) ->
  rtmp_socket:send(State#rtmp_session.socket, Message),
  {noreply, State};

handle_info(exit, StateData) ->
  {stop, normal, StateData};

handle_info({rtmp_lag, _Media}, State) ->
  ?D("Client stop due to rtmp_lag"),
  {stop, normal, State};

handle_info({message, Stream, Code, Body}, #rtmp_session{socket = Socket} = State) ->
  rtmp_socket:status(Socket, Stream, Code, Body),
  {noreply, State};

handle_info({ems_stream, StreamId, play_complete, LastDTS}, #rtmp_session{socket = Socket} = State) ->
  #rtmp_stream{base_dts = BaseDTS, options = Options} = rtmp_session:get_stream(StreamId, State),
  Start = case {BaseDTS, proplists:get_value(start, Options)} of
    {undefined, undefined} -> 0;
    {Num, _} when is_number(Num) -> Num;
    {_, Num} when is_number(Num) -> Num
  end,
  rtmp_lib:play_complete(Socket, StreamId, [{duration, rtmp:justify_ts(LastDTS - Start)}]),
  {noreply, State};

handle_info({ems_stream, StreamId, play_failed}, #rtmp_session{socket = Socket} = State) ->
  rtmp_lib:play_failed(Socket, StreamId),
  {noreply, State};

handle_info({ems_stream, StreamId, seek_success, NewDTS}, #rtmp_session{socket = Socket} = State) ->
  #rtmp_stream{base_dts = BaseDTS} = Stream = rtmp_session:get_stream(StreamId, State),

  % ?D({self(), "seek to", round(NewDTS), rtmp:justify_ts(NewDTS - BaseDTS)}),
  rtmp_lib:seek_notify(Socket, StreamId, rtmp:justify_ts(NewDTS - BaseDTS)),
  State1 = rtmp_session:set_stream(Stream#rtmp_stream{seeking = false}, State),
  {noreply, State1};

handle_info({ems_stream, StreamId, burst_start}, #rtmp_session{socket = Socket} = State) ->
  rtmp_socket:send(Socket, #rtmp_message{type = burst_start, stream_id = StreamId}),
  {noreply, State};

handle_info({ems_stream, StreamId, burst_stop}, #rtmp_session{socket = Socket} = State) ->
  rtmp_socket:send(Socket, #rtmp_message{type = burst_stop, stream_id = StreamId}),
  {noreply, State};

handle_info({ems_stream, StreamId, seek_failed}, #rtmp_session{socket = Socket} = State) ->
  ?D({"seek failed"}),
  rtmp_lib:seek_failed(Socket, StreamId),
  {noreply, State};

handle_info({ems_stream, StreamId, not_found}, #rtmp_session{socket = Socket} = State) ->
  rtmp_socket:status(Socket, StreamId, <<"NetStream.Play.StreamNotFound">>),
  % ems_log:access(Host, "NOT_FOUND ~s ~p ~p ~s ~p", [State#rtmp_session.addr, State#rtmp_session.user_id, State#rtmp_session.session_id, '??', StreamId]),
  {noreply, State};


handle_info(Message, #rtmp_session{module = M} = State) ->
  M:handle_info(Message, State).



%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------



% send(#rtmp_session{server_chunk_size = ChunkSize} = State, {#channel{} = Channel, Data}) ->
%   Packet = rtmp:encode(Channel#channel{chunk_size = ChunkSize}, Data),
%   % ?D({"Channel", Channel#channel.type, Channel#channel.timestamp, Channel#channel.length}),
%   send_data(State, Packet).


handle_rtmp_message(State, #rtmp_message{type = invoke, body = AMF}) ->
  #rtmp_funcall{command = CommandBin} = AMF,
  Command = binary_to_atom(CommandBin, utf8),
  call_function(State, AMF#rtmp_funcall{command = Command});

handle_rtmp_message(#rtmp_session{socket = Socket} = State, #rtmp_message{type = Type, stream_id = StreamId, body = Body, timestamp = Timestamp})
  when ((Type == video) or (Type == audio) or (Type == metadata) or (Type == metadata3)) andalso is_number(Timestamp)  ->
  case get_stream(StreamId, State) of
    #rtmp_stream{pid = Recorder} when is_pid(Recorder) ->
      Type1 = case Type of
        metadata3 -> metadata;
        _ -> Type
      end,
      Frame = flv_video_frame:decode(#video_frame{dts = Timestamp, pts = Timestamp, content = Type1}, Body),
      Recorder ! Frame,
      State;
    #rtmp_stream{} ->
      ?D({broken_frame_on_empty_stream,Type,StreamId,Timestamp}),
      State;
    false ->
      rtmp_socket:status(Socket, StreamId, <<"NetStream.Publish.Failed">>),
      State
  end;

handle_rtmp_message(State, #rtmp_message{type = shared_object, body = SOEvent}) ->
  #so_message{name = Name, persistent = Persistent} = SOEvent,
  ?D({"Shared object", Name}),
  {NewState, Object} = find_shared_object(State, Name, Persistent),
  shared_object:message(Object, SOEvent),
  NewState;

handle_rtmp_message(State, #rtmp_message{type = pong}) -> State;
handle_rtmp_message(State, #rtmp_message{type = ping}) -> State;
handle_rtmp_message(State, #rtmp_message{type = ack_read}) -> State;
handle_rtmp_message(State, #rtmp_message{type = window_size}) -> State;
handle_rtmp_message(State, #rtmp_message{type = chunk_size}) -> State;
handle_rtmp_message(State, #rtmp_message{type = broken_meta}) -> State;

handle_rtmp_message(#rtmp_session{module = M} = State, Message) ->
  {ok, State1} = M:handle_control({rtmp, Message}, State),
  State1.


find_shared_object(#rtmp_session{cached_shared_objects = Objects} = State, Name, Persistent) ->
  case lists:keysearch(Name, 1, Objects) of
    false ->
      Object = shared_objects:open(Name, Persistent),
      NewObjects = lists:keystore(Name, 1, Objects, {Name, Object}),
      {State#rtmp_session{cached_shared_objects = NewObjects}, Object};
    {value, {Name, Object}} ->
      {State, Object}
  end.

call_function(#rtmp_session{} = State, #rtmp_funcall{command = connect, args = [{object, PlayerInfo} | _]} = AMF) ->
  URL = proplists:get_value(tcUrl, PlayerInfo),
  {match, [_Proto, HostName, _Port, Path]} = re:run(URL, "(.*)://([^/:]+)([^/]*)/?(.*)$", [{capture,all_but_first,binary}]),
  Host = ems:host(HostName),
  %%?D({"Client connecting", HostName, Host, AMF#rtmp_funcall.args}),

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

	NewState1 =	State#rtmp_session{player_info = PlayerInfo, path = Path, amf_ver = AMFVersion, session_id = SessionId},

	call_function_callback(rtmp_session:set(NewState1, host, Host), AMF);


call_function(#rtmp_session{} = State, #rtmp_funcall{} = AMF) ->
  call_function_callback(State, AMF).

call_function_callback(#rtmp_session{module = M} = Session, #rtmp_funcall{} = AMF) ->
  case M:handle_rtmp_call(Session, AMF) of
    unhandled ->
      call_default_function(Session, AMF);
    {unhandled, Session1, AMF1} ->
      call_default_function(Session1, AMF1);
    #rtmp_session{} = Session1 ->
      Session1
  end.

call_default_function(#rtmp_session{module = M} = Session, #rtmp_funcall{command = Command} = AMF) ->
  case erlang:function_exported(?MODULE, Command, 2) of
    true -> ?MODULE:Command(Session, AMF);
    false ->
      case M:handle_control({unhandled_call, AMF}, Session) of
        {ok, Session1} -> Session1;
        Else -> Else
      end
  end.








handle_frame(#video_frame{} = Frame, Session) ->
  case flv_video_frame:is_good_flv(Frame) of
    true -> send_frame(Frame, Session);
    _ -> Session
  end.

send_frame(#video_frame{content = Content, stream_id = StreamId, dts = DTS, pts = PTS} = Frame,
             #rtmp_session{socket = Socket, bytes_sent = Sent, module = M} = State) ->
  {State1, BaseDts, _Starting, Allow} = case rtmp_session:get_stream(StreamId, State) of
    #rtmp_stream{base_dts = DTS_, receive_audio = false} when Content == audio ->
      {State, DTS_, false, false};
    #rtmp_stream{base_dts = DTS_, receive_video = false} when Content == video ->
      {State, DTS_, false, false};
    #rtmp_stream{seeking = true} ->
      {State, undefined, false, false};
    #rtmp_stream{pid = undefined, started = false} = Stream ->
      rtmp_lib:play_start(Socket, StreamId, 0, file),
      State1_ = set_stream(Stream#rtmp_stream{started = true, base_dts = DTS}, State),
      {State1_, DTS, true, true};
    #rtmp_stream{pid = Media, started = false, options = Options} = Stream ->
      MediaType = proplists:get_value(type, ems_media:info(Media)),
      rtmp_lib:play_start(Socket, StreamId, 0, MediaType),
      State1_ = set_stream(Stream#rtmp_stream{started = true, base_dts = DTS}, State),
      Duration = proplists:get_value(duration, Options),
      {ok, State2_} = M:handle_control({start_stream, Media, [{duration,Duration},{stream_id,StreamId},{dts,DTS}]}, State1_),
      {State2_, DTS, true, true};
    #rtmp_stream{base_dts = DTS_} ->
      {State, DTS_, false, true};
    Else ->
      erlang:error({old_frame, Else,StreamId})
  end,

  case Allow of
    true ->
      % case Frame#video_frame.content of
      %   metadata -> ?D(Frame);
      %   _ ->
      %     (catch ?D({StreamId, Frame#video_frame.codec,Frame#video_frame.flavor, round(DTS), rtmp:justify_ts(DTS - BaseDts), Frame#video_frame.sound})),
      %     ok
      % end,

      send_rtmp_frame(Socket, Frame#video_frame{dts = rtmp:justify_ts(DTS - BaseDts), pts = rtmp:justify_ts(PTS - BaseDts)}),
    	Size = try iolist_size(Frame#video_frame.body) of
    	  S -> S
    	catch
    	  _:_ -> 0
    	end,
      State1#rtmp_session{bytes_sent = Sent + Size};
    false ->
      State1
  end.

send_rtmp_frame(Socket, #video_frame{content = Type, stream_id = StreamId, dts = DTS} = Frame) ->
  Message = #rtmp_message{
    channel_id = rtmp_lib:channel_id(Type, StreamId),
    timestamp = DTS,
    type = Type,
    stream_id = StreamId,
    body = flv_video_frame:encode(Frame)},
	rtmp_socket:send(Socket, Message).


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
terminate(_Reason, #rtmp_session{module = M} = State) ->
  M:handle_control(terminate, State),
  ok.




flush_stream(StreamId) ->
  receive
    #video_frame{stream_id = StreamId} -> flush_stream(StreamId)
    after 0 -> ok
  end.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVersion, _State, _Extra) ->
  ok.

find_stream_by_pid(PlayerPid, #rtmp_session{streams1 = Streams}) ->
  find_stream_by_pid(PlayerPid, Streams);

find_stream_by_pid(PlayerPid, Streams) ->
  lists:keyfind(PlayerPid, #rtmp_stream.pid, Streams).

get_stream(StreamId, #rtmp_session{streams1 = Streams}) ->
  lists:keyfind(StreamId, #rtmp_stream.stream_id, Streams).

set_stream(#rtmp_stream{} = Stream, #rtmp_session{streams1 = Streams} = State) ->
  Streams1 = lists:ukeymerge(#rtmp_stream.stream_id, [Stream], Streams),
  State#rtmp_session{streams1 = Streams1}.

alloc_stream(#rtmp_session{streams1 = Streams}) ->
  StreamNumbers = [N || #rtmp_stream{stream_id = N} <- Streams],
  alloc_stream(StreamNumbers, 1).

alloc_stream(StreamNumbers, N) ->
  case lists:member(N, StreamNumbers) of
    true -> alloc_stream(StreamNumbers, N+1);
    false -> #rtmp_stream{stream_id = N}
  end.


delete_stream(#rtmp_stream{stream_id = StreamId}, State) ->
  delete_stream(StreamId, State);

delete_stream(StreamId, #rtmp_session{streams1 = Streams} = State) ->
  State#rtmp_session{streams1 = lists:keydelete(StreamId, #rtmp_stream.stream_id, Streams)}.

get_socket(#rtmp_session{socket = Socket}) -> Socket.




%%-------------------------------------------------------------------------
%% Default RTMP functionality
%%-------------------------------------------------------------------------


connect(#rtmp_session{} = State, _AMF) ->
  rtmp_session:accept_connection(State).


%% @private
createStream(#rtmp_session{} = State, AMF) ->
  #rtmp_stream{stream_id = StreamId} = Stream = rtmp_session:alloc_stream(State),
  rtmp_session:reply(State,AMF#rtmp_funcall{args = [null, StreamId]}),
  % ?D({createStream,StreamId}),
  rtmp_session:set_stream(Stream, State).

releaseStream(State, #rtmp_funcall{} = _AMF) ->
  State.



closeStream(#rtmp_session{} = State, #rtmp_funcall{stream_id = StreamId}) ->
  % ?D({closeStream,StreamId}),
  close_stream(State, StreamId).


close_stream(#rtmp_session{module = M} = State, StreamId) ->
  case rtmp_session:get_stream(StreamId, State) of
    #rtmp_stream{pid = Player, recording = Recording, recording_ref = Ref, name = _Name} when is_pid(Player) ->
      (catch erlang:demonitor(Ref, [flush])),
      {ok, State1} = M:handle_control({close_stream, StreamId, Player, Recording}, State),
      rtmp_session:flush_stream(StreamId),
      rtmp_session:set_stream(#rtmp_stream{stream_id = StreamId}, State1);
    _ ->
      State
  end.


%%-------------------------------------------------------------------------
%% @private
%%-------------------------------------------------------------------------
deleteStream(State, #rtmp_funcall{stream_id = StreamId} = _AMF) ->
  close_stream(State, StreamId),
  rtmp_session:delete_stream(StreamId, State).


receiveAudio(State, #rtmp_funcall{args = [null, Flag], stream_id = StreamId}) when Flag == true orelse Flag == false ->
  Stream = rtmp_session:get_stream(StreamId, State),
  rtmp_session:set_stream(Stream#rtmp_stream{receive_audio = Flag}, State).

receiveVideo(State, #rtmp_funcall{args = [null, Flag], stream_id = StreamId}) when Flag == true orelse Flag == false ->
  Stream = rtmp_session:get_stream(StreamId, State),
  rtmp_session:set_stream(Stream#rtmp_stream{receive_video = Flag}, State).

