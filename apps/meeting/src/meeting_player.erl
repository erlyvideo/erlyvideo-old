%%% @author     Yura Zhloba <yzh44yzh@gmail.com> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        Player of multistream video
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%%---------------------------------------------------------------------------------------
-module(meeting_player).
-author('Yura Zhloba <yzh44yzh@gmail.com>').
-behaviour(gen_server).

-include("meeting.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("kernel/include/file.hrl").

-define(SORT_BUFFER, 40).

%% External API
-export([start_link/2]).
-export([play/2]).

-export([dump/2, list/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(player, {
    meeting,
    reader,
    subscriber,
    previous_frame,
    start_time,
    client_buffer,
    streams = []
  }).

-record(stream, {
  user_id,
  out_id,
  start_dts
}).

%% Server-less dump of file

format(DTS) ->
  round(DTS / 1000).

dump(Host, ConfName) when is_atom(Host) ->
  {ok, #player{reader = Reader}} = init([ConfName, [{host,Host}]]),
  put(last_dts, 0),
  dump(Reader, flv:data_offset());

dump(Reader, Key) ->
  case flv_reader:read_frame(Reader, Key) of
    #video_frame{content = metadata, next_id = Next, dts = DTS, stream_id = StreamId, body = [<<"onMetaData">>, {object, Meta}]} = F->
      case proplists:get_value(action, Meta) of
        <<"newUser">> -> io:format("~3.B ~3.B userJoined~n", [format(DTS), StreamId]);
        <<"publishStart">> -> io:format("~3.B ~3.B publishStart~n", [format(DTS), StreamId]);
        <<"publishStop">> -> put(StreamId, undefined), io:format("~3.B ~3.B publishStop~n", [format(DTS), StreamId]);
        <<"removeUser">> -> io:format("~3.B ~3.B userLeft~n", [format(DTS), StreamId]);
        <<"message">> -> io:format("~3.B ~3.B message: ~s~n", [format(DTS), StreamId, proplists:get_value(body, Meta)]);
        _ -> ?D({meta, F})
      end,
      put(last_dts, DTS),
      dump(Reader, Next);
    #video_frame{next_id = Next, dts = DTS} = F ->
      dump_frame(F),
      put(last_dts, DTS),
      dump(Reader, Next);
    eof ->
      ?D({eof, format(get(last_dts))})
  end.

dump_frame(#video_frame{stream_id = StreamId, dts = DTS}) -> 
 case get({stream,StreamId}) of
   undefined -> io:format("~3.B ~3.B videoflow~n", [format(DTS), StreamId]), put({stream,StreamId}, DTS);
   _ -> ok
  end.

start_link(Conference, Options) ->
  gen_server_ems:start_link(?MODULE, [Conference, Options], []).

play(Player, Session) ->
  gen_server:cast(Player, {play, Session}).


list(Host) ->
  Dir = meeting_saver:get_records_dir(Host),
  lists:map(fun(Path) ->
    Name = list_to_binary(filename:basename(Path, ".flv")),
    {ok, #file_info{mtime = MTime}} = file:read_file_info(Path),
    {{Y,M,D},{H,Min,S}} = MTime,
    Time = iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y,M,D,H,Min,S])),
    [{name,Name},{time,Time}]
  end, filelib:wildcard(Dir ++ "/*.flv")).

init([Conference, Options]) when is_binary(Conference) ->
    init([binary_to_list(Conference), Options]);

init([Conference, Options]) ->
  % ?D({init, Conference}),
  Host = proplists:get_value(host, Options),
  File = meeting_file_chooser:get_for_reading(meeting_saver:get_records_dir(Host), Conference),
  ?D({read_from_file, File}),
  {ok, F} = file:open(File, [read,binary]),
  {ok, Reader} = flv_reader:init({file, F}, [{find_metadata,false}]),
  ClientBuffer = proplists:get_value(client_buffer, Options, 0),
  {ok, #player{meeting = Conference, reader = Reader, client_buffer = ClientBuffer}}.


handle_call(Request, _From, State) ->
  ?D({unknown_call, Request}),
  {stop, {unknown_call, Request}, State}.


handle_cast({play, Session}, State) ->
  ?D({play, Session}),
  erlang:monitor(process, Session),
  self() ! next_frame,
  {noreply, State#player{subscriber = Session, start_time = erlang:now()}};

handle_cast(Msg, State) ->
  ?D({unknown_cast, Msg}),
  {stop, {unknown_cast, Msg}, State}.

handle_info({next_frame, StreamId, UserId}, #player{streams = Streams, previous_frame = #video_frame{dts = DTS}} = State) ->
  Stream = #stream{user_id = UserId, out_id = StreamId, start_dts = DTS},
  handle_info(next_frame, State#player{streams = lists:keystore(UserId, #stream.user_id, Streams, Stream)});

handle_info(next_frame, #player{previous_frame = PrevFrame, reader = Reader} = State) ->
  NeedToResend = send_frame(State, PrevFrame),
  NextKey = case PrevFrame of
    undefined -> undefined;
    #video_frame{next_id = Key} -> Key
  end,
  case flv_reader:read_frame(Reader, NextKey) of
      eof -> {stop, normal, State};
      Frame -> process_frame(Frame, NeedToResend, State)
  end;

handle_info({'DOWN', _, process, _Pid, _Reason}, State) ->
  ?D({stop_meeting_player, self()}),
  {stop, normal, State};

handle_info(Info, State) ->
  ?D({invalid_message, Info}),
  {stop, {invalid_message, Info}, State}.


terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


send_frame(#player{subscriber = Session, streams = Streams}, #video_frame{content = metadata, stream_id = UserId, body = [Method, {object, Params}]} = _F) ->
  Action = proplists:get_value(action, Params),
  NewId = case Action of
    <<"publishStop">> -> (lists:keyfind(UserId, #stream.user_id, Streams))#stream.out_id;
    _ -> 0
  end,
  % Socket ! F, %#video_frame{stream_id = NewId},
  % NewId = 0,
  ?D({action,Action, round(_F#video_frame.dts)}),
  Socket = rtmp_session:get(Session, socket),
  ?D({send, Session, Socket, UserId, NewId, Params}),
  rtmp_socket:status(Socket, NewId, Method, Params),
  AutoTick = Action =/= <<"publishStart">>,
  AutoTick;
  

send_frame(#player{subscriber = Socket, streams = Streams} = _State, #video_frame{stream_id = UserId, dts = DTS, pts = PTS} = F) ->
  #stream{out_id = NewId, start_dts = StartDTS} = lists:keyfind(UserId, #stream.user_id, Streams),
  % ?D({send,NewId,round(DTS),StartDTS}),
  Socket ! F#video_frame{stream_id = NewId, dts = DTS - StartDTS, pts = PTS - StartDTS},
  true;

send_frame(_State, undefined) ->
  true.


process_frame(#video_frame{dts = DTS} = Frame, NeedToResend, #player{start_time = Start, client_buffer = ClientBuffer} = State) ->
  % ?D({frame,Frame#video_frame.stream_id,Frame#video_frame.content,Frame#video_frame.dts}),
  case NeedToResend of
    true -> schedule_tick(DTS, Start, ClientBuffer);
    false -> ok
  end,

  {noreply, State#player{previous_frame = Frame}}.

schedule_tick(DTS, Start, ClientBuffer) ->
  RealDelta = timer:now_diff(erlang:now(), Start) div 1000,
  StreamDelta = round(DTS) - 0 ,
  Timeout = case StreamDelta - RealDelta - ClientBuffer of
    Delta when Delta < 0 -> 0;
    Delta -> Delta
  end,
  % ?D({tick,round(DTS),RealDelta,Timeout}),
  if Timeout == 0 -> self() ! next_frame;
    true -> timer:send_after(Timeout, next_frame)
  end.
