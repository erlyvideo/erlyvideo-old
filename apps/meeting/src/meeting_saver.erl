%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        Video gluer in meetingr
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%%
%%%---------------------------------------------------------------------------------------
-module(meeting_saver).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).
-include("meeting.hrl").

-include_lib("erlmedia/include/video_frame.hrl").

-define(SORT_BUFFER, 40).

%% External API
-export([start_link/2]).

-export([add_user/3, remove_user/2, add_stream/3, remove_stream/2, add_message/4, get_records_dir/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(saver, {
  name,
  host,
  writer,
  start_dts,
  meeting,
  streams = [],
  options
}).

-record(in_stream, {
  stream_id,
  pid,
  ref,
  delta,
  start_dts,
  mask = [video, audio, text],
  position
}).


start_link(Conference, Options) ->
  gen_server_ems:start_link(?MODULE, [Conference, Options], []).


%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

add_user(Saver, UserId, UserName) ->
  gen_server:cast(Saver, {add_user, UserId, UserName}).

remove_user(Saver, UserId) ->
  gen_server:cast(Saver, {remove_user, UserId}).

add_stream(Saver, Stream, UserId) when is_pid(Stream) ->
  gen_server:cast(Saver, {add_stream, Stream, UserId}).

remove_stream(Saver, UserId) ->
  gen_server:cast(Saver, {remove_stream, UserId}).

add_message(Saver, UserId, UserName, Body) ->
  gen_server:cast(Saver, {add_message, UserId, UserName, Body}).

get_records_dir(Host) ->
  ems:get_var(meeting_records_dir, Host, ems:get_var(file_dir, Host, "/tmp")) ++ "/meetings".

init([Conference, Options]) ->
  Name = proplists:get_value(name, Options),
  Host = proplists:get_value(host, Options),
  File = meeting_file_chooser:get_for_writing(get_records_dir(Host), binary_to_list(Name)),
  ?D({write_to_file, File}),
  {ok, Writer} = flv_writer:start_link(File, [{sort_buffer, false}]),
  erlang:monitor(process, Conference),
  {ok, #saver{meeting = Conference,
              writer = Writer,
              name = Name,
              start_dts = erlang:now(),
              options = Options}}.


handle_call(Request, _From, State) ->
  ?D({unknown_call, Request}),
  {stop, {unknown_call, Request}, State}.

stream_dts(#saver{start_dts = StartDTS}) ->
  timer:now_diff(erlang:now(), StartDTS) div 1000.

handle_cast({add_user, UserId, UserName}, #saver{writer = Writer} = State) ->
  DTS = stream_dts(State),
  Frame = #video_frame{content = metadata, stream_id = UserId, dts = DTS, pts = DTS,
    body = [<<"onMetaData">>, {object, [{action, <<"newUser">>},
                                        {user_id, UserId},
                                        {user_name, UserName}]}]},
  ?D({add_user, State#saver.name, UserId, UserName}),
  flv_writer:write_frame(Frame, Writer),
  {noreply, State};

handle_cast({remove_user, UserId}, #saver{writer = Writer} = State) ->
  DTS = stream_dts(State),
  Frame = #video_frame{content = metadata, stream_id = UserId, dts = DTS, pts = DTS,
    body = [<<"onMetaData">>, {object, [{action, <<"removeUser">>}, {user_id, UserId}]}]},
  ?D({remove_user, State#saver.name, UserId}),
  flv_writer:write_frame(Frame, Writer),
  {noreply, State};

handle_cast({add_stream, Stream, UserId}, #saver{streams = Streams, writer = Writer} = State) ->
  DTS = stream_dts(State),

  Frame = #video_frame{content = metadata, stream_id = UserId, dts = DTS, pts = DTS,
    body = [<<"onMetaData">>, {object, [{action, <<"publishStart">>}, {user_id, UserId}]}]},
  ?D({add_stream, State#saver.name, UserId, Stream, stream_dts(State)}),
  flv_writer:write_frame(Frame, Writer),

  InStream = #in_stream{stream_id = UserId, pid = Stream, delta = stream_dts(State)},

  ems_media:play(Stream, [{stream_id, UserId}]),
  {noreply, State#saver{streams = [InStream|Streams]}};

handle_cast({remove_stream, UserId}, #saver{streams = Streams, writer = Writer} = State) ->
  case lists:keytake(UserId, #in_stream.stream_id, Streams) of
    false ->
      {noreply, State};
    {value, #in_stream{pid = Stream}, NewStreams} ->
      ems_media:stop(Stream),
      DTS = stream_dts(State),
      Frame = #video_frame{content = metadata, stream_id = UserId, dts = DTS, pts = DTS,
        body = [<<"onMetaData">>, {object, [{action, <<"publishStop">>}, {user_id, UserId}]}]},
      ?D({remove_stream, State#saver.name, UserId}),
      flv_writer:write_frame(Frame, Writer),
      {noreply, State#saver{streams = NewStreams}}
  end;

handle_cast({add_message, UserId, UserName, Body}, #saver{writer = Writer} = State) ->
  DTS = stream_dts(State),
      Frame = #video_frame{content = metadata, stream_id = UserId, dts = DTS, pts = DTS,
        body = [<<"onMetaData">>, {object, [{action, <<"message">>},
                                            {user_id, UserId},
                                            {user_name, UserName},
                                            {body, Body}]}]},
      ?D({add_message, State#saver.name, UserId, UserName}),
      flv_writer:write_frame(Frame, Writer),
      {noreply, State};

handle_cast(Msg, State) ->
  ?D({unknown_cast, Msg}),
  {stop, {unknown_cast, Msg}, State}.

handle_info(#video_frame{stream_id = StreamId, dts = DTS, pts = PTS} = Frame, #saver{streams = Streams, writer = Writer} = State) ->
  case lists:keyfind(StreamId, #in_stream.stream_id, Streams) of
    #in_stream{start_dts = undefined, delta = Delta} = Stream ->
      flv_writer:write_frame(Frame#video_frame{dts = Delta, pts = Delta}, Writer),
      {noreply, State#saver{streams = lists:keystore(StreamId, #in_stream.stream_id, Streams, Stream#in_stream{start_dts = DTS})}};
    #in_stream{delta = Delta, start_dts = StartDTS} -> 
      flv_writer:write_frame(Frame#video_frame{dts = DTS- StartDTS+Delta, pts = PTS-StartDTS+Delta}, Writer),
      {noreply, State};
    _ -> 
      erlang:error({unknown_stream_id, StreamId, Streams})
  end;

handle_info(stop, State) ->
  ?D({stop_meeting_saver, self()}),
  {stop, normal, State};

handle_info({'DOWN', _, process, _Conference, _Reason}, State) ->
  {stop, normal, State};

handle_info(Info, State) ->
  ?D({invalid_message, Info}),
  {stop, {invalid_message, Info}, State}.


terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

