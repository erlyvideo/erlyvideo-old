%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTMP encoding/decoding module. 
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information.
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
-module(rtmp_lib).
-author('Max Lapshin <max@maxidoors.ru>').
-version(1.1).

-include("../include/rtmp.hrl").
-export([wait_for_reply/2]).
-export([connect/1, connect/2, createStream/1, play/1, play/3, seek/3, pause/3, resume/3, publish/3, publish/4]).
-export([shared_object_connect/2, shared_object_set/4]).
-export([play_complete/3, play_failed/2, seek_notify/3, seek_failed/2, play_start/4, pause_notify/2, unpause_notify/3]).
-export([channel_id/2, empty_audio/2]).
-export([accept_connection/1, accept_connection/2, reject_connection/1]).
-export([reply/2, reply/3, fail/2]).
-export([notify_publish_start/4]).

-export([call/4]).

-define(RTMP_WINDOW_SIZE, 2500000).
-define(FMS_VERSION, "4,0,0,1121").

-define(D(X), ems_log:debug(3, rtmp, "~p:~p ~p",[?MODULE, ?LINE, X])).


wait_for_reply(RTMP, InvokeId) when is_integer(InvokeId) ->
  wait_for_reply(RTMP, InvokeId*1.0);
  
%% @private
wait_for_reply(RTMP, InvokeId) ->
  receive
    {rtmp, RTMP, #rtmp_message{type = invoke, body = #rtmp_funcall{command = <<"_result">>, id = InvokeId, args = Args}}} -> Args
  after
    30000 -> erlang:error(timeout)
  end.

default_connect_options(URL) ->
  {App, _Path} = app_path(URL),
  {Host, _} = http_uri2:extract_path_with_query(URL),
  lists:ukeysort(1, [
    {app, list_to_binary(App)},
    {flashVer,<<"MAC 10,0,32,18">>},
    {swfUrl,<<"http://localhost/player/Player.swf">>},
    {tcUrl,list_to_binary("rtmp://"++Host)},
    {fpad,false},
    {capabilities,15.0},
    {audioCodecs,3191.0},
    {videoCodecs,252.0},
    {videoFunction,1.0},
    {pageUrl,<<"http://localhost:8082/">>},
    {objectEncoding,0.0}
  ]).

empty_audio(StreamId, DTS) ->
  #rtmp_message{type = audio, body = <<>>, timestamp = DTS, ts_type = new, stream_id = StreamId, channel_id = rtmp_lib:channel_id(audio, StreamId)}.


accept_connection(RTMP) -> accept_connection(RTMP, []).

accept_connection(RTMP, Options) ->
  AMFVersion = proplists:get_value(amf_version, Options, 0),
  
  Message = #rtmp_message{channel_id = 2, timestamp = 0, body = <<>>},
  % gen_server:call(self(), {invoke, AMF#rtmp_funcall{command = 'onBWDone', type = invoke, id = 2, stream_id = 0, args = [null]}}),
  rtmp_socket:send(RTMP, Message#rtmp_message{type = window_size, body = ?RTMP_WINDOW_SIZE}),
  rtmp_socket:send(RTMP, Message#rtmp_message{type = bw_peer, body = ?RTMP_WINDOW_SIZE}),
  rtmp_socket:send(RTMP, Message#rtmp_message{type = stream_begin, stream_id = 0}),

  ConnectObj = [{fmsVer, <<"FMS/",?FMS_VERSION>>}, {capabilities, 31}, {mode, 1}],
  StatusObj = [{level, <<"status">>},
               {code, <<"NetConnection.Connect.Success">>},
               {description, <<"Connection succeeded.">>},
               {data,[{<<"version">>, <<?FMS_VERSION>>}]},
               {objectEncoding, AMFVersion}],
  reply(RTMP, #rtmp_funcall{id = 1, args = [{object, ConnectObj}, {object, StatusObj}]}),
  rtmp_socket:setopts(RTMP, [{chunk_size, 16#200000}]),
  rtmp_socket:setopts(RTMP, [{amf_version, AMFVersion}]),
  ok.

reject_connection(RTMP) ->
  ConnectObj = [{fmsVer, <<"FMS/", ?FMS_VERSION>>}, {capabilities, 31}, {mode, 1}],
  StatusObj = [{level, <<"status">>},
               {code, <<"NetConnection.Connect.Rejected">>},
               {description, <<"Connection rejected.">>}],
  reply(RTMP, #rtmp_funcall{id = 1, args = [{object, ConnectObj}, {object, StatusObj}]}),
  ok.


reply(RTMP, AMF, Args) -> reply(RTMP, AMF#rtmp_funcall{args = [null|Args]}).

reply(RTMP, AMF) -> rtmp_socket:invoke(RTMP, AMF#rtmp_funcall{command = '_result', type = invoke}).
fail(RTMP, AMF) -> rtmp_socket:invoke(RTMP, AMF#rtmp_funcall{command = '_error', type = invoke}).


%% @spec (RTMP::rtmp_socket()) -> any()
%% @doc Send connect request to server with some predefined params
-spec connect(RTMP::rtmp_socket_pid()) -> ok.
connect(RTMP) ->
  {url, URL} = rtmp_socket:getopts(RTMP, url),
  connect(RTMP, default_connect_options(URL)).

%% @spec (RTMP::rtmp_socket(), Options::[{Key::atom(), Value::any()}]) -> any()
%% @doc Send connect request to server
connect(RTMP, Options) ->
  {url, URL} = rtmp_socket:getopts(RTMP, url),
  ConnectArgs = lists:ukeymerge(1, lists:ukeysort(1, Options), default_connect_options(URL)),
  InvokeId = 1,
  AMF = #rtmp_funcall{
    command = connect,
    type = invoke,
    id = InvokeId,
    stream_id = 0,
    args = [{object, ConnectArgs}]
  },
  % io:format("~p -> ~p~n", [{connect, Options}, ConnectArgs]),
  rtmp_socket:invoke(RTMP, AMF),
  wait_for_reply(RTMP, InvokeId),
  ok.
  
createStream(RTMP) ->
  InvokeId = 1,
  AMF = #rtmp_funcall{
    command = createStream,
    type = invoke,
    id = InvokeId,
    stream_id = 0,
    args = [null]
  },
  rtmp_socket:invoke(RTMP, AMF),
  [null, StreamId] = wait_for_reply(RTMP, InvokeId),
  round(StreamId).


play(URL) when is_list(URL) orelse is_binary(URL) ->
  case rtmp_socket:connect(URL) of
    {ok, RTMP} ->
      receive
        {rtmp, RTMP, connected} -> ok
      after
        10000 -> erlang:raise({timeout,{rtmp,URL}})
      end,
      invoke_rtmp_play(RTMP, URL);
    Else ->
      Else
  end.
  
app_path(URL) ->
  {_, FullPath} = http_uri2:extract_path_with_query(URL),
  case re:run(FullPath, "/(.*)//(.*)", [{capture,all_but_first,list}]) of
    {match, [App1, Path1]} ->
      {App1, Path1};
    _ ->
      [App1|PathParts] = string:tokens(FullPath, "/"),
      Path1 = string:join(PathParts, "/"),
      {App1, Path1}
  end.
  

invoke_rtmp_play(RTMP, URL) ->
  rtmp_socket:setopts(RTMP, [{active, true}]),
  connect(RTMP),
  {_App, Path} = app_path(URL),
  ?D({"Connected to RTMP source", URL, _App, Path}),
  Stream = rtmp_lib:createStream(RTMP),
  ?D({"Stream",Stream}),
  play(RTMP, Stream, Path),
  ?D({"Playing", Path}),
  
  {ok, RTMP}.


play(RTMP, Stream, Path) when is_list(Path) ->
  play(RTMP, Stream, list_to_binary(Path));
  
play(RTMP, Stream, Path) ->
  AMF = #rtmp_funcall{
    command = play,
    type = invoke,
    id = 2,
    stream_id = Stream,
    args = [null, Path]
  },
  rtmp_socket:invoke(RTMP, AMF),
  receive
    {rtmp, RTMP, #rtmp_message{type = stream_begin}} -> ok
  after
    30000 -> erlang:error(timeout)
  end.

call(RTMP, Stream, Name, Args) ->
  AMF = #rtmp_funcall{
    command = Name,
    type = invoke,
    stream_id = Stream,
    args = [null | Args]
  },
  rtmp_socket:invoke(RTMP, AMF),
  ok.
  

pause(RTMP, Stream, DTS) ->
  call(RTMP, Stream, pause, [true, DTS]).
  

resume(RTMP, Stream, DTS) ->
  call(RTMP, Stream, pause, [false, DTS]).

seek(RTMP, Stream, DTS) ->
  call(RTMP, Stream, seek, [DTS]),
  receive
    {rtmp, RTMP, #rtmp_message{type = stream_begin}} -> ok
  after
    30000 -> erlang:error(timeout)
  end.

publish(RTMP, Stream, [Path, live]) ->
  publish(RTMP, Stream, Path, live);

publish(RTMP, Stream, [Path, record]) ->
  publish(RTMP, Stream, Path, record);

publish(RTMP, Stream, Path) ->
  publish(RTMP, Stream, Path, live).

publish(RTMP, Stream, Path, Type) when is_list(Path) ->
  publish(RTMP, Stream, list_to_binary(Path), Type);

publish(RTMP, Stream, Path, Type) ->
  AMF = #rtmp_funcall{
    command = publish,
    type = invoke,
    stream_id = Stream,
    args = [null, Path, atom_to_binary(Type, latin1)]
  },
  rtmp_socket:invoke(RTMP, AMF),
  receive
    {rtmp, RTMP, #rtmp_message{type = stream_begin}} -> ok
  after
    30000 -> erlang:error(timeout)
  end.

shared_object_connect(RTMP, Name) ->
  rtmp_socket:send(RTMP, #rtmp_message{type=shared_object, body=#so_message{name = Name, events=[connect]}}),
  receive
    {rtmp, RTMP, #rtmp_message{type = shared_object, body = #so_message{name = Name}}} -> ok
  after
    30000 -> erlang:error(timeout)
  end.
  
shared_object_set(RTMP, Name, Key, Value) ->
  rtmp_socket:send(RTMP, #rtmp_message{type=shared_object, body=#so_message{name = Name, events=[{set_attribute, {Key, Value}}]}}),
  receive
    {rtmp, RTMP, #rtmp_message{type = shared_object, body = #so_message{name = Name}}} -> ok
  after
    30000 -> erlang:error(timeout)
  end.
  

-spec play_start(RTMP::pid(), StreamId::non_neg_integer(), DTS::timestamp_type(), Type::live|file) -> ok.
play_start(RTMP, StreamId, DTS, Type) ->
  % rtmp_socket:send(RTMP, #rtmp_message{type = abort, body = channel_id(audio, StreamId), timestamp = DTS}),
  % rtmp_socket:send(RTMP, #rtmp_message{type = abort, body = channel_id(video, StreamId), timestamp = DTS}),
  case Type of
    resume -> ok;
    _ -> 
      Reset = rtmp_socket:prepare_status(StreamId, <<"NetStream.Play.Reset">>),
      rtmp_socket:send(RTMP, Reset#rtmp_message{timestamp = DTS, channel_id = channel_id(metadata, StreamId)})
  end,

  case Type of
    live -> ok;
    _ -> rtmp_socket:send(RTMP, #rtmp_message{type = stream_recorded, stream_id = StreamId, timestamp = DTS, ts_type = new})
  end,
  rtmp_socket:send(RTMP, #rtmp_message{type = stream_begin, stream_id = StreamId, timestamp = DTS, ts_type = new}),
  
  
  PlayStart = rtmp_socket:prepare_status(StreamId, <<"NetStream.Play.Start">>),
  rtmp_socket:send(RTMP, PlayStart#rtmp_message{timestamp = DTS, channel_id = channel_id(metadata, StreamId)}),

  rtmp_socket:send(RTMP, #rtmp_message{type = metadata, channel_id = channel_id(metadata, StreamId), stream_id = StreamId,
    body = [<<"|RtmpSampleAccess">>, true, true], timestamp = DTS, ts_type = delta}),
    
  % rtmp_socket:send(RTMP, #rtmp_message{type = audio, body = <<>>, timestamp = DTS, channel_id = channel_id(audio, StreamId), stream_id = StreamId}),
    
  Notify = rtmp_socket:prepare_notify(StreamId, <<"onStatus">>, [{code, <<"NetStream.Data.Start">>}]),
  rtmp_socket:send(RTMP, Notify#rtmp_message{channel_id = channel_id(metadata, StreamId), timestamp = DTS}),
  
  ok.
  

pause_notify(RTMP, StreamId) ->
  % rtmp_socket:send(RTMP, #rtmp_message{type = stream_maybe_seek, stream_id = StreamId}),
  % rtmp_socket:send(RTMP, #rtmp_message{type = stream_end, stream_id = StreamId}),
  rtmp_socket:status(RTMP, StreamId, <<"NetStream.Pause.Notify">>),
  ok.

unpause_notify(RTMP, StreamId, DTS) ->
  Status = rtmp_socket:prepare_status(StreamId, <<"NetStream.Unpause.Notify">>),
  rtmp_socket:send(RTMP, Status#rtmp_message{channel_id = rtmp_lib:channel_id(metadata, StreamId), ts_type = delta, timestamp = 0}),
  play_start(RTMP, StreamId, DTS, resume),
  ok.
  
  

seek_notify(RTMP, StreamId, DTS) ->
  rtmp_socket:send(RTMP, #rtmp_message{type = stream_end, stream_id = StreamId, ts_type = new}),
  rtmp_socket:send(RTMP, #rtmp_message{type = stream_recorded, stream_id = StreamId, timestamp = 0, ts_type = new}),
  rtmp_socket:send(RTMP, #rtmp_message{type = stream_begin, stream_id = StreamId, timestamp = 0, ts_type = new}),

  SeekStatus = rtmp_socket:prepare_status(StreamId, <<"NetStream.Seek.Notify">>),
  rtmp_socket:send(RTMP, SeekStatus#rtmp_message{timestamp = DTS, channel_id = channel_id(metadata, StreamId), ts_type = new}),
  
  PlayStartStatus = rtmp_socket:prepare_status(StreamId, <<"NetStream.Play.Start">>),
  rtmp_socket:send(RTMP, PlayStartStatus#rtmp_message{timestamp = DTS, channel_id = channel_id(metadata, StreamId), ts_type = delta}),
  
  rtmp_socket:send(RTMP, #rtmp_message{type = metadata, channel_id = channel_id(metadata, StreamId), stream_id = StreamId,
    body = [<<"|RtmpSampleAccess">>, true, true], timestamp = DTS, ts_type = delta}),

  
  % rtmp_socket:send(RTMP, #rtmp_message{type = metadata, channel_id = channel_id(audio, StreamId), stream_id = StreamId,
  %   timestamp = same, body = [<<"|RtmpSampleAccess">>, true, true]}),
    
  % DataNotify = rtmp_socket:prepare_notify(StreamId, ),
  rtmp_socket:send(RTMP, #rtmp_message{type = metadata, timestamp = DTS, channel_id = channel_id(metadata, StreamId), ts_type = new,
                                       body = [<<"onStatus">>, {object, [{code, <<"NetStream.Data.Start">>}]}]}),
  ok.

seek_failed(RTMP, StreamId) ->
  rtmp_socket:status(RTMP, StreamId, <<"NetStream.Seek.InvalidTime">>),
  ok.
  

play_complete(RTMP, StreamId, Options) ->

  Duration = case proplists:get_value(duration, Options) of
    undefined -> 0;
    Else -> Else
  end,

  % rtmp_socket:send(RTMP, #rtmp_message{type = audio, body = <<>>, ts_type = new, stream_id = StreamId,
  %   timestamp = Duration, channel_id = channel_id(audio, StreamId)}),
  rtmp_socket:send(RTMP, empty_audio(StreamId, Duration)),
  rtmp_socket:send(RTMP, #rtmp_message{type = stream_end, stream_id = StreamId, channel_id = 2, timestamp = 0, ts_type = new}),

  PlayComplete1Arg = {object, [{level, <<"status">>}, {code, <<"NetStream.Play.Complete">>}, {duration, Duration/1000},{bytes,0}]},
  PlayComplete1 = #rtmp_message{type = metadata, channel_id = channel_id(metadata, StreamId), stream_id = StreamId, 
                body = [<<"onPlayStatus">>, PlayComplete1Arg], timestamp = Duration, ts_type = new},
  rtmp_socket:send(RTMP, PlayComplete1),



  % PlayCompleteArg = {object, [{level, <<"status">>}, {code, <<"NetStream.Play.Complete">>}, {duration, Duration/1000},{bytes,0}]},
  % PlayComplete = #rtmp_message{type = metadata, channel_id = channel_id(metadata, StreamId), stream_id = StreamId, 
  %               body = [<<"onMetaData">>, PlayCompleteArg], timestamp = same},
  % rtmp_socket:send(RTMP, PlayComplete),
  

  
  rtmp_socket:send(RTMP, #rtmp_message{type = stream_end, stream_id = StreamId, channel_id = 2, timestamp = 0, ts_type = new}),

  

  PlayStopArg = {object, [{level, <<"status">>}, {code, <<"NetStream.Play.Stop">>},{description,<<"file end">>}]},
  PlayStop = #rtmp_message{type = invoke, channel_id = channel_id(metadata, StreamId), timestamp = Duration, ts_type = new, stream_id = StreamId, 
                body = #rtmp_funcall{command = onStatus, id = 0, stream_id = StreamId, args = [null, PlayStopArg]}},
  rtmp_socket:send(RTMP, PlayStop),
  % rtmp_socket:status(RTMP, StreamId, <<"NetStream.Play.Stop">>).
  ok.


play_failed(RTMP, StreamId) ->
  PlayComplete1Arg = {object, [{level, <<"status">>}, {code, <<"NetStream.Play.Failed">>}]},
  PlayComplete1 = #rtmp_message{type = metadata, channel_id = channel_id(video, StreamId), stream_id = StreamId, 
                body = [<<"onPlayStatus">>, PlayComplete1Arg], timestamp = same},
  rtmp_socket:send(RTMP, PlayComplete1),



  PlayCompleteArg = {object, [{level, <<"status">>}, {code, <<"NetStream.Play.Failed">>}]},
  PlayComplete = #rtmp_message{type = metadata, channel_id = channel_id(video, StreamId), stream_id = StreamId, 
                body = [<<"onMetaData">>, PlayCompleteArg], timestamp = same},
  rtmp_socket:send(RTMP, PlayComplete),


  rtmp_socket:send(RTMP, #rtmp_message{type = stream_end, stream_id = StreamId, channel_id = 2, timestamp = 0}),

  PlayStopArg = {object, [{level, <<"status">>}, {code, <<"NetStream.Play.Failed">>},{description,"file end"}]},
  PlayStop = #rtmp_message{type = invoke, channel_id = channel_id(video, StreamId), timestamp = 0, stream_id = StreamId, 
                body = #rtmp_funcall{command = onStatus, id = 0, stream_id = StreamId, args = [null, PlayStopArg]}},
  rtmp_socket:send(RTMP, PlayStop),
  % rtmp_socket:status(RTMP, StreamId, <<"NetStream.Play.Stop">>).
  ok.
  

-spec channel_id(Content::content_type(), StreamId::non_neg_integer()) -> non_neg_integer().
channel_id(metadata, StreamId) -> 4 + StreamId;
channel_id(video, StreamId) -> 6 + StreamId;
channel_id(audio, StreamId) -> 5 + StreamId.


notify_publish_start(RTMP, StreamId, SessionId, Name) when is_list(Name) ->
  notify_publish_start(RTMP, StreamId, SessionId, list_to_binary(Name));

notify_publish_start(RTMP, StreamId, SessionId, Name) ->
  rtmp_socket:send(RTMP, #rtmp_message{type = stream_begin, stream_id = StreamId}),
  Arg = {object, [
    {level, <<"status">>}, 
    {code, <<"NetStream.Publish.Start">>}, 
    {description, <<"Publishing ", Name/binary, ".">>},
    {clientid, SessionId}
  ]},
  Msg = rtmp_socket:prepare_invoke(StreamId, onStatus, [Arg]),
  rtmp_socket:send(RTMP, Msg),
  ok.

% wait_for(Msg) ->
%   receive
%     Msg -> Msg;
%     Else -> io:format("Else: ~p/~p~n", [Msg, Else]), wait_for(Msg)
%   after
%     10000 -> erlang:error(timeout)
%   end.
% 
%     
