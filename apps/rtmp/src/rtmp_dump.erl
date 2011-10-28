%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Can parse dump commands to debug rtmp scenarios
%%% Required only for console tool ``contrib/rtmp_dump''
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%% @hidden
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
-module(rtmp_dump).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/flv.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-define(D(X), io:format("~p:~p ~p~n", [?MODULE, ?LINE, X])).

-export([run/1]).

-export([protect/2, connect/2, play/2, wait/2, sleep/2, pause/2, resume/2, seek/2, fcpublish/2, publish/2]).
-export([createStream/2, receiveAudio/2, receiveVideo/2]).

-record(dumper, {
  id = 0,
  commands,
  rtmp,
  worker_pid,
  stream,
  last_dts,
  substreams = []
}).


run(Filename) when is_list(Filename) ->
  {ok, Commands} = file:consult(Filename),
  run(#dumper{commands = Commands});

run(#dumper{commands = [{parallel,N}|Commands]} = Dumper) ->
  run(Dumper#dumper{commands = [{parallel,N,0}|Commands]});


run(#dumper{commands = [{parallel,N,Delay}|Commands]} = Dumper) ->
  ?D({spawn,N,clients}),
  Pids = [begin
  Pid = spawn(fun() ->
    run(Dumper#dumper{id = I, commands = Commands})
  end),
  erlang:monitor(process, Pid),
  timer:sleep(Delay),
  Pid
  end || I <- lists:seq(1,N)],
  wait_for(Pids);
  
run(#dumper{commands = [Command|Commands]} = Dumper) ->
  [Function|Args] = erlang:tuple_to_list(Command),
  ?D(Command),
  case ?MODULE:Function(Dumper#dumper{commands = Commands}, Args) of
    {ok, Dumper1} ->
      run(Dumper1);
    #dumper{} = Dumper1 ->
      run(Dumper1);
    {error, Reason} ->
      ?D({error, Reason})
  end;
  
run(#dumper{commands = []}) ->
  ok.

wait_for([]) -> ok;
wait_for(Pids) ->
  receive
    {'DOWN', _, process, Pid, _Reason} -> wait_for(lists:delete(Pid, Pids))
  end.

protect(#dumper{id = Id} = Dumper, []) ->
  try run(Dumper) of
    Reply -> Reply
  catch
    Class:Error ->
      ?D({respawn,Id,Class,Error, erlang:get_stacktrace()}),
      protect(Dumper, [])
  end.

connect(#dumper{} = Dumper, [URL]) ->
  connect(Dumper, [URL, []]);

connect(#dumper{} = Dumper, [URL, Options]) ->
  Timeout = proplists:get_value(timeout, Options, 5000),
  Debug = proplists:get_value(debug, Options, false),
  
  {_HostPort,FullPath} = http_uri2:extract_path_with_query(URL),
  {match, [App]} = re:run(FullPath, "/([^/]+)", [{capture,all_but_first,binary}]),
  
  {ok, RTMP} = rtmp_socket:connect(URL),
  receive 
    {rtmp, RTMP, connected} ->
      rtmp_socket:setopts(RTMP, [{active, true},{debug,Debug}]),
      rtmp_lib:connect(RTMP, Options ++ [{app, App}, {tcUrl, list_to_binary(URL)}]),
      Dumper#dumper{rtmp = RTMP}
  after
    Timeout ->
      {error, timeout}
  end.
  
play(#dumper{rtmp = RTMP, stream = Stream_} = Dumper, [Path]) ->
  Stream = case Stream_ of
    undefined -> rtmp_lib:createStream(RTMP);
    _ -> Stream_
  end,
  rtmp_lib:play(RTMP, Stream, Path),
  Dumper#dumper{stream = Stream}.

sleep(#dumper{} = Dumper, [Time]) ->
  timer:sleep(Time),
  Dumper.

wait(#dumper{rtmp = RTMP} = Dumper, [AbsTime]) ->
  % rtmp_socket:setopts(RTMP, [{active, once}]),
  receive
    {rtmp, RTMP, #rtmp_message{timestamp = DTS, type = Type}} when DTS >= AbsTime andalso (Type == audio orelse Type == video)->
      ?D({wait_success, AbsTime, DTS}),
      Dumper#dumper{last_dts = DTS};
    {rtmp, RTMP, #rtmp_message{timestamp = DTS}} ->
      wait(Dumper#dumper{last_dts = DTS}, [AbsTime]);
    {rtmp, RTMP, disconnect} -> 
      {error, disconnect}
  end.

createStream(#dumper{rtmp = RTMP} = Dumper, []) ->
  Stream = rtmp_lib:createStream(RTMP),
  Dumper#dumper{stream = Stream}.

receiveAudio(#dumper{rtmp = RTMP, stream = Stream} = Dumper, [Flag]) ->
  rtmp_lib:call(RTMP, Stream, receiveAudio, [Flag]),
  Dumper.

receiveVideo(#dumper{rtmp = RTMP, stream = Stream} = Dumper, [Flag]) ->
  rtmp_lib:call(RTMP, Stream, receiveVideo, [Flag]),
  Dumper.

pause(#dumper{rtmp = RTMP, stream = Stream, last_dts = DTS} = Dumper, []) ->
  rtmp_lib:pause(RTMP, Stream, DTS),
  flush(),
  Dumper.

resume(#dumper{rtmp = RTMP, stream = Stream, last_dts = DTS} = Dumper, []) ->
  rtmp_lib:resume(RTMP, Stream, DTS),
  flush(),
  Dumper.

seek(#dumper{rtmp = RTMP, stream = Stream} = Dumper, [DTS]) ->
  rtmp_lib:seek(RTMP, Stream, DTS),
  flush(),
  Dumper.


fcpublish(#dumper{rtmp = RTMP} = Dumper, [Path]) ->
  #rtmp_message{body = #rtmp_funcall{} = FC1} = Invoke1 = rtmp_socket:prepare_invoke(0, 'releaseStream', [list_to_binary(Path)]),
  rtmp_socket:send(RTMP, Invoke1#rtmp_message{body = FC1#rtmp_funcall{id = 2}}),
  #rtmp_message{body = #rtmp_funcall{} = FC2} = Invoke2 = rtmp_socket:prepare_invoke(0, 'FCPublish', [list_to_binary(Path)]),
  rtmp_socket:send(RTMP, Invoke2#rtmp_message{body = FC2#rtmp_funcall{id = 3}}),
  Dumper.

publish(Dumper, [File, Path]) ->
  publish(Dumper, [File, Path, <<"live">>, sync]);

publish(Dumper, [File, Path, Type]) when Type == sync orelse Type == async ->
  publish(Dumper, [File, Path, <<"live">>, Type]);

publish(Dumper, [File, Path, Command]) when Command == record orelse Command == live ->
  publish(Dumper, [File, Path, atom_to_binary(Command,latin1), sync]);

publish(#dumper{rtmp = RTMP} = Dumper, [File, Path, Command, Type]) ->
  StreamId = rtmp_lib:createStream(RTMP),
  erlyvideo:load_config(),
  ems_vhosts:start(),
  application:start(log4erl),
  {ok, Media} = ems_media:start_link(file_media, [{host,default},{name,File},{clients_timeout,false}]++media_detector:file(default, File, [])),
  Cmd = if
    is_atom(Command) -> atom_to_binary(Command,latin1);
    is_list(Command) -> list_to_binary(Command);
    is_binary(Command) -> Command
  end,
  rtmp_socket:invoke(RTMP, StreamId, publish, [list_to_binary(Path), Cmd]),
  case Type of
    sync ->
      put(filename, File),
      run_publish(Media, RTMP, StreamId, undefined),
      Dumper#dumper{stream = StreamId};
    async ->
      Pid = spawn_link(fun() ->
        put(filename, File),
        run_publish(Media, RTMP, StreamId, undefined)
      end),
      Dumper#dumper{stream = StreamId, worker_pid = Pid}
  end.

run_publish(Media, RTMP, StreamId, Key) ->
  case ems_media:read_frame(Media, Key) of
    eof -> ?D({file_published, get(filename)});
    #video_frame{next_id = Next} = Frame ->
      rtmp_session:send_rtmp_frame(RTMP, Frame#video_frame{stream_id = StreamId}),
      run_publish(Media, RTMP, StreamId, Next)
  end.
  
flush() ->
  receive
    {rtmp, _RTMP, _Message} -> flush()
  after
    0 -> ok
  end.

    