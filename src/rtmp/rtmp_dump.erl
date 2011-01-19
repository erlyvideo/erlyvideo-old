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

-export([connect/2, play/2, wait/2, pause/2, resume/2, seek/2, fcpublish/2, publish/2]).

-record(dumper, {
  commands,
  rtmp,
  stream,
  last_dts
}).


run(Filename) when is_list(Filename) ->
  {ok, Commands} = file:consult(Filename),
  run(#dumper{commands = Commands});
  
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
      rtmp_lib:connect(RTMP, [{app, App}, {tcUrl, <<"rtmp://localhost/live/a">>}]),
      Dumper#dumper{rtmp = RTMP}
  after
    Timeout ->
      {error, timeout}
  end.
  
play(#dumper{rtmp = RTMP} = Dumper, [Path]) ->
  Stream = rtmp_lib:createStream(RTMP),
  rtmp_lib:play(RTMP, Stream, Path),
  Dumper#dumper{stream = Stream}.

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

publish(#dumper{rtmp = RTMP} = Dumper, [Path]) ->
  Stream = rtmp_lib:createStream(RTMP),
  rtmp_socket:invoke(RTMP, Stream, publish, [list_to_binary(Path)]),
  Dumper#dumper{stream = Stream}.

  
flush() ->
  receive
    {rtmp, _RTMP, _Message} -> flush()
  after
    0 -> ok
  end.

    