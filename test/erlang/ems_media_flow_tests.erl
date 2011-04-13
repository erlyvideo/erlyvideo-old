%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2010 Max Lapshin
%%% @doc        Client of erlyvideo license server
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
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
-module(ems_media_flow_tests).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("eunit/include/eunit.hrl").
-include("../../include/ems_media.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").

-export([read_flow/1]).

read_flow(FlowName) ->
  Path = io_lib:format("test/fixtures/~s_flow.txt", [FlowName]),
  {ok, Bin} = file:read_file(lists:flatten(Path)),
  Text = <<"-module(ems_flow_temp). -include_lib(\"erlmedia/include/video_frame.hrl\"). -export([flow/0]). flow() -> [",
  Bin/binary, "].">>,
  file:write_file("/tmp/ems_flow_temp.erl", Text),
  {ok, ModName, Code} = compile:file("/tmp/ems_flow_temp.erl", [verbose,report_errors,report_warnings,binary]),
  file:delete("/tmp/ems_flow_temp.erl"),
  code:soft_purge(ModName),
  code:load_binary(ModName, "ems_flow_temp.erl", Code),
  Flow = ems_flow_temp:flow(),
  code:soft_purge(ModName),
  code:purge(ModName),
  Flow.


prepare_test_flow(Options, Fun) ->
  {spawn, {setup,
  fun() ->
    % log4erl:change_log_level(error),
    {ok, _Pid} = ems_media:start_link(test_media, Options)
  end,
  fun({ok, Pid}) ->
    Pid ! stop,
    ems_test_helper:wait4(Pid),
    log4erl:change_log_level(debug)
  end,
  fun({ok, Pid}) ->
    [fun() -> Fun(Pid) end]
  end
  }}.


feed_with_flow(Media, FlowName) ->
  Frames = read_flow(FlowName),
  [Media ! Frame || Frame <- Frames],
  ok.


media_info_autofill_test() ->
  prepare_test_flow([], fun(Media) ->
    feed_with_flow(Media, av_header),
    ?assertMatch(#media_info{audio = [#stream_info{codec = aac}], video = [#stream_info{codec = h264}]}, ems_media:media_info(Media))
  end).

av_test_() ->
  prepare_test_flow([{send_audio_before_keyframe,false}], fun(Media) ->
    feed_with_flow(Media, av_header),
    ?assertMatch(#media_info{video = [#stream_info{codec = h264}]}, ems_media:media_info(Media)),
    Master = self(),
    Reader = spawn_link(fun() ->
      ems_media:play(Media, [{stream_id,1}]),
      Master ! {ready, self()},
      Master ! {frames, ems_test_helper:receive_all_frames(100)}
    end),
    receive
      {ready, Reader} -> ok
    end,
    feed_with_flow(Media, audio_video),
    ?assertMatch(#media_info{video = [#stream_info{codec = h264}]}, ems_media:media_info(Media)),
    timer:sleep(100),
    Frames = receive
      {frames, Frames_} -> Frames_
    after
      500 -> ?assertNot(false)  
    end,
    % io:format("~p~n", [[{C, Fl, DTS, StrId} || #video_frame{content = C, flavor = Fl, dts = DTS, stream_id = StrId} <- Frames]]),
    ?assertEqual(6, length(Frames)),
    ?assert(ems_test_helper:has_small_delta(Frames, 100))
  end).