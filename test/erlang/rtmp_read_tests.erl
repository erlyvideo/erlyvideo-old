%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2011 Max Lapshin
%%% @doc        test helper module for erlyvideo
%%% 
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlmedia is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlmedia is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlmedia.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtmp_read_tests).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("../../src/log.hrl").

-compile(export_all).


metadata_duration_test_() ->
  {spawn, {setup,
    fun() -> 
      ems_test_helper:set_ticker_timeouts(true),
      % log4erl:change_log_level(error),
      ok
    end,
    fun(_) ->
      ems_test_helper:set_ticker_timeouts(false),
      % log4erl:change_log_level(debug),
      ok
    end,
    [fun() ->
      {ok, _Media} = media_provider:play(default, "rtmp://localhost/rtmp/video.mp4", [{clients_timeout,0}]),
      receive
        stop -> ok
      after
        500 -> ok
      end,
      Frames = ems_test_helper:receive_all_frames(),
      ?assert(length(Frames) > 20),
      ?D({read_frames,length(Frames)}),
      Metadata = [F || #video_frame{content = Content, body = [Command|_]} = F <- Frames, Content == metadata andalso Command == <<"onMetaData">>],
      ?D({meta,length(Metadata)}),
      [#video_frame{content = metadata, body = [<<"onMetaData">>|{object, Body}]}] = Metadata,
      undefined = Body
    end
  ]}}.