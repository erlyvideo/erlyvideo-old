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
-module(rtmp_publish_tests).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("../../src/log.hrl").

-compile(export_all).


publish_file(Path, URL) ->
  rtmp_publish:publish(ems_test_helper:file_path(Path), URL, [{no_timeout, true}]).

check_publish(Name) ->
  log4erl:change_log_level(error),
  {ok, Media} = media_provider:play(default, "testStream", [{type,live},{source_timeout,0},{clients_timeout, 0},{stream_id,1}]),
  link(Media),
  publish_file(Name, "rtmp://localhost/testStream"),
  (catch ems_media:stop_stream(Media)),
  log4erl:change_log_level(debug),
  Frames = ems_test_helper:receive_all_frames(),
  ?assert(length(Frames) > 0).

check_publish_(Name) ->
  fun() -> check_publish(Name) end.

h264_aac_1_test() ->
  check_publish("h264_aac_1.flv").

vp6_nelly_3_test() ->
  check_publish("vp6_nelly_3.flv").

publish_test1_() ->
  {spawn, [
    % check_publish_("flv_aac_1.flv")
    % check_publish_("flv_mp3_1.flv"),
    % check_publish_("h264_1.flv"),
    % check_publish_("h264_mp3_1.flv"),
    % check_publish_("vp6_mp3_1.flv"),
    % check_publish_("vp6_mp3_2.flv"),
    % check_publish_("vp6_nelly_1.flv"),
    % check_publish_("vp6_nelly_2.flv"),
    
  ]}.

