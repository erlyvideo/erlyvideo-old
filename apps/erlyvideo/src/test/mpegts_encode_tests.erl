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
-module(mpegts_encode_tests).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("../../src/log.hrl").

-compile(export_all).


encode_mpegts([], Streamer) ->
  {Streamer1, _} = mpegts:flush(Streamer),
  Streamer1;
  
encode_mpegts([Frame|Frames], Streamer) ->
  {Streamer1, _} = mpegts:encode(Streamer, Frame),
  encode_mpegts(Frames, Streamer1).

h264_aac_stream_test() ->
  {ok, _Media, Frames} = ems_test_helper:read_file("h264_aac_2.flv"),
  encode_mpegts(Frames, mpegts:init()).

h264_aac_interleaved_test() ->
  {ok, _Media, Frames} = ems_test_helper:read_file("h264_aac_2.flv"),
  encode_mpegts(Frames, mpegts:init([{buffered, true},{interleave,20}])).

empty_encode_test() ->
  encode_mpegts([], mpegts:init([{interleave,20}])).

