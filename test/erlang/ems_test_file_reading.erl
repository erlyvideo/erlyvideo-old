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
-module(ems_test_file_reading).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").

read_file(Path) ->
  {ok, F} = file:open(ems_test_helper:file_path(Path), [read,binary]),
  Reader = file_media:file_format(Path),
  {ok, Media} = Reader:init({file,F}, []),
  read_all_frames(Media, [], Reader, undefined).

read_all_frames(Media, Frames, Reader, Key) ->
  case Reader:read_frame(Media, Key) of
    #video_frame{next_id = Next} = F -> read_all_frames(Media, [F|Frames], Reader, Next);
    eof -> {ok, Media, lists:reverse(Frames)}
  end.

test_duration(Frames, Nearby) ->
  [#video_frame{dts = DTS}|_] = lists:reverse(Frames),
  true = DTS >= Nearby - 1000 andalso DTS =< Nearby + 1000.

test_file(Id) ->
  [_,Ext|Rest] = lists:reverse(string:tokens(atom_to_list(Id), "_")),
  Path = string:join(lists:reverse(Rest), "_")++"."++Ext,
  {ok, _, Frames} = read_file(Path),
  fun() -> test_duration(Frames, 20000) end.

-define(CHECK(X), X() -> test_file(X), ok).


h264_aac_1_mp4_test() ->
  {ok, Media, Frames} = read_file("h264_aac_1.mp4"),
  test_duration(Frames, 20000),
  io:format("~p~n", [mp4_reader:media_info(Media)]),
  ?assertMatch(#media_info{
    flow_type = file,
    duration = 19989.333333333332,
    video = [
      #stream_info{
        content = video,
        stream_id = 1,
        codec = h264,
        config = <<1,66,192,13,255,225,0,25,103,66,192,13,171,32,40,51,243,224,34,
                   0,0,3,0,2,0,0,3,0,97,30,40,84,144,1,0,4,104,206,60,128>>,
        language = undefined,
        params = #video_params{width = 320, height = 180}
      }
    ],
    audio = [
      #stream_info{
        content = audio,
        stream_id = 2,
        codec = aac,
        config = <<17,144>>,
        bitrate = undefined,
        language = undefined
      }
    ],
    metadata = []
  }, mp4_reader:media_info(Media)).

?CHECK(h264_aac_1_flv_test).
?CHECK(h264_mp3_1_mp4_test).
?CHECK(h264_mp3_1_flv_test).
?CHECK(h264_1_mp4_test).
?CHECK(h264_1_flv_test).
% ?CHECK(flv_aac_1_flv_test).
?CHECK(flv_mp3_1_flv_test).
?CHECK(mp3_1_mp3_test).
  


% h264_1_h264_test() -> ok.

% aac_1_aac_test() -> ok.



