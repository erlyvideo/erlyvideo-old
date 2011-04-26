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
-include("../../src/log.hrl").
-export([read_file/1]).


read_file(Path) ->
  ems_test_helper:read_file(Path).
  

test_duration(Frames, Nearby) ->
  [#video_frame{dts = FDTS}|_] = Frames,
  [#video_frame{dts = LDTS}|_] = lists:reverse(Frames),
  TDTS = LDTS - FDTS, 
  %?D({"Delta DTS ",TDTS}),
  true = TDTS >= Nearby - 1000 andalso TDTS =< Nearby + 1000.

test_file(Id) ->
  [_,Ext|Rest] = lists:reverse(string:tokens(atom_to_list(Id), "_")),
  Path = string:join(lists:reverse(Rest), "_")++"."++Ext,
  Reader = file_media:file_format(Path),
  {ok, Media, Frames} = read_file(Path),
  test_duration(Frames, 20000),
  ?assertMatch(#media_info{flow_type = file}, Reader:media_info(Media)).

-define(CHECK(X), X() -> test_file(X), ok).


h264_aac_1_mp4_test() ->
  {ok, Media, Frames} = read_file("h264_aac_1.mp4"),
  test_duration(Frames, 20000),
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

%mpegts_file_reader_test() ->
%  Pid = spawn_link(?MODULE, mpegts_file_reader,[self()]).
%  St = erlang:monitor(process,Pid),
%  receive 
%    #video_frame{} = F -> ?D("asd")
%   {'DOWN', _Ref, process, Pid, _Reason} -> ?D("Crash")
%  end.

%mpegts_file_reader_test() ->
% Reader = spawn_link(?MODULE,mpegts_read_frame,[[],self()]),  
%  {ok,Cons}=mpegts_sup:start_file_reader("/home/tthread/a.ts",[{consumer,self()},{no_delay,true}]),
%  erlang:monitor(process,Cons),
%  {ok,Frames} = mpegts_read_frame([],self()),
%  [First|Tail] = Frames, [Last|LTail] = lists:reverse(Frames),
%  Nur = Last#video_frame.dts - First#video_frame.dts,
%  ?D({"DTS",Nur,Last#video_frame.dts,First#video_frame.dts}).
%  test_duration(Frames,20000).

  

mpegts_reader_file_test_() ->
  {spawn, {setup,
    fun() ->
      ems_test_helper:set_ticker_timeouts(true),
      ems_network_lag_monitor:set_threshold(10000000),
      log4erl:change_log_level(error),
      ok
    end,
    fun(_) ->
      ems_test_helper:set_ticker_timeouts(false),
      log4erl:change_log_level(debug),
      ok
    end,
    [fun() ->
      {ok,Pid} = mpegts:read("http://127.0.0.1:8082/stream/video.mp4?duration=60",[]),
      erlang:monitor(process, Pid),
      Frames = ems_test_helper:receive_all_frames(500),
      ?assert(length(Frames) > 100),
      ?assertNot(ems_test_helper:is_interleaved(Frames,15)),
      ?assert(ems_test_helper:is_monotonic(Frames))
    end]
  }}.


mpegts_reader_iphone_test_() ->
  {spawn, {setup,
    fun() ->
      ems_test_helper:set_ticker_timeouts(true),
      log4erl:change_log_level(error),
      ok
    end,
    fun(_) ->
      ems_test_helper:set_ticker_timeouts(false),
      log4erl:change_log_level(debug),
      ok
    end,
    [fun() ->
      {ok,Pid} = mpegts:read("http://127.0.0.1:8082/iphone/segments/video.mp4/3.ts",[]),
      erlang:monitor(process, Pid),
      Frames = ems_test_helper:receive_all_frames(100),
      ?assert(length(Frames) > 100),
      ?assert(ems_test_helper:is_interleaved(Frames,10)),
      ?assertNot(ems_test_helper:is_monotonic(Frames))
    end]
  }}.

%mpegts_read_frame(Frames,Pid) ->
%  erlang:monitor(process,Pid),
%  link(Pid),
%  receive 
%   #video_frame{} = F ->
%         ?D({F#video_frame.flavor, F#video_frame.content, round(F#video_frame.dts - 31600000)}),
%         mpegts_read_frame([F|Frames],Pid);
%  Else -> 
%    ?D({stop,Pid,Else}),
%    {ok,lists:reverse(Frames)}
%  end. 

  

h264_aac_2_mp4_test() ->
  {ok, Media, Frames} = read_file("h264_aac_2.mp4"),
  test_duration(Frames, 20000),
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
        language = <<"eng">>
      },
      #stream_info{
        content = audio,
        stream_id = 3,
        codec = aac,
        config = <<17,144>>,
        bitrate = undefined,
        language = <<"ger">>
      }
    ],
    metadata = [
      #stream_info{content = text, stream_id = 4, codec = srt, language = <<"eng">>},
      #stream_info{content = text, stream_id = 5, codec = srt, language = <<"rus">>}
    ]
  }, mp4_reader:media_info(Media)).


?CHECK(h264_aac_1_flv_test).


h264_mp3_1_mp4_test() ->
  {ok, Media, Frames} = read_file("h264_mp3_1.mp4"),
  test_duration(Frames, 20000),
  ?assertMatch(#media_info{
    flow_type = file,
    duration = 20016.0,
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
        codec = mp3,
        config = undefined,
        bitrate = undefined,
        language = undefined
      }
    ],
    metadata = []
  }, mp4_reader:media_info(Media)).
  




?CHECK(h264_mp3_1_flv_test).


h264_1_mp4_test() ->
  {ok, Media, Frames} = read_file("h264_1.mp4"),
  test_duration(Frames, 20000),
  ?assertMatch(#media_info{
    flow_type = file,
    duration = 19958.333333333332,
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
    ],
    metadata = []
  }, mp4_reader:media_info(Media)).
  

h264_1_flv_test() ->
  {ok, Media, Frames} = read_file("h264_1.flv"),
  test_duration(Frames, 20000),
  ?assertMatch(#media_info{
    flow_type = file,
    duration = 20000.0,
    video = [
      #stream_info{
        content = video,
        stream_id = 1,
        codec = h264,
        config = <<1,66,192,13,255,225,0,25,103,66,192,13,171,32,40,51,243,224,34,
                   0,0,3,0,2,0,0,3,0,97,30,40,84,144,1,0,4,104,206,60,128>>,
        params = #video_params{width = 320, height = 180}
      }
    ],
    audio = [
    ],
    metadata = []
  }, flv_reader:media_info(Media)).
  
% ?CHECK(flv_aac_1_flv_test).
flv_mp3_1_flv_test() ->
  {ok, Media, Frames} = read_file("flv_mp3_1.flv"),
  test_duration(Frames, 20000),
  ?assertMatch(#media_info{
    flow_type = file,
    duration = 20062.0,
    video = [
      #stream_info{
        content = video,
        stream_id = 1,
        codec = sorensen,
        config = undefined,
        params = #video_params{width = 320, height = 180}
      }
    ],
    audio = [
      #stream_info{
        content = audio,
        stream_id = 2,
        codec = mp3,
        config = undefined,
        params = #audio_params{}
      }
    ],
    metadata = []
  }, flv_reader:media_info(Media)).




mp3_1_mp3_test() ->
  {ok, Media, Frames} = read_file("mp3_1.mp3"),
  test_duration(Frames, 10000),
  ?assertMatch(#media_info{
    flow_type = file,
    duration = 10083,
    video = [],
    audio = [
      #stream_info{
        content = audio,
        stream_id = 1,
        codec = mp3,
        config = undefined,
        params = #audio_params{channels = 2, sample_rate = 44100}
      }
    ],
    metadata = []
  }, mp3_reader:media_info(Media)).
  
  


% h264_1_h264_test() -> ok.

% aac_1_aac_test() -> ok.



