%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2011 Max Lapshin
%%% @doc        Erlyvideo video_frame issues
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlmedia.
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
-module(video_frame).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include("../include/video_frame.hrl").
-include("../include/media_info.hrl").
-include("../include/aac.hrl").


-export([config_frame/1, config_frames/1, define_media_info/2, frame_sound/1]).


config_frame(#stream_info{codec = aac, config = Config}) ->
  #video_frame{
   	content = audio,
   	flavor  = config,
		dts     = 0,
		pts     = 0,
		body    = Config,
	  codec	  = aac,
	  sound	  = {stereo, bit16, rate44}
	};

config_frame(#stream_info{codec = h264, config = Config}) ->
  #video_frame{
   	content = video,
   	flavor  = config,
		dts     = 0,
		pts     = 0,
		body    = Config,
	  codec	  = h264
	};

config_frame(_) ->
  undefined.

config_frames(#media_info{audio = A, video = V}) ->
  Frames = [config_frame(S) || S <- A ++ V],
  [F || F <- Frames, F =/= undefined].

frame_sound(#stream_info{content = audio, codec = Codec, params = #audio_params{channels = 1, sample_rate = Rate}}) when Codec == pcma orelse Codec == pcmu ->
  {mono, bit8, Rate};

frame_sound(#stream_info{content = audio, codec = Codec, params = #audio_params{channels = 2, sample_rate = Rate}}) when Codec == pcma orelse Codec == pcmu ->
  {stereo, bit8, Rate};

frame_sound(#stream_info{content = audio, codec = speex}) ->
  {mono, bit16, rate44};

frame_sound(#stream_info{content = audio}) ->
  {stereo, bit16, rate44};

frame_sound(#stream_info{}) ->
  undefined.


stream_count(#media_info{audio = A, video = V}) ->
  Count = fun
    (L) when is_list(L) -> length(L);
    (_) -> 0
  end,
  Count(A) + Count(V).

define_media_info(#media_info{audio = A} = Media, #video_frame{codec = aac, flavor = config, body = Body}) when A == [] orelse A == wait ->
  #aac_config{channel_count = Channels, sample_rate = Rate} = aac:decode_config(Body),
  Info = #stream_info{
    content = audio,
    codec = aac,
    config = Body,
    params = #audio_params{channels = Channels, sample_rate = Rate},
    stream_id = stream_count(Media) + 1
  },
  Media#media_info{audio = [Info]};

define_media_info(#media_info{} = Media, #video_frame{codec = aac}) ->
  Media;

define_media_info(#media_info{video = V} = Media, #video_frame{codec = h264, flavor = config, body = Body}) when V == [] orelse V == wait  ->
  Metadata = h264:metadata(Body),
  Info = #stream_info{
    content = video,
    codec = h264,
    config = Body,
    params = #video_params{
      width = proplists:get_value(width, Metadata),
      height = proplists:get_value(height, Metadata)
    },
    stream_id = stream_count(Media) + 1
  },
  Media#media_info{video = [Info]};

define_media_info(#media_info{} = Media, #video_frame{codec = h264}) ->
  Media;


define_media_info(#media_info{video = V} = Media, #video_frame{content = video, codec = Codec, body = Body}) when V == [] orelse V == wait  ->
  {Width, Height} = case flv:video_size(Body, Codec) of
     {ok, {W, H}} -> {W, H};
     {more, _} -> {undefined, undefined};
     {error, unknown} -> {undefined, undefined}
  end,
  Info = #stream_info{
    content = video,
    codec = Codec,
    params = #video_params{width = Width, height = Height}
  },
  Media#media_info{video = [Info]};

define_media_info(#media_info{audio = A} = Media, #video_frame{content = audio, codec = Codec, sound = {Channels, _, Rate}}) when A == [] orelse A == wait  ->
  Info = #stream_info{
    content = audio,
    codec = Codec,
    params = #audio_params{channels = Channels, sample_rate = Rate}
  },
  Media#media_info{audio = [Info]};

define_media_info(Media, _) ->
  Media.
