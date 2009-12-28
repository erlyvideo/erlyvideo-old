-module(ems_rtsp).
-author(max@maxidoors.ru).

-include("../include/h264.hrl").
-include("../include/video_frame.hrl").
-include("../include/ems.hrl").
-include("../lib/ertsp/include/rtsp.hrl").

-export([announce/4]).

announce(Host, Path, Streams, _Headers) -> 
  Media = media_provider:open(Host, Path, live),
  Streams1 = config_media(Media, Streams),
  {ok, Media, Streams1}.


config_media(Media, Streams) -> config_media(Media, Streams, []).

config_media(_Media, [], Output) -> Output;
config_media(Media, [#rtsp_stream{type = video, pps = PPS, sps = SPS} = Stream | Streams], Output) ->
  {H264, _} = h264:decode_nal(SPS, #h264{}),
  {H264_2, Configs} = h264:decode_nal(PPS, H264),
  lists:foreach(fun(Frame) ->
    Media ! Frame#video_frame{timestamp = 0}
  end, Configs),
  config_media(Media, Streams, [Stream#rtsp_stream{config = H264_2} | Output]);

config_media(Media, [#rtsp_stream{type = audio, config = Config} = Stream | Streams], Output) ->
  AudioConfig = #video_frame{       
   	type          = audio,
   	decoder_config = true,
		timestamp      = 0,
		body          = Config,
	  sound_format	= aac,
	  sound_type	  = stereo,
	  sound_size	  = bit16,
	  sound_rate	  = rate44
	},
  Media ! AudioConfig,

  config_media(Media, Streams, [Stream | Output]).
