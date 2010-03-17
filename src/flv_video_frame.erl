%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Module, that glues video_frame and flv
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(flv_video_frame).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/flv.hrl").
-include_lib("erlmedia/include/video_frame.hrl").

-export([to_tag/1, encode/1, decode/2]).
-export([tag_to_video_frame/1]).





encode(#video_frame{type = audio} = VideoFrame) ->
  flv:encode_audio_tag(video_frame_to_tag(VideoFrame));

encode(#video_frame{type = video} = VideoFrame) ->
  flv:encode_video_tag(video_frame_to_tag(VideoFrame));

encode(#video_frame{type = metadata} = VideoFrame) ->
  flv:encode_meta_tag(video_frame_to_tag(VideoFrame)).

video_frame_to_tag(#video_frame{type = audio,
                    decoder_config = DecoderConfig,
                    codec_id = Codec,
                	  sound_type	= SoundType,
                	  sound_size	= SoundSize,
                	  sound_rate	= SoundRate,
                    body = Body}) when is_binary(Body) ->
  #flv_audio_tag{codec = Codec, rate = SoundRate, bitsize = SoundSize, channels = SoundType, body = Body, decoder_config = DecoderConfig};



video_frame_to_tag(#video_frame{type = video,
                    frame_type = FrameType,
                   	decoder_config = DecoderConfig,
                   	codec_id = Codec,
                   	pts = PTS,
                   	dts = DTS,
                    body = Body}) when is_binary(Body) ->
  #flv_video_tag{codec = Codec, decoder_config = DecoderConfig, frame_type = FrameType, composition_time = PTS - DTS, body = Body};


video_frame_to_tag(#video_frame{type = metadata, body = Metadata}) ->
  Metadata.

tag_to_video_frame(#flv_audio_tag{codec = Codec, rate = Rate, bitsize = Size, channels = SoundType, body = Body, decoder_config = DecoderConfig}) ->
  #video_frame{type = audio,
               decoder_config = DecoderConfig,
               codec_id = Codec,
               sound_type	= SoundType,
               sound_size	= Size,
               sound_rate	= Rate,
               pts = 0,
               dts = 0,
               body = Body};


tag_to_video_frame(#flv_video_tag{codec = Codec, decoder_config = DecoderConfig, frame_type = FrameType, composition_time = CTime, body = Body}) ->
  #video_frame{type = video,
               frame_type = FrameType,
               decoder_config = DecoderConfig,
               codec_id = Codec,
               pts = CTime,
               dts = 0,
               body = Body};


tag_to_video_frame(Metadata) ->
  #video_frame{type = metadata, body = Metadata, dts = 0, pts = 0}.


decode(#video_frame{type = video, dts = DTS} = Frame, Data) ->
  #flv_video_tag{codec = Codec, frame_type = FrameType, composition_time = CTime, decoder_config = Cfg, body = Body} = flv:decode_video_tag(Data),
  Frame#video_frame{frame_type = FrameType, codec_id = Codec, decoder_config = Cfg, pts = DTS + CTime, body= Body};


decode(#video_frame{type = audio} = Frame, Data) ->
  #flv_audio_tag{codec = Codec, rate = Rate, bitsize = Bitsize, channels = Channels, decoder_config = Cfg, body = Body} = flv:decode_audio_tag(Data),
  Frame#video_frame{codec_id = Codec, sound_type = Channels, sound_size = Bitsize, sound_rate = Rate, body= Body, decoder_config = Cfg};

decode(#video_frame{type = metadata} = Frame, Metadata) ->
  Frame#video_frame{body = flv:decode_meta_tag(Metadata)}.
              
%%--------------------------------------------------------------------
%% @spec (Frame::video_frame()) -> FLVTag::binary()
%% @doc Dumps video frame to tag
%% @end 
%%--------------------------------------------------------------------

to_tag(#video_frame{type = Type, stream_id = _RealStreamId, dts = DTS1} = Frame) ->
  DTS = round(DTS1),
  StreamId = 0, % by spec
  Body = encode(Frame),
	BodyLength = size(Body),
	{TimeStampExt, TimeStamp} = case <<DTS:32>> of
		<<TimeStampExt1:8,TimeStamp1:24>> -> 
			{TimeStampExt1, TimeStamp1};
		_ ->
			{0, DTS}
	end,
	PrevTagSize = BodyLength + 11,
	<<(flv:frame_format(Type)):8,BodyLength:24,TimeStamp:24,TimeStampExt:8,StreamId:24,Body/binary,PrevTagSize:32>>.


-include_lib("eunit/include/eunit.hrl").

encode_video_test() ->
  ?assertMatch(<<_/binary>>, encode({video_frame,false,video,1664.2277777772397,
                                             1664.2277777772397,1,avc,keyframe,
                                             undefined,undefined,undefined,
                                             <<0,0,4,112,37,184,32,33,241,158,155,37,243>>})).
