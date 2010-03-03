%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon, 2009 Max Lapshin
%%% @doc        RTMP encoding/decoding and command handling module
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Luke Hubbard, Stuart Jackson, Roberto Saccon, 2009 Max Lapshin
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
-module(ems_flv).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/ems.hrl").
-include_lib("flv/include/flv.hrl").
-include_lib("flv/include/flv_constants.hrl").
-include_lib("erlyvideo/include/video_frame.hrl").

-export([to_tag/1, encode/1, decode/2]).





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
  #video_frame{type = metadata, body = Metadata}.


decode(#video_frame{type = video, dts = DTS} = Frame, Data) ->
  #flv_video_tag{codec = Codec, frame_type = FrameType, composition_time = CTime, decoder_config = Cfg, body = Body} = flv:decode_video_tag(Data),
  Frame#video_frame{frame_type = FrameType, codec_id = Codec, decoder_config = Cfg, pts = DTS + CTime, body= Body};


decode(#video_frame{type = audio} = Frame, Data) ->
  #flv_audio_tag{codec = Codec, rate = Rate, bitsize = Bitsize, channels = Channels, decoder_config = Cfg, body = Body} = flv:decode_audio_tag(Data),
  Frame#video_frame{codec_id = Codec, sound_type = Channels, sound_size = Bitsize, sound_rate = Rate, body= Body, decoder_config = Cfg};

decode(#video_frame{type = metadata} = Frame, Metadata) ->
  Frame#video_frame{body = flv:decode_meta_tag(Metadata)}.
              

to_tag(#video_frame{type = Type, stream_id = _RealStreamId, dts = DTS} = Frame) ->
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
  ?assertMatch(<<_/binary>>, ems_flv:encode({video_frame,false,video,1664.2277777772397,
                                             1664.2277777772397,1,avc,keyframe,
                                             undefined,undefined,undefined,
                                             <<0,0,4,112,37,184,32,33,241,158,155,37,243>>})).
