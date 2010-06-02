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
-include("../include/video_frame.hrl").
-include("../include/flv.hrl").
-include("log.hrl").

-export([to_tag/1, encode/1, decode/2]).
-export([tag_to_video_frame/1]).





encode(#video_frame{content = audio} = VideoFrame) ->
  flv:encode_audio_tag(video_frame_to_tag(VideoFrame));

encode(#video_frame{content = video} = VideoFrame) ->
  flv:encode_video_tag(video_frame_to_tag(VideoFrame));

encode(#video_frame{content = metadata} = VideoFrame) ->
  flv:encode_meta_tag(video_frame_to_tag(VideoFrame)).

video_frame_to_tag(#video_frame{content = audio,
                    flavor = Flavor,
                    codec = Codec,
                	  sound	= {SoundType, SoundSize, SoundRate},
                    body = Body}) when is_binary(Body) ->
  #flv_audio_tag{codec = Codec, rate = SoundRate, bitsize = SoundSize, channels = SoundType, body = Body, flavor = Flavor};



video_frame_to_tag(#video_frame{content = video,
                    flavor = Flavor,
                   	codec = Codec,
                   	pts = PTS,
                   	dts = DTS,
                    body = Body}) when is_binary(Body) ->
  #flv_video_tag{codec = Codec, flavor = Flavor, composition_time = PTS - DTS, body = Body};


video_frame_to_tag(#video_frame{content = metadata, body = Metadata}) ->
  Metadata.

tag_to_video_frame(#flv_tag{next_tag_offset = NextOffset, timestamp = Timestamp, body = AVTag}) ->
	VideoFrame = tag_to_video_frame(AVTag, Timestamp),
	VideoFrame#video_frame{next_id = NextOffset};


tag_to_video_frame(#flv_audio_tag{codec = Codec, rate = Rate, bitsize = Size, channels = Channels, body = Body, flavor = Flavor}) ->
  #video_frame{content = audio,
               flavor = Flavor,
               codec = Codec,
               sound = {Channels, Size, Rate},
               pts = 0,
               dts = 0,
               body = Body};


tag_to_video_frame(#flv_video_tag{codec = Codec, flavor = Flavor, composition_time = CTime, body = Body}) ->
  #video_frame{content = video,
               flavor = Flavor,
               codec = Codec,
               pts = CTime,
               dts = 0,
               body = Body};


tag_to_video_frame(Metadata) ->
  #video_frame{content = metadata, body = Metadata, dts = 0, pts = 0}.
  
-spec(tag_to_video_frame(Tag::flv_specific_tag(), Timestamp::number()) -> video_frame()).
tag_to_video_frame(Tag, Timestamp) ->
  Frame = tag_to_video_frame(Tag),
  CTime = Frame#video_frame.pts,
  Frame#video_frame{dts = Timestamp, pts = Timestamp+CTime}.
  

decode(#video_frame{content = video, dts = DTS} = Frame, Data) ->
  #flv_video_tag{codec = Codec, flavor = Flavor, composition_time = CTime, body = Body} = flv:decode_video_tag(Data),
  Frame#video_frame{flavor = Flavor, codec = Codec, pts = DTS + CTime, body = Body};


decode(#video_frame{content = audio} = Frame, Data) ->
  #flv_audio_tag{codec = Codec, rate = Rate, bitsize = Bitsize, channels = Channels, flavor = Flavor, body = Body} = flv:decode_audio_tag(Data),
  Frame#video_frame{codec = Codec, sound = {Channels, Bitsize, Rate}, body= Body, flavor = Flavor};

decode(#video_frame{content = metadata} = Frame, Metadata) ->
  Frame#video_frame{body = flv:decode_meta_tag(Metadata)}.
              
%%--------------------------------------------------------------------
%% @spec (Frame::video_frame()) -> FLVTag::binary()
%% @doc Dumps video frame to tag
%% @end 
%%--------------------------------------------------------------------

to_tag(#video_frame{content = Content, stream_id = _RealStreamId, dts = DTS1} = Frame) ->
  DTS = round(DTS1),
  StreamId = 0, % by spec
  Body = encode(Frame),
	BodyLength = size(Body),
	<<TimeStampExt:8,TimeStamp:24>> = <<DTS:32>>,
	PrevTagSize = BodyLength + 11,
	<<(flv:frame_format(Content)):8,BodyLength:24,TimeStamp:24,TimeStampExt:8,StreamId:24,Body/binary,PrevTagSize:32>>.


-include_lib("eunit/include/eunit.hrl").

encode_video_test() ->
  ?assertMatch(<<_/binary>>, encode({video_frame,video,1664.2277777772397,1664.2277777772397,1,h264,config,
                                    {undefined,undefined,undefined},<<0,0,4,112,37,184,32,33,241,158,155,37,243>>,undefined})).
