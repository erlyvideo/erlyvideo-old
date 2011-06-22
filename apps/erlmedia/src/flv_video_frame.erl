%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Module, that glues video_frame and flv
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
-module(flv_video_frame).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/video_frame.hrl").
-include("../include/flv.hrl").
-include("log.hrl").
-include("flv_constants.hrl").

-export([to_tag/1, encode/1, decode/1, decode/2]).
-export([tag_to_video_frame/1]).

-export([is_good_flv/1]).

is_good_flv(#video_frame{content = audio, sound = {_Channels, _Bits, Rate}}) when is_number(Rate) -> false;
is_good_flv(#video_frame{content = audio, codec = Codec})
  when Codec == pcm orelse Codec == aac orelse Codec == mp3 orelse Codec == speex orelse Codec == nellymoser orelse Codec == nellymoser8 -> true;
is_good_flv(#video_frame{content = video, codec = Codec})
  when Codec == h264 orelse Codec == vp6 orelse Codec == vp6_alpha orelse Codec == sorenson orelse Codec == mjpeg
  orelse Codec == screen orelse Codec == screen2 -> true;
is_good_flv(#video_frame{content = metadata}) -> true;
is_good_flv(#video_frame{}) -> false.





encode(#video_frame{codec = empty}) ->
  <<>>;

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

decode(<<FlvHeader:?FLV_TAG_HEADER_LENGTH/binary, Bin/binary>>) ->
  Tag = flv:tag_header(FlvHeader),
  #flv_tag{type = Content, timestamp = DTS, size = Size} = Tag,
  <<Body:Size/binary, _/binary>> = Bin,
  decode(#video_frame{content = Content, dts = DTS, pts = DTS}, Body).

decode(#video_frame{content = video, dts = DTS} = Frame, Data) ->
  #flv_video_tag{codec = Codec, flavor = Flavor, composition_time = CTime, body = Body} = flv:decode_video_tag(Data),
  Frame#video_frame{flavor = Flavor, codec = Codec, pts = DTS + CTime, body = Body};


decode(#video_frame{content = audio} = Frame, <<>>) ->
  Frame#video_frame{codec = empty, flavor = frame, body = <<>>};

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
