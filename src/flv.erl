%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon, 2009 Max Lapshin
%%% @doc        RTMP encoding/decoding and command handling module
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org</a> for more information
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
-module(flv).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/flv.hrl").
-include("flv_constants.hrl").

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).


-export([audio_codec/1, audio_type/1, audio_size/1, audio_rate/1, video_codec/1, frame_type/1, frame_format/1]).

-export([header/0, header/1, read_header/1, read_tag_header/2, read_tag/2, data_offset/0]).
-export([getWidthHeight/3, extractVideoHeader/2, decodeScreenVideo/2, decodeSorensen/2, decodeVP6/2, extractAudioHeader/2]).

-export([encode_audio_tag/1, encode_video_tag/1, encode_meta_tag/1, encode_tag/1,
         decode_audio_tag/1, decode_video_tag/1, decode_meta_tag/1, decode_tag/1]).


frame_format(audio) -> ?FLV_TAG_TYPE_AUDIO;
frame_format(video) -> ?FLV_TAG_TYPE_VIDEO;
frame_format(metadata) -> ?FLV_TAG_TYPE_META;
frame_format(?FLV_TAG_TYPE_AUDIO) -> audio;
frame_format(?FLV_TAG_TYPE_VIDEO) -> video;
frame_format(?FLV_TAG_TYPE_META) -> metadata.

audio_codec(adpcm) -> ?FLV_AUDIO_FORMAT_ADPCM;
audio_codec(aac) -> ?FLV_AUDIO_FORMAT_AAC;
audio_codec(speex) -> ?FLV_AUDIO_FORMAT_SPEEX;
audio_codec(mp3) -> ?FLV_AUDIO_FORMAT_MP3;
audio_codec(nelly_moser) -> ?FLV_AUDIO_FORMAT_NELLYMOSER;
audio_codec(?FLV_AUDIO_FORMAT_ADPCM) -> adpcm;
audio_codec(?FLV_AUDIO_FORMAT_MP3) -> mp3;
audio_codec(?FLV_AUDIO_FORMAT_NELLYMOSER) -> nelly_moser;
audio_codec(?FLV_AUDIO_FORMAT_SPEEX) -> speex;
audio_codec(?FLV_AUDIO_FORMAT_AAC) -> aac.


audio_type(mono) -> ?FLV_AUDIO_TYPE_MONO;
audio_type(stereo) -> ?FLV_AUDIO_TYPE_STEREO;
audio_type(?FLV_AUDIO_TYPE_MONO) -> mono;
audio_type(?FLV_AUDIO_TYPE_STEREO) -> stereo.

audio_size(bit8) -> ?FLV_AUDIO_SIZE_8BIT;
audio_size(bit16) -> ?FLV_AUDIO_SIZE_16BIT;
audio_size(?FLV_AUDIO_SIZE_8BIT) -> bit8;
audio_size(?FLV_AUDIO_SIZE_16BIT) -> bit16.

audio_rate(?FLV_AUDIO_RATE_5_5) -> rate5;
audio_rate(?FLV_AUDIO_RATE_11) -> rate11;
audio_rate(?FLV_AUDIO_RATE_22) -> rate22;
audio_rate(?FLV_AUDIO_RATE_44) -> rate44;
audio_rate(rate5) -> ?FLV_AUDIO_RATE_5_5;
audio_rate(rate11) -> ?FLV_AUDIO_RATE_11;
audio_rate(rate22) -> ?FLV_AUDIO_RATE_22;
audio_rate(rate44) -> ?FLV_AUDIO_RATE_44.

video_codec(avc) -> ?FLV_VIDEO_CODEC_AVC;
video_codec(sorensen) -> ?FLV_VIDEO_CODEC_SORENSEN;
video_codec(vp6) -> ?FLV_VIDEO_CODEC_ON2VP6;
video_codec(?FLV_VIDEO_CODEC_ON2VP6) -> vp6;
video_codec(?FLV_VIDEO_CODEC_SORENSEN) -> sorensen;
video_codec(?FLV_VIDEO_CODEC_AVC) -> avc.

frame_type(frame) -> ?FLV_VIDEO_FRAME_TYPEINTER_FRAME;
frame_type(keyframe) -> ?FLV_VIDEO_FRAME_TYPE_KEYFRAME;
frame_type(disposable) -> ?FLV_VIDEO_FRAME_TYPEDISP_INTER_FRAME;
frame_type(?FLV_VIDEO_FRAME_TYPEDISP_INTER_FRAME) -> disposable;
frame_type(?FLV_VIDEO_FRAME_TYPEINTER_FRAME) -> frame;
frame_type(?FLV_VIDEO_FRAME_TYPE_KEYFRAME) -> keyframe.



data_offset() -> ?FLV_HEADER_LENGTH + ?FLV_PREV_TAG_SIZE_LENGTH.

read_header(Device) ->  % Always on first bytes
  case file:read(Device, ?FLV_HEADER_LENGTH) of
    {ok, Data} ->
      {header(Data), size(Data) + ?FLV_PREV_TAG_SIZE_LENGTH};
    Else ->
      Else
  end.


read_tag_header(Device, Offset) ->
	case file:pread(Device,Offset, ?FLV_TAG_HEADER_LENGTH) of
		{ok, <<Type, Size:24, TimeStamp:24, TimeStampExt, _StreamId:24>>} ->
      % io:format("Frame ~p ~p ~p~n", [Type, TimeStamp, Size]),
		  <<TimeStampAbs:32>> = <<TimeStampExt, TimeStamp:24>>,
      #flv_tag{type = frame_format(Type), timestamp = TimeStampAbs, size = Size, 
               offset = Offset + ?FLV_TAG_HEADER_LENGTH,
               next_tag_offset = Offset + ?FLV_TAG_HEADER_LENGTH + Size + ?FLV_PREV_TAG_SIZE_LENGTH};
    eof -> eof;
    {error, Reason} -> {error, Reason}
  end.

read_tag(Device, Offset) ->
  case read_tag_header(Device, Offset) of
    #flv_tag{type = Type, size = Size} = Tag ->
      {ok, Body} = file:pread(Device, Offset + ?FLV_TAG_HEADER_LENGTH, Size),
      
      FrameType = case Type of
        video ->
          case Body of 
            <<?FLV_VIDEO_FRAME_TYPE_KEYFRAME:4, _CodecID:4, _/binary>> -> keyframe;
            _ -> frame
          end;
        _ -> frame
      end,
      
      decode_tag(Tag#flv_tag{body = Body, frame_type = FrameType});
    Else -> Else
  end.


decode_video_tag(<<FrameType:4, ?FLV_VIDEO_CODEC_AVC:4, ?FLV_VIDEO_AVC_NALU:8, CTime:24, Body/binary>>) ->
  #flv_video_tag{frame_type = flv:frame_type(FrameType), codec = avc, composition_time = CTime, body= Body};

decode_video_tag(<<FrameType:4, ?FLV_VIDEO_CODEC_AVC:4, ?FLV_VIDEO_AVC_SEQUENCE_HEADER:8, CTime:24, Body/binary>>) ->
  #flv_video_tag{frame_type = flv:frame_type(FrameType), codec = avc, decoder_config = true, composition_time = CTime, body= Body};

decode_video_tag(<<FrameType:4, CodecId:4, Body/binary>>) ->
  #flv_video_tag{frame_type = flv:frame_type(FrameType), codec = flv:video_codec(CodecId), composition_time = 0, body = Body}.



decode_audio_tag(<<?FLV_AUDIO_FORMAT_AAC:4, Rate:2, Bitsize:1, Channels:1, ?FLV_AUDIO_AAC_RAW:8, Body/binary>>) ->
  #flv_audio_tag{codec = aac, channels = flv:audio_type(Channels), bitsize = flv:audio_size(Bitsize), 
                 rate	= flv:audio_rate(Rate), body= Body};

decode_audio_tag(<<?FLV_AUDIO_FORMAT_AAC:4, Rate:2, Bitsize:1, Channels:1, ?FLV_AUDIO_AAC_SEQUENCE_HEADER:8, Body/binary>>) ->
  #flv_audio_tag{codec = aac, channels = flv:audio_type(Channels), bitsize = flv:audio_size(Bitsize), 
                 decoder_config = true, rate	= flv:audio_rate(Rate), body= Body};

decode_audio_tag(<<CodecId:4, Rate:2, Bitsize:1, Channels:1, Body/binary>>) ->
  #flv_audio_tag{codec = flv:audio_codec(CodecId), channels = flv:audio_type(Channels), bitsize = flv:audio_size(Bitsize), 
                 rate	= flv:audio_rate(Rate), body= Body}.


decode_meta_tag(Metadata) when is_binary(Metadata) ->
  decode_list(Metadata);

decode_meta_tag(Metadata) ->
  Metadata.

decode_tag(#flv_tag{type = video, body = VideoTag} = Tag) ->
  Tag#flv_tag{body = decode_video_tag(VideoTag)};

decode_tag(#flv_tag{type = audio, body = AudioTag} = Tag) ->
  Tag#flv_tag{body = decode_audio_tag(AudioTag)};

decode_tag(#flv_tag{type = metadata, body = Metadata} = Tag) ->
  Tag#flv_tag{body = decode_meta_tag(Metadata)}.


decode_list(Data) -> decode_list(Data, []).

decode_list(<<>>, Acc) -> lists:reverse(Acc);

decode_list(Body, Acc) ->
  {Element, Rest} = amf0:decode(Body),
  decode_list(Rest, [Element | Acc]).

encode_list(List) -> encode_list(<<>>, List).

encode_list(Message, []) -> Message;
encode_list(Message, [Arg | Args]) ->
  AMF = amf0:encode(Arg),
  encode_list(<<Message/binary, AMF/binary>>, Args).



encode_tag(#flv_tag{type = Type, timestamp = Time, body = InnerTag}) ->
  <<TimeStampExt, TimeStamp:24>> = <<(round(Time)):32>>,
  StreamId = 0,
  Body = case Type of
    audio -> encode_audio_tag(InnerTag);
    video -> encode_video_tag(InnerTag);
    metadata -> encode_meta_tag(InnerTag)
  end,
  BodyLength = size(Body),
  PrevTagSize = ?FLV_TAG_HEADER_LENGTH + BodyLength,
  <<(flv:frame_format(Type)):8,BodyLength:24,TimeStamp:24,TimeStampExt:8,StreamId:24,Body/binary,PrevTagSize:32>>.

encode_audio_tag(#flv_audio_tag{decoder_config = true,
                    codec = aac,
                	  channels	= Channels,
                	  bitsize	= BitSize,
                	  rate	= SoundRate,
                    body = Body}) when is_binary(Body) ->
  <<?FLV_AUDIO_FORMAT_AAC:4, (flv:audio_rate(SoundRate)):2, (flv:audio_size(BitSize)):1, (flv:audio_type(Channels)):1,
    ?FLV_AUDIO_AAC_SEQUENCE_HEADER:8, Body/binary>>;


encode_audio_tag(#flv_audio_tag{codec = aac,
                    channels	= Channels,
                    bitsize	= BitSize,
                	  rate	= SoundRate,
                    body = Body}) when is_binary(Body) ->
	<<?FLV_AUDIO_FORMAT_AAC:4, (flv:audio_rate(SoundRate)):2, (flv:audio_size(BitSize)):1, (flv:audio_type(Channels)):1,
	  ?FLV_AUDIO_AAC_RAW:8, Body/binary>>;

encode_audio_tag(#flv_audio_tag{codec = Codec,
                    channels	= Channels,
                    bitsize	= BitSize,
                	  rate	= SoundRate,
                    body = Body}) when is_binary(Body) ->
	<<(flv:audio_codec(Codec)):4, (flv:audio_rate(SoundRate)):2, (flv:audio_size(BitSize)):1, (flv:audio_type(Channels)):1, Body/binary>>.



encode_video_tag(#flv_video_tag{frame_type = FrameType,
                   	decoder_config = true,
                   	codec = avc,
                   	composition_time = Time,
                    body = Body}) when is_binary(Body) ->
	<<(flv:frame_type(FrameType)):4, (flv:video_codec(avc)):4, ?FLV_VIDEO_AVC_SEQUENCE_HEADER, (round(Time)):24, Body/binary>>;

encode_video_tag(#flv_video_tag{frame_type = FrameType,
                   	codec = avc,
                   	composition_time = Time,
                    body = Body}) when is_binary(Body) ->
	<<(flv:frame_type(FrameType)):4, (flv:video_codec(avc)):4, ?FLV_VIDEO_AVC_NALU, (round(Time)):24, Body/binary>>;

encode_video_tag(#flv_video_tag{frame_type = FrameType,
                   	codec = CodecId,
                    body = Body}) when is_binary(Body) ->
	<<(flv:frame_type(FrameType)):4, (flv:video_codec(CodecId)):4, Body/binary>>.


encode_meta_tag(Metadata) when is_binary(Metadata) ->
  Metadata;

encode_meta_tag(Metadata) ->
  encode_list(Metadata).

	
% Extracts width and height from video frames.
% TODO: add to video_frame, not done yet
% @param IoDev
% @param Pos
% @param codecID
% @return {Width, Height}
getWidthHeight(IoDev, Pos, CodecID)->
	case CodecID of
		?FLV_VIDEO_CODEC_SORENSEN 	-> decodeSorensen(IoDev, Pos);
		?FLV_VIDEO_CODEC_SCREENVIDEO 	-> decodeScreenVideo(IoDev, Pos);
		?FLV_VIDEO_CODEC_ON2VP6 	-> decodeVP6(IoDev, Pos);
		%not sure if FLV_VIDEO_CODEC_ON2VP6 == FLV_VIDEO_CODEC_ON2VP6_ALPHA: needs to be tested...
		?FLV_VIDEO_CODEC_ON2VP6_ALPHA 	-> decodeVP6(IoDev, Pos);
		%FLV_VIDEO_CODEC_SCREENVIDEO2 doesn't seem to be widely used yet, no decoding method available
		?FLV_VIDEO_CODEC_SCREENVIDEO2 	-> {undefined, undefined}
	end.

				


% Extracts video header information for a tag.
% @param IoDev
% @param Pos
% @return {FrameType, CodecID}
extractVideoHeader(IoDev, Pos) ->	
	{ok, <<FrameType:4, CodecID:4>>} = file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH, 1),
	{Width, Height} = getWidthHeight(IoDev, Pos, CodecID),
	{FrameType, CodecID, Width, Height}.



decodeScreenVideo(IoDev, Pos) ->
	case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + 1, 4) of
		{ok, <<_Offset:4, Width:12, Height:12, _Rest:4>>} -> {Width, Height}
	end.
	
decodeSorensen(IoDev, Pos) ->
	case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + 1, 9) of
		{ok, IoList} ->
		  <<_Offset:30, Info:3, _Rest:39>> = IoList,
			case Info of
				
				0 -> <<_:30, _:3, Width1:8, Height1:8, _Rest1:23>> = IoList, {Width1, Height1};
				1 -> <<_:30, _:3, Width2:16, Height2:16, _Rest2:7>> = IoList, {Width2, Height2};
				2 -> {352, 288};
				3 -> {176, 144};
				4 -> {128, 96};
				5 -> {320, 240};
				6 -> {160, 120}
			end
	end.

decodeVP6(IoDev, Pos)->
	case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + 1, 6) of
			{ok, <<HeightHelper:4, WidthHelper:4, _Offset:24, Width:8, Height:8>>} -> {Width*16-WidthHelper, Height*16-HeightHelper}
	end.
% Extracts audio header information for a tag.
% @param IoDev
% @param Pos
% @return {SoundType, SoundSize, SoundRate, SoundFormat}
extractAudioHeader(IoDev, Pos) ->	
	case file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH, 1) of   
	  {ok, <<SoundFormat:4, SoundRate:2, SoundSize:1, SoundType:1>>} -> {SoundType, SoundSize, SoundRate, SoundFormat};
		eof -> {ok, done};
		{error, Reason} -> {error, Reason}
  end.


header() -> header(#flv_header{version = 1, audio = 1, video = 1}).

header(#flv_header{version = Version, audio = Audio, video = Video}) -> 
	Reserved = 0,
	Offset = 9,
	PrevTag = 0,
	<<"FLV",Version:8,Reserved:5,Audio:1,Reserved:1,Video:1,Offset:32,PrevTag:32>>;
header(Bin) when is_binary(Bin) ->
	<<"FLV", Version:8, _:5, Audio:1, _:1, Video:1, 0,0,0,9>> = Bin,
	#flv_header{version=Version,audio=Audio,video=Video}.
		


