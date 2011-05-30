%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Module to read and write FLV files
%%% It has many functions, but you need only several of them: read_header/1, header/1, read_tag/2, encode_tag/1
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org</a> for more information
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
-module(flv).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/video_frame.hrl").
-include("../include/flv.hrl").
-include("log.hrl").
-include("flv_constants.hrl").



-export([audio_codec/1, audio_type/1, audio_size/1, audio_rate/1, video_codec/1, frame_type/1, frame_format/1]).

-export([read_tag_header/2, read_tag/2, read_tag/1]).

-export([header/0, header/1, read_header/1, tag_header/1, data_offset/0]).
-export([video_size/2, extractAudioHeader/2]).

-export([encode_audio_tag/1, encode_video_tag/1, encode_meta_tag/1, encode_tag/1,
         decode_audio_tag/1, decode_video_tag/1, decode_meta_tag/1, decode_tag/1]).

-export([rtmp_tag_generator/1]).

-export([read_frame/1, read_frame/2, duration/1]).

read_frame(Reader, Offset) ->
  case flv:read_tag(Reader, Offset) of
		#flv_tag{} = Tag ->
		  flv_video_frame:tag_to_video_frame(Tag);
    eof -> eof;
    {error, Reason} -> {error, Reason}
  end.


read_frame(Binary) ->
  case read_tag(Binary) of
    more -> more;
    {ok, #flv_tag{} = Tag, Rest} ->
      {ok, flv_video_frame:tag_to_video_frame(Tag), Rest};
    {error, Reason} -> {error, Reason}
  end.


frame_format(audio) -> ?FLV_TAG_TYPE_AUDIO;
frame_format(video) -> ?FLV_TAG_TYPE_VIDEO;
frame_format(metadata) -> ?FLV_TAG_TYPE_META;
frame_format(?FLV_TAG_TYPE_AUDIO) -> audio;
frame_format(?FLV_TAG_TYPE_VIDEO) -> video;
frame_format(?FLV_TAG_TYPE_META) -> metadata.

audio_codec(pcm) -> ?FLV_AUDIO_FORMAT_PCM;
audio_codec(pcm_le) -> ?FLV_AUDIO_FORMAT_PCM_LE;
audio_codec(adpcm) -> ?FLV_AUDIO_FORMAT_ADPCM;
audio_codec(aac) -> ?FLV_AUDIO_FORMAT_AAC;
audio_codec(speex) -> ?FLV_AUDIO_FORMAT_SPEEX;
audio_codec(mp3) -> ?FLV_AUDIO_FORMAT_MP3;
audio_codec(pcma) -> ?FLV_AUDIO_FORMAT_A_G711;
audio_codec(pcmu) -> ?FLV_AUDIO_FORMAT_MU_G711;
audio_codec(nellymoser) -> ?FLV_AUDIO_FORMAT_NELLYMOSER;
audio_codec(nellymoser8) -> ?FLV_AUDIO_FORMAT_NELLYMOSER8;
audio_codec(?FLV_AUDIO_FORMAT_PCM) -> pcm;
audio_codec(?FLV_AUDIO_FORMAT_ADPCM) -> adpcm;
audio_codec(?FLV_AUDIO_FORMAT_PCM_LE) -> pcm_le;
audio_codec(?FLV_AUDIO_FORMAT_MP3) -> mp3;
audio_codec(?FLV_AUDIO_FORMAT_NELLYMOSER8) -> nellymoser8;
audio_codec(?FLV_AUDIO_FORMAT_NELLYMOSER) -> nellymoser;
audio_codec(?FLV_AUDIO_FORMAT_A_G711) -> pcma;
audio_codec(?FLV_AUDIO_FORMAT_MU_G711) -> pcmu;
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


video_codec(?FLV_VIDEO_CODEC_JPEG) -> mjpeg;
video_codec(?FLV_VIDEO_CODEC_SORENSON) -> sorenson;
video_codec(?FLV_VIDEO_CODEC_SCREENVIDEO) -> screen;
video_codec(?FLV_VIDEO_CODEC_ON2VP6) -> vp6;
video_codec(?FLV_VIDEO_CODEC_ON2VP6_ALPHA) -> vp6_alpha;
video_codec(?FLV_VIDEO_CODEC_SCREENVIDEO2) -> screen2;
video_codec(?FLV_VIDEO_CODEC_AVC) -> h264;

video_codec(mjpeg) -> ?FLV_VIDEO_CODEC_JPEG;
video_codec(sorenson) -> ?FLV_VIDEO_CODEC_SORENSON;
video_codec(screen) -> ?FLV_VIDEO_CODEC_SCREENVIDEO;
video_codec(vp6) -> ?FLV_VIDEO_CODEC_ON2VP6;
video_codec(vp6_alpha) -> ?FLV_VIDEO_CODEC_ON2VP6_ALPHA;
video_codec(screen2) -> ?FLV_VIDEO_CODEC_SCREENVIDEO2;
video_codec(h264) -> ?FLV_VIDEO_CODEC_AVC.

frame_type(frame) -> ?FLV_VIDEO_FRAME_TYPEINTER_FRAME;
frame_type(keyframe) -> ?FLV_VIDEO_FRAME_TYPE_KEYFRAME;
frame_type(disposable) -> ?FLV_VIDEO_FRAME_TYPEDISP_INTER_FRAME;
frame_type(generated) -> ?FLV_VIDEO_FRAME_TYPE_GENERATED;
frame_type(command) -> ?FLV_VIDEO_FRAME_TYPE_COMMAND;
frame_type(?FLV_VIDEO_FRAME_TYPE_COMMAND) -> command;
frame_type(?FLV_VIDEO_FRAME_TYPE_GENERATED) -> generated;
frame_type(?FLV_VIDEO_FRAME_TYPEDISP_INTER_FRAME) -> disposable;
frame_type(?FLV_VIDEO_FRAME_TYPEINTER_FRAME) -> frame;
frame_type(?FLV_VIDEO_FRAME_TYPE_KEYFRAME) -> keyframe.



rtmp_tag_generator(#video_frame{content = Content, dts = DTS} = Frame) ->
  Tag = flv_video_frame:encode(Frame),
  Type = case Content of
    audio -> 8;
    video -> 9;
    metadata -> 18
  end,
  TagSize = <<(size(Tag)):24, Type>>,
  fun(StartDTS, StreamId) ->
    ChannelId = rtmp_lib:channel_id(Content, StreamId),
    [rtmp:encode_id(new, ChannelId), <<(round(DTS - StartDTS)):24>>, TagSize, <<StreamId:32/little>>, Tag]
  end.


%%--------------------------------------------------------------------
%% @spec () -> Offset::numeric()
%% @doc Returns offset of first frame in FLV file
%% @end
%%--------------------------------------------------------------------
data_offset() -> ?FLV_HEADER_LENGTH + ?FLV_PREV_TAG_SIZE_LENGTH.



duration(Reader) ->
  {_Header, DataOffset} = read_header(Reader),
  duration(Reader, DataOffset, 0).

duration(Reader, Offset, Duration) ->
  case read_tag_header(Reader, Offset) of
    #flv_tag{timestamp = Timestamp, next_tag_offset = NextOffset} ->
      duration(Reader, NextOffset, Timestamp);
    eof ->
      Duration
  end.

%%--------------------------------------------------------------------
%% @spec (File::file()) -> {Header::flv_header(), Offset::numeric()}
%% @doc Read header from freshly opened file
%% @end
%%--------------------------------------------------------------------
read_header({Module, Device}) ->  % Always on first bytes
  case Module:pread(Device, 0, ?FLV_HEADER_LENGTH) of
    {ok, Data} ->
      {header(Data), size(Data) + ?FLV_PREV_TAG_SIZE_LENGTH};
    Else ->
      Else
  end;

read_header(Device) ->
  read_header({file, Device}).



tag_header(<<Type, Size:24, TimeStamp:24, TimeStampExt, _StreamId:24>>) ->
  <<TimeStampAbs:32>> = <<TimeStampExt, TimeStamp:24>>,
  #flv_tag{type = frame_format(Type), timestamp = TimeStampAbs, size = Size}.

%%--------------------------------------------------------------------
%% @spec (Body::binary()) -> Config::aac_config()
%% @doc Unpack binary AAC config into #aac_config{}
%% @end
%%--------------------------------------------------------------------
read_tag_header({Module,Device}, Offset) ->
	case Module:pread(Device,Offset, ?FLV_TAG_HEADER_LENGTH+1) of
	  {ok, <<Bin:?FLV_TAG_HEADER_LENGTH/binary, VideoFlavor:4, _:4>>} ->
	    FlvTag = tag_header(Bin),
      FlvTag1 = FlvTag#flv_tag{offset = Offset + ?FLV_TAG_HEADER_LENGTH,
       next_tag_offset = Offset + ?FLV_TAG_HEADER_LENGTH + FlvTag#flv_tag.size + ?FLV_PREV_TAG_SIZE_LENGTH},
      Flavor = case {FlvTag1#flv_tag.type, VideoFlavor} of
        {video, ?FLV_VIDEO_FRAME_TYPE_KEYFRAME} -> keyframe;
        _ -> frame
      end,
      FlvTag1#flv_tag{flavor = Flavor};
	  eof -> eof;
	  {error, Reason} -> {error, Reason}
  end;

read_tag_header(Device, Offset) ->
  read_tag_header({file,Device}, Offset).

%%--------------------------------------------------------------------
%% @spec (File::file(), Offset::numeric()) -> Tag::flv_tag()
%% @doc Reads from File FLV tag, starting on offset Offset. NextOffset is hidden in #flv_tag{}
%% @end
%%--------------------------------------------------------------------
read_tag(<<Header:?FLV_TAG_HEADER_LENGTH/binary, Data/binary>>) ->
  case tag_header(Header) of
    #flv_tag{type = audio, size = 0} = Tag ->
      Tag#flv_tag{body = #flv_audio_tag{codec = empty, body = <<>>, flavor = frame}, flavor = frame};

    #flv_tag{type = Type, size = Size} = Tag when size(Data) >= Size ->
      {Body, Rest} = erlang:split_binary(Data, Size),

      Flavor = case Type of
        video ->
          case Body of
            <<?FLV_VIDEO_FRAME_TYPE_KEYFRAME:4, _CodecID:4, _/binary>> -> keyframe;
            _ -> frame
          end;
        _ -> frame
      end,

      {ok, decode_tag(Tag#flv_tag{body = Body, flavor = Flavor}), Rest};

    #flv_tag{} ->
      more;
    Else -> Else
  end;

read_tag(Header) when is_binary(Header) ->
  more.


read_tag({Module,Device} = Reader, Offset) ->
  case read_tag_header(Reader, Offset) of
    #flv_tag{type = audio, size = 0} = Tag ->
      Tag#flv_tag{body = #flv_audio_tag{codec = empty, body = <<>>, flavor = frame}, flavor = frame};

    #flv_tag{type = Type, size = Size} = Tag ->
      {ok, Body} = Module:pread(Device, Offset + ?FLV_TAG_HEADER_LENGTH, Size),

      Flavor = case Type of
        video ->
          case Body of
            <<?FLV_VIDEO_FRAME_TYPE_KEYFRAME:4, _CodecID:4, _/binary>> -> keyframe;
            _ -> frame
          end;
        _ -> frame
      end,

      decode_tag(Tag#flv_tag{body = Body, flavor = Flavor});
    Else -> Else
  end;

read_tag(Device, Offset) ->
  read_tag({file,Device}, Offset).


decode_video_tag(<<FrameType:4, ?FLV_VIDEO_CODEC_AVC:4, ?FLV_VIDEO_AVC_NALU:8, CTime:24, Body/binary>>) ->
  #flv_video_tag{flavor = flv:frame_type(FrameType), codec = h264, composition_time = CTime, body= Body};

decode_video_tag(<<_FrameType:4, ?FLV_VIDEO_CODEC_AVC:4, ?FLV_VIDEO_AVC_SEQUENCE_HEADER:8, CTime:24, Body/binary>>) ->
  #flv_video_tag{flavor = config, codec = h264, composition_time = CTime, body= Body};

decode_video_tag(<<FrameType:4, CodecId:4, Body/binary>>) ->
  #flv_video_tag{flavor = flv:frame_type(FrameType), codec = flv:video_codec(CodecId), composition_time = 0, body = Body}.



decode_audio_tag(<<?FLV_AUDIO_FORMAT_AAC:4, Rate:2, Bitsize:1, Channels:1, ?FLV_AUDIO_AAC_RAW:8, Body/binary>>) ->
  #flv_audio_tag{codec = aac, channels = flv:audio_type(Channels), bitsize = flv:audio_size(Bitsize),
                 flavor = frame, rate	= flv:audio_rate(Rate), body= Body};

decode_audio_tag(<<?FLV_AUDIO_FORMAT_AAC:4, Rate:2, Bitsize:1, Channels:1, ?FLV_AUDIO_AAC_SEQUENCE_HEADER:8, Body/binary>>) ->
  #flv_audio_tag{codec = aac, channels = flv:audio_type(Channels), bitsize = flv:audio_size(Bitsize),
                 flavor = config, rate	= flv:audio_rate(Rate), body= Body};

decode_audio_tag(<<CodecId:4, Rate:2, Bitsize:1, Channels:1, Body/binary>>) ->
  #flv_audio_tag{codec = flv:audio_codec(CodecId), channels = flv:audio_type(Channels), bitsize = flv:audio_size(Bitsize),
                 flavor = frame, rate	= flv:audio_rate(Rate), body= Body}.


decode_meta_tag(Metadata) when is_list(Metadata) ->
  Metadata;

decode_meta_tag(Metadata) when is_binary(Metadata) ->
  decode_list(Metadata);

decode_meta_tag({object, Metadata}) ->
  [Metadata].

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


%%--------------------------------------------------------------------
%% @spec (FLVTag::flv_tag()) -> Tag::binary()
%% @doc Packs #flv_tag{} into binary, suitable for writing into file
%% @end
%%--------------------------------------------------------------------
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

encode_audio_tag(#flv_audio_tag{flavor = config,
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



encode_video_tag(#flv_video_tag{flavor = config,
                   	codec = h264,
                   	composition_time = Time,
                    body = Body}) when is_binary(Body) ->
	<<(flv:frame_type(keyframe)):4, (flv:video_codec(h264)):4, ?FLV_VIDEO_AVC_SEQUENCE_HEADER, (round(Time)):24, Body/binary>>;

encode_video_tag(#flv_video_tag{flavor = Flavor,
                   	codec = h264,
                   	composition_time = Time,
                    body = Body}) when is_binary(Body) ->
	<<(flv:frame_type(Flavor)):4, (flv:video_codec(h264)):4, ?FLV_VIDEO_AVC_NALU, (round(Time)):24, Body/binary>>;

encode_video_tag(#flv_video_tag{flavor = Flavor,
                   	codec = CodecId,
                    body = Body}) when is_binary(Body) ->
	<<(flv:frame_type(Flavor)):4, (flv:video_codec(CodecId)):4, Body/binary>>.


encode_meta_tag(Metadata) when is_binary(Metadata) ->
  Metadata;

encode_meta_tag(Metadata) when is_list(Metadata) ->
  encode_list(Metadata).








video_size(<<H_Helper:4, W_Helper:4, _:24, W:8, H:8, _/binary>>,   vp6) -> {ok, {W*16-W_Helper, H*16-H_Helper}};
video_size(_, vp6) -> {more, 6};

video_size(<<_:4,       Width:12, Height:12, _:4,  _/binary>>,   screen) -> {ok, {Width, Height}};
video_size(_, screen) -> {more, 8};

video_size(<<_:30, 0:3, Width:8,  Height:8,  _:23, _/binary>>, sorenson) -> {ok, {Width, Height}};
video_size(<<_:30, 1:3, Width:16, Height:16, _:7,  _/binary>>, sorenson) -> {ok, {Width, Height}};
video_size(<<_:30, 2:3,                      _:39, _/binary>>, sorenson) -> {ok, {352, 288}};
video_size(<<_:30, 3:3,                      _:39, _/binary>>, sorenson) -> {ok, {176, 144}};
video_size(<<_:30, 4:3,                      _:39, _/binary>>, sorenson) -> {ok, {128, 96}};
video_size(<<_:30, 5:3,                      _:39, _/binary>>, sorenson) -> {ok, {320, 240}};
video_size(<<_:30, 6:3,                      _:39, _/binary>>, sorenson) -> {ok, {160, 120}};
video_size(_, sorenson) -> {more, 9};
video_size(_, _) -> {error, unknown}.



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

%%--------------------------------------------------------------------
%% @spec (Header::flv_header()) -> Body::binary()
%% @doc Packs FLV file header into binary
%% @end
%%--------------------------------------------------------------------
header(#flv_header{version = Version, audio = Audio, video = Video}) ->
	Reserved = 0,
	Offset = 9,
	PrevTag = 0,
	<<"FLV",Version:8,Reserved:5,Audio:1,Reserved:1,Video:1,Offset:32,PrevTag:32>>;
header(Bin) when is_binary(Bin) ->
	<<"FLV", Version:8, _:5, Audio:1, _:1, Video:1, 0,0,0,9>> = Bin,
	#flv_header{version=Version,audio=Audio,video=Video}.



