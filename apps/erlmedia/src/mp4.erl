%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2010 Max Lapshin
%%% @doc        MP4 decoding module
%%% 
%%% This is how ttxt subtitle track is translated
%%% [<<"onCuePoint">>,
%%%     [{<<"name">>,<<"onCuePoint">>},
%%%     {<<"time">>,16.0},
%%%     {<<"parameters">>,
%%%     [{<<"text">>, <<"But then you see the light...">>}]},
%%%      {<<"type">>,<<"navigation">>}]]
%%% 
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
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

-module(mp4).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/mp4.hrl").
-include("../include/srt.hrl").
-include("../include/video_frame.hrl").
-include("log.hrl").

-export([ftyp/2, moov/2, mvhd/2, trak/2, tkhd/2, mdia/2, mdhd/2, stbl/2, stsd/2, esds/2, avcC/2]).
-export([btrt/2, stsz/2, stts/2, stsc/2, stss/2, stco/2, co64/2, smhd/2, minf/2, ctts/2, udta/2]).
-export([mp4a/2, mp4v/2, avc1/2, s263/2, samr/2, free/2]).
-export([hdlr/2, vmhd/2, dinf/2, dref/2, 'url '/2, 'pcm '/2, 'spx '/2, '.mp3'/2]).
-export([meta/2, ilst/2, covr/2, data/2, nam/2, alb/2]).
-export([extract_language/1,get_coverart/1]).

-export([fill_track/9]).

-record(esds, {
  object_type,
  stream_type,
  upstream,
  buffer_size,
  max_bitrate,
  avg_bitrate,
  specific
}).


-export([mp4_desc_length/1, open/2, read_frame/2, frame_count/1, seek/4, seek/5, mp4_read_tag/1]).

-define(FRAMESIZE, 32).


open(Reader, Options) ->
  {ok, Mp4Media} = read_header(Reader),
  #mp4_media{tracks = Tracks} = Mp4Media1 = read_srt_files(Mp4Media, proplists:get_value(url, Options)),
  Index = build_index(Tracks),
  {ok, Mp4Media1#mp4_media{index = Index, reader = Reader, tracks = list_to_tuple(Tracks)}}.

get_coverart(Reader) ->
  {ok, MP4_Media} = read_header(#mp4_media{}, Reader, 0),
  case proplists:get_value(cover_art,MP4_Media#mp4_media.additional,undefined) of
    undefined -> <<>>;
    Bin -> Bin
  end.

read_header(Reader) ->
  read_header(#mp4_media{}, Reader, 0).


read_srt_files(Media, Url) when is_binary(Url) ->
  read_srt_files(Media, binary_to_list(Url));

read_srt_files(Media, Url) ->
  case filelib:is_file(Url) of
    true ->
      Wildcard = filename:dirname(Url) ++ "/" ++ filename:basename(Url, ".mp4") ++ ".*" ++ ".srt",
      lists:foldl(fun(SrtFile, Mp4Media) ->
         read_srt_file(Mp4Media, SrtFile)
      end, Media, filelib:wildcard(Wildcard));
    _ ->
      Media
  end.


read_srt_file(#mp4_media{tracks = Tracks, duration = Duration} = Media, SrtFile) ->
  {match,[Lang]} = re:run(SrtFile, "\\.(\\w+)\\.srt", [{capture,all_but_first,list}]),
  Subtitles = parse_srt_file(SrtFile),
  SrtFrames = subtitles_to_mp4_frames(Subtitles),
  Track = #mp4_track{data_format = srt, content = text, track_id = length(Tracks)+1, timescale = 1, 
  duration = Duration, language = list_to_binary(Lang), frames = SrtFrames},
  Media#mp4_media{tracks = Tracks ++ [Track]}.
  
parse_srt_file(File) ->
  % (11:41:23 PM) max lapshin: AccessModule чаще всего — file
  % (11:41:31 PM) max lapshin: но в принципе может быть и http_file
  % (11:41:50 PM) max lapshin: поэтому надо сделать такую проверку:
  % parsed_srt_tracks({file, _}, Options) ->
  {ok, Data} = file:read_file(File),
  {ok, Subtitles, _More} = srt_parser:parse(Data),
  Subtitles.

subtitles_to_mp4_frames(Subtitles) ->
  [#mp4_frame{id = Id, dts = From, pts = To, size = size(Text), codec = srt, body = Text, content = text} ||
   #srt_subtitle{id = Id, from = From, to = To, text = Text} <- Subtitles].

read_header(#mp4_media{additional = Additional} = Mp4Media, {Module, Device} = Reader, Pos) -> 
  case read_atom_header(Reader, Pos) of
    eof -> {ok, Mp4Media};
    {error, Reason} -> {error, Reason};
    {atom, mdat, _Offset, all_file} ->
      {ok, Mp4Media};
    {atom, mdat, Offset, Length} ->
      read_header(Mp4Media, Reader, Offset + Length);
    {atom, _AtomName, Offset, 0} -> 
      read_header(Mp4Media, Reader, Offset);
    {atom, AtomName, Offset, Length} -> 
      % ?D({"Root atom", AtomName, Length}),
      {ok, AtomData} = Module:pread(Device, Offset, Length),
      NewMedia = case atom_to_binary(AtomName, latin1) of
        <<"EV", _/binary>> ->
          Mp4Media#mp4_media{additional = [{AtomName,AtomData}| Additional]};
        _ ->
          case erlang:function_exported(mp4, AtomName, 2) of
            true -> mp4:AtomName(AtomData, Mp4Media);
            false -> Mp4Media
          end
      end,
      read_header(NewMedia, Reader, Offset + Length)
  end.

read_atom_header({Module, Device}, Pos) ->
  case Module:pread(Device, Pos, 8) of
    {ok, <<0:32, AtomName/binary>>} ->
      {atom, binary_to_atom(AtomName, latin1), Pos + 8, all_file};
    {ok, <<1:32, AtomName/binary>>} ->
      case Module:pread(Device, Pos+4, 12) of
        {ok, <<AtomName:4/binary, AtomLength:64>>} when AtomLength >= 12 -> 
          {atom, binary_to_atom(AtomName, latin1), Pos + 16, AtomLength - 16};
        eof ->
          eof;
        {ok, Bin} ->
          ?D({invalid_atom, Bin}),
          {error, {invalid_atom, Bin}};
        {error, Error} ->
          {error, Error}
      end;
    {ok, <<AtomLength:32, AtomName/binary>>} when AtomLength >= 8 ->
      % ?D({"Atom", binary_to_atom(AtomName, latin1), Pos, AtomLength}),
      {atom, binary_to_atom(AtomName, latin1), Pos + 8, AtomLength - 8};
    eof ->
      eof;
    {ok, Bin} ->
      ?D({invalid_atom, Bin}),
      {error, {invalid_atom, Bin}};
    {error, Error} ->
      {error, Error}
  end.


seek(#mp4_media{} = Media, Audio, Video, Timestamp) ->
  seek(Media, Audio, Video, Timestamp, keyframe).


seek(#mp4_media{} = Media, Audio, Video, Timestamp, SeekMode) ->
  A = case SeekMode of
    keyframe -> undefined;
    _ -> Audio
  end,
  seek(Media, A, Video, Timestamp, 0, undefined, SeekMode).

seek(Media, Audio, Video, Timestamp, Id, Found, SeekMode) ->
  case read_frame(Media, #frame_id{id = Id, a = Audio, v = Video}) of
    #mp4_frame{dts = DTS} when DTS >= Timestamp andalso SeekMode == frame -> {Id,DTS};
    #mp4_frame{keyframe = true, dts = DTS} when DTS > Timestamp andalso SeekMode == keyframe -> Found;
    #mp4_frame{keyframe = true, dts = DTS} when SeekMode == keyframe -> seek(Media, Audio, Video, Timestamp, Id+1, {Id,DTS}, SeekMode);
    #mp4_frame{dts = DTS} when SeekMode == frame -> seek(Media, Audio, Video, Timestamp, Id+1, {Id, DTS}, SeekMode);
    #mp4_frame{dts = _DTS} -> seek(Media, Audio, Video, Timestamp, Id+1, Found, SeekMode);
    eof -> undefined
  end.

read_frame(#mp4_media{tracks = Tracks, index = Index} = Media, #frame_id{id = Id,a = Audio,v = Video, t = Text} = FrameId) ->
  IndexOffset = Id*4,
  
  case Index of
    <<_:IndexOffset/binary, Audio, _:1, AudioId:23, _/binary>> -> 
      (unpack_frame(element(Audio,Tracks), AudioId))#mp4_frame{next_id = FrameId#frame_id{id = Id+1}, content = audio};
    <<_:IndexOffset/binary, Text, _:1, TextId:23, _/binary>> -> 
       %?D({read_text,Text,TextId}),
      (unpack_frame(element(Text,Tracks), TextId))#mp4_frame{next_id = FrameId#frame_id{id = Id+1}, content = text};
    <<_:IndexOffset/binary, Video, _:1, VideoId:23, _/binary>> -> 
      (unpack_frame(element(Video,Tracks), VideoId))#mp4_frame{next_id = FrameId#frame_id{id = Id+1}, content = video};
    <<_:IndexOffset/binary>> -> 
      eof;
    <<_:IndexOffset/binary, _OtherTrackId, _K:1, _FrameIndex:23, _/binary>> ->
      read_frame(Media, FrameId#frame_id{id = Id+1})
  end.
  
  
unpack_frame(#mp4_track{frames = Frames, content = text, data_format = _Codec}, Id) when Id < length(Frames) ->
  lists:nth(Id+1, Frames);
  
unpack_frame(#mp4_track{frames = Frames, data_format = Codec}, Id) when Id*?FRAMESIZE < size(Frames) ->
  FrameOffset = Id*?FRAMESIZE,

  <<_:FrameOffset/binary, FKeyframe:1, Size:63, Offset:64, DTS:64/float, PTS:64/float, _/binary>> = Frames,
  Keyframe = case FKeyframe of
    1 -> true;
    0 -> false
  end,
  #mp4_frame{id = Id, dts = DTS, pts = PTS, size = Size, offset = Offset, keyframe = Keyframe, codec = Codec}.


frame_count(undefined) -> 0;
frame_count(#mp4_track{frames = Frames}) -> size(Frames) div ?FRAMESIZE;
frame_count(Frames) -> size(Frames) div ?FRAMESIZE.

parse_atom(<<>>, Mp4Parser) ->
  Mp4Parser;
  
parse_atom(Atom, _) when size(Atom) < 4 ->
  {error, "Invalid atom"};
  
parse_atom(<<AllAtomLength:32, BinaryAtomName:4/binary, AtomRest/binary>>, Mp4Parser) when (size(AtomRest) >= AllAtomLength - 8) ->
  AtomLength = AllAtomLength - 8,
  <<Atom:AtomLength/binary, Rest/binary>> = AtomRest,
  AtomName = case binary:bin_to_list(BinaryAtomName) of
   [169|Value] -> 
     binary_to_atom(binary:list_to_bin(Value),latin1);
   _ValidValue ->
     binary_to_atom(BinaryAtomName,latin1)
  end,
  NewMp4Parser = case erlang:function_exported(?MODULE, AtomName, 2) of
    true -> ?MODULE:AtomName(Atom, Mp4Parser);
    false -> Mp4Parser
    % false -> Mp4Parser
  end,
  parse_atom(Rest, NewMp4Parser);
  
parse_atom(<<AllAtomLength:32, BinaryAtomName:4/binary, _Rest/binary>>, Mp4Parser) ->
  ?D({"Invalid atom", AllAtomLength, binary_to_atom(BinaryAtomName, latin1), size(_Rest)}),
  Mp4Parser;

parse_atom(<<0:32>>, Mp4Parser) ->
  ?D("NULL atom"),
  Mp4Parser.

  
% FTYP atom
ftyp(<<_Major:4/binary, _Minor:4/binary, _CompatibleBrands/binary>>, MediaInfo) ->
  % ?D({"File", _Major, _Minor, ftyp(_CompatibleBrands, [])}),
  % NewParser = Mp4Parser#mp4_header{file_type = binary_to_list(Major), file_types = decode_atom(ftyp, CompatibleBrands, [])},
  MediaInfo;

ftyp(<<>>, BrandList) when is_list(BrandList) ->
  lists:reverse(BrandList);

ftyp(<<Brand:4/binary, CompatibleBrands/binary>>, BrandList) ->
  ftyp(CompatibleBrands, [Brand|BrandList]).
  
% Movie box
moov(Atom, MediaInfo) ->
  Media = #mp4_media{tracks = Tracks} = parse_atom(Atom, MediaInfo),
  Media#mp4_media{tracks = lists:reverse(Tracks)}.

free(_Atom, Media) ->
  Media.

% MVHD atom
mvhd(<<0:32, CTime:32, MTime:32, TimeScale:32, Duration:32, Rate:16, _RateDelim:16,
      Volume:16, 0:16, _Reserved1:64, Matrix:36/binary, _Reserved2:24/binary, NextTrackId:32>>, #mp4_media{} = Media) ->
        
  _Meta = [{ctime,CTime},{mtime,MTime},{timescale,TimeScale},{duration,Duration},{rate,Rate},
          {volume,Volume},{matrix,Matrix},{next_track,NextTrackId}],
  % ?D(Meta),
  Media#mp4_media{timescale = TimeScale, duration = Duration/TimeScale}.

udta(UDTA, Media) ->
  parse_atom(UDTA, Media).

meta(<<0:32, Meta/binary>>, #mp4_media{} = Media) ->
  parse_atom(Meta, Media).

ilst(ILST, #mp4_media{} = Media) ->
  parse_atom(ILST, Media).

covr(Data, #mp4_media{additional = Add} = Media) ->
  CoverArt = parse_atom(Data, covr),
  Media#mp4_media{additional = [{cover_art, CoverArt}|Add]}.

nam(Data, #mp4_media{additional = Add} = Media) ->
  Name = parse_atom(Data, name),
  Metadata = case proplists:get_value(name, Add, undefined) of
    undefined ->
      Name;
    AnotherData -> 
      <<AnotherData/binary,"-",Name/binary>> 
  end,
  Media#mp4_media{additional = [{name, Metadata}|Add]}.

alb(Data, #mp4_media{additional = Add} = Media) ->
  Name = parse_atom(Data, name),
  Metadata = case proplists:get_value(name, Add, undefined) of
    undefined ->
      Name;
    AnotherData -> 
      <<AnotherData/binary,"-",Name/binary>> 
  end,
  Media#mp4_media{additional = [{name, Metadata}|Add]}.

data(<<_Flags:32, 0:32, Data/binary>>, _Meaning) ->
  Data.

% '----'(Meta, #mp4_media{} = Media) ->

% Track box
trak(<<>>, MediaInfo) ->
  MediaInfo;
  
trak(Atom, MediaInfo) ->
  Track = parse_atom(Atom, #mp4_track{}),
  fill_track_info(MediaInfo, Track).



clean_track(#mp4_track{} = Track) ->
  Track#mp4_track{sample_sizes = [], sample_dts = [], sample_offsets = [], sample_composition = [],
                  keyframes = [], chunk_offsets = [], chunk_sizes = []}.

append_track(#mp4_media{tracks = Tracks} = MediaInfo, 
             #mp4_track{content = video, width = Width, height = Height} = Track)  ->
  MediaInfo#mp4_media{width = Width, height = Height, tracks = [clean_track(Track)|Tracks]};

append_track(#mp4_media{tracks = Tracks} = MediaInfo, #mp4_track{} = Track) ->
  MediaInfo#mp4_media{tracks = [clean_track(Track)|Tracks]}.


fill_track_info(#mp4_media{} = MediaInfo, #mp4_track{} = Track) ->
  {Frames, MaxDTS} = fill_track(Track),
  Duration = max_duration(MediaInfo, MaxDTS),
  Media1 = append_track(MediaInfo, Track#mp4_track{frames = Frames}),
  Media1#mp4_media{duration = Duration}.

max_duration(#mp4_media{duration = undefined}, Duration2) -> Duration2;
max_duration(#mp4_media{duration = Duration1}, Duration2) when Duration2 > Duration1 -> Duration2;
max_duration(#mp4_media{duration = Duration1}, _) -> Duration1.

  

% Track header
tkhd(<<0, Flags:24, CTime:32, MTime:32, TrackID:32, _Reserved1:32, 
       Duration:32, _Reserved2:64, Layer:16, _AlternateGroup:16,
       Volume, _VolDelim, _Reserved3:16, Matrix:36/binary, Width:16, _WidthDelim:16, Height:16, _HeightDelim:16>>, Mp4Track) ->
  _Meta = [{flags,Flags},{ctime,CTime},{mtime,MTime},{track_id,TrackID},{duration,Duration},{layer,Layer},
         {volume,Volume},{matrix,Matrix},{width,Width},{height,Height}],
  % ?D(Meta),
  Mp4Track#mp4_track{track_id = TrackID, duration = Duration}.

% Media box
mdia(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

% Media header
mdhd(<<0:8, _Flags:24, _Ctime:32, _Mtime:32, TimeScale:32, Duration:32,
       _:1, Language:15/bitstring, _Quality:16>>, #mp4_track{} = Mp4Track) ->
  % ?D({"Timescale:", Duration, extract_language(_Language)}),
  Mp4Track#mp4_track{timescale = TimeScale, duration = Duration, language = extract_language(Language)};

mdhd(<<1:8, _Flags:24, _Ctime:64, _Mtime:64, TimeScale:32, Duration:64, 
       _:1, Language:15/bitstring, _Quality:16>>, Mp4Track) ->
  % ?D({"Timescale:", Duration, extract_language(_Language)}),
  Mp4Track#mp4_track{timescale = TimeScale, duration = Duration, language = extract_language(Language)}.
  
extract_language(<<L1:5, L2:5, L3:5>>) ->
  case list_to_binary([L1+16#60, L2+16#60, L3+16#60]) of
    <<"und">> -> undefined;
    Else -> Else
  end.


%% Handler Reference Box
hdlr(<<0:32, 0:32, "vide", _Reserved:8/binary, NameNull/binary>>, Mp4Track) ->
  Len = (size(NameNull) - 1),
  _Name = case NameNull of
    <<N:Len/binary, 0>> -> N;
    _ -> NameNull
  end,
  Mp4Track#mp4_track{content = video};

hdlr(<<0:32, 0:32, "soun", _Reserved:8/binary, NameNull/binary>>, Mp4Track) ->
  Len = (size(NameNull) - 1),
  _Name = case NameNull of
    <<N:Len/binary, 0>> -> N;
    _ -> NameNull
  end,
  Mp4Track#mp4_track{content = audio};

hdlr(<<0:32, 0:32, "hint", _Reserved:8/binary, NameNull/binary>>, Mp4Track) ->
  Len = (size(NameNull) - 1),
  _Name = case NameNull of
    <<N:Len/binary, 0>> -> N;
    _ -> NameNull
  end,
  Mp4Track#mp4_track{content = hint};

hdlr(<<0:32, 0:32, _Handler:4/binary, _Reserved:8/binary, NameNull/binary>>, #mp4_media{} = Mp4Media) ->
  Len = (size(NameNull) - 1),
  _Name = case NameNull of
    <<N:Len/binary, 0>> -> N;
    _ -> NameNull
  end,
  % ?D({hdlr, Handler, Name}),
  Mp4Media;

hdlr(<<0:32, 0:32, Handler:4/binary, _Reserved:8/binary, NameNull/binary>>, #mp4_track{} = Mp4Track) ->
  Len = (size(NameNull) - 1),
  _Name = case NameNull of
    <<N:Len/binary, 0>> -> N;
    _ -> NameNull
  end,
  % ?D({hdlr, Handler, Name}),
  Mp4Track#mp4_track{content = binary_to_atom(Handler, latin1)}.
  
% SMHD atom
smhd(<<0:8, _Flags:3/binary, 0:16/big-signed-integer, _Reserve:2/binary>>, Mp4Track) ->
  Mp4Track;

smhd(<<0:8, _Flags:3/binary, _Balance:16/big-signed-integer, _Reserve:2/binary>>, Mp4Track) ->
  Mp4Track.

% Media information
minf(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

%% Video Media Header Box
vmhd(<<_Version:32, _Mode:16, _R:16, _G:16, _B:16>>, Mp4Track) ->
  % _VMHD = {vmhd, Version, Mode, R, G, B},
  % ?D(_VMHD),
  Mp4Track.
  
dinf(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).
  

dref(<<0:32, _Count:32, Atom/binary>> = _Dref, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

'url '(_URL, Mp4Track) ->
  Mp4Track.

% Sample table box
stbl(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

% Sample description
stsd(<<0:8, _Flags:3/binary, _EntryCount:32, EntryData/binary>>, Mp4Track) ->
  % ?D({_EntryCount, EntryData}),
  parse_atom(EntryData, Mp4Track).


mp4a(<<_Reserved:6/binary, _RefIndex:16, _Unknown:8/binary, _ChannelsCount:32,
       _SampleSize:32, _SampleRate:32, Atom/binary>>, Mp4Track) ->
  parse_atom(Atom, Mp4Track#mp4_track{data_format = aac}).
  
mp4v(_T, Mp4Track) ->
  Mp4Track#mp4_track{data_format = mpeg4}.


'.mp3'(_, Mp4Track) ->
  Mp4Track#mp4_track{data_format = mp3}.


avc1(<<_Reserved:6/binary, _RefIndex:16, _Unknown1:16/binary, Width:16, Height:16,
      HorizRes:16, _:16, VertRes:16, _:16, _FrameCount:16, _CompressorName:32/binary,
      Depth:16, _Predefined:16, _Unknown:4/binary, Atom/binary>>, Mp4Track) ->
  _Meta = [{width,Width},{height,Height},
          {horiz_res, HorizRes},{vert_res, VertRes},
          {depth,Depth}],
  % ?D({"Video size:", Meta}),
  parse_atom(Atom, Mp4Track#mp4_track{data_format = h264, width = Width, height = Height}).

s263(<<_Reserved:6/binary, _RefIndex:16, _Unknown1:16/binary, Width:16, Height:16,
       _HorizRes:32, _VertRes:32, _FrameCount:16, _CompressorName:32/binary, _Depth:16, 
       _Predefined:16, _Unknown:4/binary, Atom/binary>>, Mp4Track) ->
  % ?D({"Video size:", Width, Height}),
  parse_atom(Atom, Mp4Track#mp4_track{data_format = s263, width = Width, height = Height}).

samr(<<_Reserved:2/binary, _RefIndex:16, Atom/binary>> = AMR, Mp4Track) ->
  ?D(AMR),
  parse_atom(Atom, Mp4Track#mp4_track{data_format = samr}).


'pcm '(_, Mp4Track) ->
  ?D(pcm),
  Mp4Track#mp4_track{data_format = pcm_le}.

'spx '(_, Mp4Track) ->
  ?D(pcm),
  Mp4Track#mp4_track{data_format = speex}.

  
%%%%%%%%%%%%%%%%%%    ESDS     %%%%%%%%%%%%%%
esds(<<Version:8, _Flags:3/binary, DecoderConfig/binary>>, #mp4_track{} = Mp4Track) when Version == 0 ->
  % ?D({"Extracted audio config", DecoderConfig}),
  ESDS = config_from_esds_tag(DecoderConfig),
  % ?D(ESDS),
  Mp4Track#mp4_track{decoder_config = ESDS#esds.specific, data_format = ESDS#esds.object_type}.

% avcC atom
avcC(DecoderConfig, #mp4_track{} = Mp4Track) ->
  % ?D({"Extracted video config", DecoderConfig, h264:unpack_config(DecoderConfig)}),
  Mp4Track#mp4_track{decoder_config = DecoderConfig}.

btrt(<<_BufferSize:32, MaxBitRate:32, AvgBitRate:32>>, #mp4_track{} = Mp4Track) ->
  Mp4Track#mp4_track{max_bitrate = MaxBitRate, bitrate = AvgBitRate}.


%% FIXME: Code here must be relocated in some more generic place and way. 
%% Here goes not some esds tag, but IOD (Initial Object Description)
%% Look how to parse it at vlc/modules/demux/ts.c:2400
%%

config_from_esds_tag(Data) ->
  config_from_esds_tag(Data, #esds{}).

config_from_esds_tag(Data, ESDS) ->
  case mp4_read_tag(Data) of
    {?MP4ESDescrTag, <<_ID1:16, _Priority1, Description/binary>>, <<>>} ->
      config_from_esds_tag(Description, ESDS);
    {?MP4DecConfigDescrTag, <<ObjectType, StreamType, BufferSize:24, MaxBitrate:32, AvgBitrate:32, Rest1/binary>>, Rest2} ->
      ESDS1 = config_from_esds_tag(Rest1, ESDS#esds{
        object_type = mp4_object_type(ObjectType), stream_type = StreamType, buffer_size = BufferSize,
        max_bitrate = MaxBitrate, avg_bitrate = AvgBitrate}),
      config_from_esds_tag(Rest2, ESDS1);
    {?MP4DecSpecificDescrTag, <<Config/binary>>, _} ->
      ESDS#esds{specific = Config};
    {?MP4Unknown6Tag, _Body, Rest} ->
      config_from_esds_tag(Rest, ESDS);
    {_Tag, _Data, Rest} ->
      ?D({"Unknown esds tag. Send this line to max@maxidoors.ru: ", _Tag, _Data}),
      config_from_esds_tag(Rest, ESDS);
    undefined ->
      ESDS
  end.

mp4_object_type(8) -> text;
mp4_object_type(16#20) -> mpeg4;
mp4_object_type(16#21) -> h264;
mp4_object_type(16#40) -> aac;
mp4_object_type(16#60) -> mpeg2video;
mp4_object_type(16#61) -> mpeg2video;
mp4_object_type(16#62) -> mpeg2video;
mp4_object_type(16#63) -> mpeg2video;
mp4_object_type(16#64) -> mpeg2video;
mp4_object_type(16#65) -> mpeg2video;
mp4_object_type(16#66) -> aac;
mp4_object_type(16#67) -> aac;
mp4_object_type(16#68) -> aac;
mp4_object_type(16#69) -> mp3;
mp4_object_type(16#6A) -> mpeg1video;
mp4_object_type(16#6B) -> mp3;
mp4_object_type(16#6C) -> mjpeg;
mp4_object_type(16#6D) -> png;
mp4_object_type(16#6E) -> jpeg2000;
mp4_object_type(16#A3) -> vc1;
mp4_object_type(16#A4) -> dirac;
mp4_object_type(16#A5) -> ac3;
mp4_object_type(16#DD) -> vorbis;
mp4_object_type(16#E0) -> dvd_subtitle;
mp4_object_type(16#E1) -> qcelp.

mp4_desc_length(<<0:1, Length:7, Rest:Length/binary, Rest2/binary>>) ->
  {Rest, Rest2};

mp4_desc_length(<<1:1, Length1:7, 0:1, Length:7, Rest/binary>>) ->
  TagLength = Length1 * 128 + Length,
  <<Rest1:TagLength/binary, Rest2/binary>> = Rest,
  {Rest1, Rest2};

mp4_desc_length(<<1:1, Length2:7, 1:1, Length1:7, 0:1, Length:7, Rest/binary>>)  ->
  TagLength = (Length2 bsl 14 + Length1 bsl 7 + Length),
  <<Rest1:TagLength/binary, Rest2/binary>> = Rest,
  {Rest1, Rest2};

mp4_desc_length(<<1:1, Length3:7, 1:1, Length2:7, 1:1, Length1:7, 0:1, Length:7, Rest/binary>>)  ->
  TagLength = (Length3 bsl 21 + Length2 bsl 14 + Length1 bsl 7 + Length),
  <<Rest1:TagLength/binary, Rest2/binary>> = Rest,
  {Rest1, Rest2}.

mp4_read_tag(<<>>) ->
  undefined;

mp4_read_tag(<<Tag, Data/binary>>) ->
  {Body, Rest} = mp4_desc_length(Data),
  {Tag, Body, Rest}.


%%%%%%%%%%%%%%%%%%    STSZ  %%%%%%%%%%%%%%%%
% Sample sizes in bytes are stored here
%%
stsz(<<_Version:8, _Flags:24, 0:32, SampleCount:32, SampleSizeData/binary>>, Mp4Track) -> % case for different sizes
  read_stsz(SampleSizeData, SampleCount, Mp4Track);
  
stsz(<<_Version:8, _Flags:24, Size:32, SampleCount:32>>, Mp4Track) ->
  fill_stsz(Size, SampleCount, Mp4Track).
  
read_stsz(_, 0, #mp4_track{sample_sizes = SampleSizes} = Mp4Track) ->
  Mp4Track#mp4_track{sample_sizes = lists:reverse(SampleSizes)};
  
read_stsz(<<Size:32, Rest/binary>>, Count, #mp4_track{sample_sizes = SampleSizes} = Mp4Track) ->
  read_stsz(Rest, Count - 1, Mp4Track#mp4_track{sample_sizes = [Size | SampleSizes]}).

fill_stsz(_, 0, Mp4Track) ->
  Mp4Track; % no need to lists:reverse, because all elements are equal

fill_stsz(Size, Count, #mp4_track{sample_sizes = SampleSizes} = Mp4Track) ->
  fill_stsz(Size, Count - 1, Mp4Track#mp4_track{sample_sizes = [Size | SampleSizes]}).


  

%%%%%%%%%%%%%%%%%%% STTS %%%%%%%%%%%%%%%%%%%
% Sample durations (dts delta between neigbour frames)
%%
stts(<<0:8, _Flags:3/binary, EntryCount:32, Rest/binary>>, Mp4Track) ->
  read_stts(Rest, EntryCount, Mp4Track, 0).

read_stts(_, 0, #mp4_track{sample_dts = DTSList} = Mp4Track, _) ->
  Mp4Track#mp4_track{sample_dts = lists:reverse(DTSList)};
  
read_stts(<<SampleCount:32, SampleDuration:32, Rest/binary>>, EntryCount, Mp4Track, DTS) ->
  Mp4Track1 = set_stts(SampleCount, SampleDuration, DTS, Mp4Track),
  read_stts(Rest, EntryCount - 1, Mp4Track1, DTS + SampleCount*SampleDuration).

set_stts(0, _Duration, _DTS, Mp4Track) ->
  Mp4Track;

set_stts(SampleCount, Duration, DTS, #mp4_track{sample_dts = DTSList} = Mp4Track) ->
  set_stts(SampleCount - 1, Duration, DTS+Duration, Mp4Track#mp4_track{sample_dts = [DTS|DTSList]}).



%%%%%%%%%%%%%%%%%%%%% STSS atom %%%%%%%%%%%%%%%%%%%
% List of keyframes
%
stss(<<0:8, _Flags:3/binary, SampleCount:32, Samples/binary>>, Mp4Track) ->
  read_stss(Samples, SampleCount, Mp4Track).

read_stss(_, 0, #mp4_track{keyframes = Keyframes} = Mp4Track) ->
  Mp4Track#mp4_track{keyframes = lists:reverse(Keyframes)};

read_stss(<<Sample:32, Rest/binary>>, EntryCount, #mp4_track{keyframes = Keyframes} = Mp4Track) ->
  read_stss(Rest, EntryCount - 1, Mp4Track#mp4_track{keyframes = [Sample-1|Keyframes]}).




%%%%%%%%%%%%%%%%%%%%%% CTTS atom  %%%%%%%%%%%%%%%%%%%%%%%
% list of B-Frames offsets
%%
ctts(<<0:32, Count:32, CTTS/binary>>, Mp4Track) ->
  read_ctts(CTTS, Count, Mp4Track).

read_ctts(_, 0, #mp4_track{sample_composition = Compositions} = Mp4Track) ->
  Mp4Track#mp4_track{sample_composition = lists:reverse(Compositions)};

read_ctts(<<Count:32, Offset:32, Rest/binary>>, EntryCount, Mp4Track) ->
  read_ctts(Rest, EntryCount - 1, set_ctts(Count, Offset, Mp4Track)).


set_ctts(0, _Offset, Mp4Track) ->
  Mp4Track;

set_ctts(Count, Offset, #mp4_track{sample_composition = Compositions} = Mp4Track) ->
  set_ctts(Count - 1, Offset, Mp4Track#mp4_track{sample_composition = [Offset | Compositions]}).

  
%%%%%%%%%%%%%%%%%%%%% STSC %%%%%%%%%%%%%%%%%%%%%%%
% Samples per chunk
%%
stsc(<<0:8, _Flags:3/binary, EntryCount:32, Rest/binary>>, Mp4Track) ->
  read_stsc(Rest, EntryCount, Mp4Track).


read_stsc(_, 0, #mp4_track{chunk_sizes = ChunkSizes} = Mp4Track) ->
  Mp4Track#mp4_track{chunk_sizes = lists:reverse(ChunkSizes)};

read_stsc(<<ChunkId:32, SamplesPerChunk:32, _SampleId:32, Rest/binary>>, EntryCount, #mp4_track{chunk_sizes = ChunkSizes} = Mp4Track) ->
  read_stsc(Rest, EntryCount - 1, Mp4Track#mp4_track{chunk_sizes = [{ChunkId - 1, SamplesPerChunk}|ChunkSizes]}).




%%%%%%%%%%%%%%%%%%%%%% STCO/CO64 atom %%%%%%%%%%%%%%%%%%%%
% sample table chunk offset
%%
stco(<<0:8, _Flags:3/binary, OffsetCount:32, Offsets/binary>>, Mp4Track) ->
  read_stco(Offsets, OffsetCount, Mp4Track).

read_stco(_, 0, #mp4_track{chunk_offsets = ChunkOffsets} = Mp4Track) ->
  Mp4Track#mp4_track{chunk_offsets = lists:reverse(ChunkOffsets)};

read_stco(<<Offset:32, Rest/binary>>, OffsetCount, #mp4_track{chunk_offsets = ChunkOffsets} = Mp4Track) ->
  read_stco(Rest, OffsetCount - 1, Mp4Track#mp4_track{chunk_offsets = [Offset | ChunkOffsets]}).


co64(<<0:8, _Flags:3/binary, OffsetCount:32, Offsets/binary>>, Mp4Track) ->
  ?D({co64,OffsetCount}),
  read_co64(Offsets, OffsetCount, Mp4Track).

read_co64(<<>>, 0, #mp4_track{chunk_offsets = ChunkOffsets} = Mp4Track) ->
  Mp4Track#mp4_track{chunk_offsets = lists:reverse(ChunkOffsets)};

read_co64(<<Offset:64, Rest/binary>>, OffsetCount, #mp4_track{chunk_offsets = ChunkOffsets} = Mp4Track) ->
  read_co64(Rest, OffsetCount - 1, Mp4Track#mp4_track{chunk_offsets = [Offset | ChunkOffsets]}).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



unpack_samples_in_chunk(#mp4_track{chunk_offsets = Offsets, chunk_sizes = ChunkSizes} = Mp4Track) ->
  ChunkCount = length(Offsets),
  Mp4Track#mp4_track{chunk_sizes = unpack_samples_in_chunk(ChunkSizes, ChunkCount, [])}.


unpack_samples_in_chunk(_, 0, ChunkSizes) ->
  lists:reverse(ChunkSizes);

unpack_samples_in_chunk([{FirstChunk, SamplesInChunk}], ChunkCount, ChunkSizes) ->
  unpack_samples_in_chunk([{FirstChunk, SamplesInChunk}], ChunkCount - 1, [SamplesInChunk | ChunkSizes]);

unpack_samples_in_chunk([{FirstChunk, _SamplesInChunk}, {FirstChunk, NextSamples} | Rest], ChunkCount, ChunkSizes) ->
  unpack_samples_in_chunk([{FirstChunk, NextSamples} | Rest], ChunkCount, ChunkSizes);
  
unpack_samples_in_chunk([{FirstChunk, SamplesInChunk}, {NextFirstChunk, NextSamples} | Rest], ChunkCount, ChunkSizes) ->
  unpack_samples_in_chunk([{FirstChunk + 1, SamplesInChunk}, {NextFirstChunk, NextSamples} | Rest], ChunkCount - 1, [SamplesInChunk | ChunkSizes]).
  

unpack_sample_offsets(#mp4_track{chunk_offsets = Offsets, chunk_sizes = ChunkSizes, sample_sizes = SampleSizes} = Mp4Track) ->
  SampleOffsets = unpack_sample_offsets(Offsets, ChunkSizes, SampleSizes, []),
  Mp4Track#mp4_track{sample_offsets = SampleOffsets}.


unpack_sample_offsets(_, _, [], Offsets) ->
  lists:reverse(Offsets);

unpack_sample_offsets([_Offset|ChunkOffsets], [0|ChunkSizes], SampleSizes, Offsets) ->
  unpack_sample_offsets(ChunkOffsets, ChunkSizes, SampleSizes, Offsets);
  
unpack_sample_offsets([Offset|ChunkOffsets], [Count|ChunkSizes], [Size|SampleSizes], Offsets) ->
  unpack_sample_offsets([Offset+Size|ChunkOffsets], [Count-1|ChunkSizes], SampleSizes, [Offset|Offsets]).
  
  
unpack_keyframes(#mp4_track{keyframes = Keyframes, sample_sizes = Sizes} = Track) ->
  Track#mp4_track{keyframes = unpack_keyframes(Keyframes, length(Sizes), 0, [])}.

unpack_keyframes([], 0, _Id, Flags) ->
  lists:reverse(Flags);

unpack_keyframes([], Count, Id, Flags) ->
  unpack_keyframes([], Count - 1, Id+1, [false|Flags]);
  
unpack_keyframes([Id | Keyframes], Count, Id, Flags) ->
  unpack_keyframes(Keyframes, Count - 1, Id+1, [true|Flags]);

unpack_keyframes([FrameId | _] = Keyframes, Count, Id, Flags) when FrameId > Id ->
  unpack_keyframes(Keyframes, Count - 1, Id+1, [false|Flags]).
  
unpack_compositions(#mp4_track{sample_dts = Timestamps, sample_composition = Composition} = Track) ->
  Track#mp4_track{sample_composition = unpack_compositions(Timestamps, Composition, [])}.

unpack_compositions([], _, PTS) ->
  lists:reverse(PTS);
  
unpack_compositions([_DTS|Timestamps], [], PTS) ->
  unpack_compositions(Timestamps, [], [0|PTS]);

unpack_compositions([_DTS|Timestamps], [CTime|Composition], PTS) ->
  unpack_compositions(Timestamps, Composition, [CTime|PTS]).
  

unpack_track(#mp4_track{} = Mp4Track) ->
  Track1 = unpack_samples_in_chunk(Mp4Track),
  Track2 = unpack_sample_offsets(Track1),
  Track3 = unpack_keyframes(Track2),
  unpack_compositions(Track3).
  
  
  
fill_track(Mp4Track) ->
  Track = unpack_track(Mp4Track),
  Frames = <<>>,
  
  #mp4_track{
    sample_sizes = SampleSizes,
    sample_dts = Timestamps,
    sample_offsets = Offsets,
    sample_composition = Compositions,
    keyframes = Keyframes,
    timescale = Timescale
  } = Track,
  {Filled, MaxDTS} = fill_track(Frames, SampleSizes, Offsets, Keyframes, Timestamps, Compositions, Timescale, 0, 0),
  {Filled, MaxDTS}.

fill_track(Frames, [], [], [], [], [], _, _, DTS) ->
  {Frames, DTS};

fill_track(Frames, [Size|SampleSizes], [Offset|Offsets], [Keyframe|Keyframes], [DTS|Timestamps], [CTime|Compositions], Timescale, Id, _) ->
  FDTS = DTS*1000/Timescale,
  FPTS = (DTS+CTime)*1000/Timescale,
  FKeyframe = case Keyframe of
    true -> 1;
    false -> 0
  end,
  fill_track(<<Frames/binary, FKeyframe:1, Size:63, Offset:64, FDTS:64/float, FPTS:64/float>>,
             SampleSizes, Offsets, Keyframes, Timestamps, Compositions, Timescale, Id+1, FDTS).


prepare_track_for_index([], Index, _Id, _Num) ->
  lists:reverse(Index);

prepare_track_for_index(<<>>, Index, _Id, _Num) ->
  lists:reverse(Index);

prepare_track_for_index([#mp4_frame{codec = srt, dts = DTS}|Frames], Index, Id, TrackId) ->
  Keyframe = 0,
  prepare_track_for_index(Frames, [{{DTS, TrackId, Keyframe}, Id}|Index], Id+1, TrackId);
  
prepare_track_for_index(<<Keyframe:1, _Size:63, _Offset:64, DTS:64/float, _PTS:64/float, Frames/binary>>, Index, Id, TrackId) ->
  prepare_track_for_index(Frames, [{{DTS, TrackId, Keyframe}, Id}|Index], Id+1, TrackId).
  
prepare_tracks_for_index(Tracks) ->
  Indexes = lists:foldl(fun(#mp4_track{frames = Frames}, IndexTracks) ->
    Num = length(IndexTracks) + 1,
    T = prepare_track_for_index(Frames, [], 0, Num),
    [T|IndexTracks]
  end, [], Tracks),
  lists:reverse(Indexes).


build_index(Tracks) when is_list(Tracks) ->
  Indexes = prepare_tracks_for_index(Tracks),
  T = lists:foldl(fun(Track, MergedTracks) -> lists:ukeymerge(1, Track, MergedTracks) end, [], Indexes),
  lists:foldl(fun({{_DTS, Track, K}, N}, Bin) -> <<Bin/binary, Track, K:1, N:23>> end, <<>>, T);

build_index(Tracks) when is_tuple(Tracks) ->
  build_index(tuple_to_list(Tracks)).

  
%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").

fill_track_test() ->
  ?assertEqual({<<1:1, 300:63, 0:64, 0.0:64/float, 0.0:64/float, 0:1, 10:63, 300:64, 25.0:64/float, 25.0:64/float>>, 25.0},
  fill_track(<<>>, [300, 10], [0,300], [true,false], [0.0,25.0], [0.0,0.0],1000, 0, 0)).

% prepare_index_tracks_test() ->
%   ?assertEqual([[{{0,1},0},{{25,1},1},{{50,1},2}], [{{0,2},0},{{30,2},1},{{45,2},2}]], prepare_tracks_for_index(test_tracks())).
% 
% test_tracks() ->
%   [#mp4_track{frames = [#mp4_frame{dts = 0}, #mp4_frame{dts = 25}, #mp4_frame{dts = 50}]}, 
%    #mp4_track{frames = [#mp4_frame{dts = 0}, #mp4_frame{dts = 30}, #mp4_frame{dts = 45}]}].
%   
% build_index_test() ->
%   ?assertEqual(<<1, 0:24, 2, 0:24, 1, 1:24, 2, 1:24, 2, 2:24, 1, 2:24>>, build_index(test_tracks())).

mp4_desc_tag_with_length_test() ->
  ?assertEqual({3, <<0,2,0,4,13,64,21,0,0,0,0,0,100,239,0,0,0,0,6,1,2>>, <<>>}, mp4_read_tag(<<3,21,0,2,0,4,13,64,21,0,0,0,0,0,100,239,0,0,0,0,6,1,2>>)),
  ?assertEqual({4, <<64,21,0,0,0,0,0,100,239,0,0,0,0>>, <<6,1,2>>}, mp4_read_tag(<<4,13,64,21,0,0,0,0,0,100,239,0,0,0,0,6,1,2>>)).
  

unpack_chunk_samples_test() ->
  Mp4Track = #mp4_track{chunk_offsets = [1,2,3,4,5,6], chunk_sizes = [{0, 4}, {3, 5}]},
  Mp4Track1 = unpack_samples_in_chunk(Mp4Track),
  Mp4Track2 = Mp4Track1#mp4_track{chunk_sizes = [4,4,4,5,5,5]},
  ?assertEqual(Mp4Track2, Mp4Track1).

unpack_sample_offsets_test() ->
  Mp4Track = #mp4_track{chunk_offsets = [10,20,30], chunk_sizes = [2,2,2], sample_sizes = [1,1,1,1,1,1]},
  Mp4Track1 = unpack_sample_offsets(Mp4Track),
  Mp4Track2 = Mp4Track1#mp4_track{sample_offsets = [10,11,20,21,30,31]},
  ?assertEqual(Mp4Track2, Mp4Track1).
  

esds_tag1_test() ->
  ?assertEqual(#esds{object_type = aac, stream_type = 21, buffer_size = 0, max_bitrate = 25839, avg_bitrate = 0}, config_from_esds_tag(<<3,21,0,2,0,4,13,64,21,0,0,0,0,0,100,239,0,0,0,0,6,1,2>>)).

esds_tag2_test() ->
  ?assertEqual(#esds{object_type = aac, stream_type = 21, buffer_size = 428, max_bitrate = 139608, avg_bitrate = 101944, specific = <<18,16>>}, config_from_esds_tag(<<3,25,0,0,0,4,17,64,21,0,1,172,0,2,33,88,0,1,142,56,5,2,18,16,6,1,2>>)).

get_coverart_validMeta_test () ->
  {ok,Dev} = file:open("test/files/tag_coverart.mp4",[read,raw,binary]),
  Reader = {file,Dev},
  Metadata = get_coverart(Reader),
  ?assertMatch(<<_:6/binary,"JFIF",_/binary>>, Metadata).

get_coverart_sizeMeta_test () ->
  {ok,Dev} = file:open("test/files/tag_coverart.mp4",[read,raw,binary]),
  Reader = {file,Dev},
  Metadata = get_coverart(Reader),
  ?assertEqual(114121,size(Metadata)).

get_coverart_unvalid_test () ->
  {ok,Dev} = file:open("test/files/without_coverart.mp4",[read,raw,binary]),
  Reader = {file,Dev},
  Metadata = get_coverart(Reader),
  ?assertEqual(<<>>,Metadata).


