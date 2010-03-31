%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @author     Takuma Mori <mori@sgra.co.jp> [http://www.sgra.co.jp/en/], SGRA Corporation
%%% @copyright  2008 Takuma Mori, 2009 Max Lapshin
%%% @doc        MP4 decoding module, rewritten from RubyIzumi
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% Copyright (c) 2009 Max Lapshin
%%%    This program is free software: you can redistribute it and/or modify
%%%    it under the terms of the GNU Affero General Public License as
%%%    published by the Free Software Foundation, either version 3 of the
%%%    License, or any later version.
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

-module(mp4).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../../include/ems.hrl").
-include("../../include/mp4.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("erlyvideo/include/media_info.hrl").

-export([init/1, read_frame/2, metadata/1, codec_config/2, seek/2, first/1]).
-export([ftyp/2, moov/2, mvhd/2, trak/2, tkhd/2, mdia/2, mdhd/2, stbl/2, stsd/2, esds/2, avcC/2,
btrt/2, stsz/2, stts/2, stsc/2, stss/2, stco/2, smhd/2, minf/2, ctts/2]).


-export([mp4_desc_length/1, build_index_table/1]).

-behaviour(gen_format).

codec_config(video, #media_info{video_codec = VideoCodec} = MediaInfo) ->
  Config = decoder_config(video, MediaInfo),
  % ?D({"Video config", Config}),
  #video_frame{       
   	type          = video,
   	decoder_config = true,
		dts           = 0,
		pts           = 0,
		body          = Config,
		frame_type    = keyframe,
		codec_id      = VideoCodec
	};

codec_config(audio, #media_info{audio_codec = AudioCodec} = MediaInfo) ->
  Config = decoder_config(audio, MediaInfo),
  % ?D({"Audio config", aac:decode_config(Config)}),
  #video_frame{       
   	type          = audio,
   	decoder_config = true,
		dts           = 0,
		pts           = 0,
		body          = Config,
	  codec_id	= AudioCodec,
	  sound_type	  = stereo,
	  sound_size	  = bit16,
	  sound_rate	  = rate44
	}.


first(#media_info{frames = Frames}) ->
  ets:first(Frames).


lookup_frame(video, #media_info{video_track = FrameTable}, Id) ->
  [Frame] = ets:lookup(FrameTable, Id),
  Frame;

lookup_frame(audio, #media_info{audio_track = FrameTable}, Id) ->
  [Frame] = ets:lookup(FrameTable, Id),
  Frame.

read_frame(#media_info{frames = Frames} = MediaInfo, Id) ->
  [{Id, Type, FrameId}] = ets:lookup(Frames, Id),
  Frame = lookup_frame(Type, MediaInfo, FrameId),
  #mp4_frame{offset = Offset, size = Size} = Frame,
  Next = case ets:next(Frames, Id) of
    '$end_of_table' -> done;
    NextID -> NextID
  end,
  
	case read_data(MediaInfo, Offset, Size) of
		{ok, Data, _} -> {video_frame(Type, Frame, Data), Next};
    eof -> done;
    {error, Reason} -> {error, Reason}
  end.
  

read_data(#media_info{device = IoDev} = MediaInfo, Offset, Size) ->
  case file:pread(IoDev, Offset, Size) of
    {ok, Data} ->
      {ok, Data, MediaInfo};
    Else -> Else
  end.
  
seek(#media_info{video_track = FrameTable, frames = Frames}, Timestamp) ->
  Ids = ets:select(FrameTable, ets:fun2ms(fun(#mp4_frame{id = Id, dts = FrameTimestamp, keyframe = true} = _Frame) when FrameTimestamp =< Timestamp ->
    {Id, FrameTimestamp}
  end)),
  case lists:reverse(Ids) of
    [{VideoID, NewTimestamp} | _] ->
      [Item] = ets:select(Frames, ets:fun2ms(fun({ID, video, VideoFrameID}) when VideoID == VideoFrameID -> 
        {ID, NewTimestamp}
      end)),
      Item;
    _ -> undefined
  end.
  

video_frame(video, #mp4_frame{dts = DTS, keyframe = Keyframe, pts = PTS}, Data) ->
  #video_frame{
   	type          = video,
		dts           = DTS,
		pts           = PTS,
		body          = Data,
		frame_type    = case Keyframe of
		  true ->	keyframe;
		  _ -> frame
	  end,
		codec_id      = h264
  };  

video_frame(audio, #mp4_frame{dts = DTS}, Data) ->
  #video_frame{       
   	type          = audio,
		dts           = DTS,
		pts           = DTS,
  	body          = Data,
	  codec_id	    = aac,
	  sound_type	  = stereo,
	  sound_size	  = bit16,
	  sound_rate	  = rate44
  }.



init(#media_info{header = undefined} = MediaInfo) -> 
  Info1 = MediaInfo#media_info{header = #mp4_header{}},
  % eprof:start(),
  % eprof:start_profiling([self()]),
  {Time, {ok, Info2}} = timer:tc(?MODULE, init, [Info1]),
  {Time2, Info3} = timer:tc(?MODULE, build_index_table, [Info2]),
  ?D({"Time to parse moov and build index", round(Time/1000), round(Time2/1000)}),
  % eprof:total_analyse(),
  % eprof:stop(),
  {ok, Info3};

init(MediaInfo) -> 
  init(MediaInfo, 0).

init(#media_info{device = Device} = MediaInfo, Pos) -> 
  case next_atom(MediaInfo, Pos) of
    eof -> {ok, MediaInfo};
    {error, Reason} -> {error, Reason};
    {atom, mdat, Offset, Length} ->
      init(MediaInfo, Offset + Length);
    {atom, _AtomName, Offset, 0} -> 
      init(MediaInfo, Offset);
    {atom, AtomName, Offset, Length} -> 
      ?D({"Root atom", AtomName, Length}),
      {ok, AtomData} = file:pread(Device, Offset, Length),
      NewInfo = case ems:respond_to(?MODULE, AtomName, 2) of
        true -> ?MODULE:AtomName(AtomData, MediaInfo);
        false -> ?D({"Unknown atom", AtomName}), MediaInfo
      end,
      init(NewInfo, Offset + Length)
  end.

next_atom(#media_info{device = Device}, Pos) ->
  case file:pread(Device, Pos, 8) of
    {ok, <<AtomLength:32, AtomName/binary>>} when AtomLength >= 8 ->
      % ?D({"Atom", binary_to_atom(AtomName, latin1), Pos, AtomLength}),
      {atom, binary_to_atom(AtomName, utf8), Pos + 8, AtomLength - 8};
    Else -> Else
  end.


build_index_table(#media_info{video_track = Video, audio_track = Audio} = MediaInfo) ->
  Index = ets:new(index, [ordered_set]),
  build_index_table(Video, ets:first(Video), Audio, ets:first(Audio), Index, 0),
  MediaInfo#media_info{frames = Index}.


build_index_table(_Video, '$end_of_table', _Audio, '$end_of_table', Index, _ID) ->
  Index;

build_index_table(Video, '$end_of_table', Audio, AudioID, Index, ID) ->
  [AFrame] = ets:lookup(Audio, AudioID),
  ets:insert(Index, {ID, audio, AFrame#mp4_frame.id}),
  build_index_table(Video, '$end_of_table', Audio, ets:next(Audio, AudioID), Index, ID+1);

build_index_table(Video, VideoID, Audio, '$end_of_table', Index, ID) ->
  [VFrame] = ets:lookup(Video, VideoID),
  ets:insert(Index, {ID, video, VFrame#mp4_frame.id}),
  build_index_table(Video, ets:next(Video, VideoID), Audio, '$end_of_table', Index, ID+1);
  

build_index_table(Video, VideoID, Audio, AudioID, Index, ID) ->
  [VFrame] = ets:lookup(Video, VideoID),
  [AFrame] = ets:lookup(Audio, AudioID),
  case {VFrame#mp4_frame.dts, AFrame#mp4_frame.dts} of
    {VDTS, ADTS} when VDTS =< ADTS ->
      ets:insert(Index, {ID, video, VFrame#mp4_frame.id}),
      build_index_table(Video, ets:next(Video, VideoID), Audio, AudioID, Index, ID+1);
    _ ->
      ets:insert(Index, {ID, audio, AFrame#mp4_frame.id}),
      build_index_table(Video, VideoID, Audio, ets:next(Audio, AudioID), Index, ID+1)
  end.
      
  
metadata(#media_info{width = Width, height = Height, seconds = Duration}) -> 
  [{width, Width}, 
   {height, Height}, 
   {duration, Duration/1000}].
  
  
decoder_config(video, #media_info{video_config = DecoderConfig}) -> DecoderConfig;
decoder_config(audio, #media_info{audio_config = DecoderConfig}) -> DecoderConfig.



  
parse_atom(<<>>, Mp4Parser) ->
  Mp4Parser;
  
parse_atom(Atom, _) when size(Atom) < 4 ->
  {error, "Invalid atom"};
  
parse_atom(<<AllAtomLength:32, BinaryAtomName:4/binary, AtomRest/binary>>, Mp4Parser) when (size(AtomRest) >= AllAtomLength - 8) ->
  AtomLength = AllAtomLength - 8,
  <<Atom:AtomLength/binary, Rest/binary>> = AtomRest,
  AtomName = binary_to_atom(BinaryAtomName, utf8),
  NewMp4Parser = case ems:respond_to(?MODULE, AtomName, 2) of
    true -> ?MODULE:AtomName(Atom, Mp4Parser);
    false -> ?D({"Unknown atom", AtomName}), Mp4Parser
  end,
  parse_atom(Rest, NewMp4Parser);
  
parse_atom(<<AllAtomLength:32, BinaryAtomName:4/binary, _Rest/binary>>, Mp4Parser) ->
  ?D({"Invalid atom", AllAtomLength, binary_to_atom(BinaryAtomName, utf8), size(_Rest)}),
  Mp4Parser;

parse_atom(<<0:32>>, Mp4Parser) ->
  ?D("NULL atom"),
  Mp4Parser.

  
% FTYP atom
ftyp(<<_Major:4/binary, _Minor:4/binary, _CompatibleBrands/binary>>, MediaInfo) ->
  ?D({"File", _Major, _Minor, ftyp(_CompatibleBrands, [])}),
  % NewParser = Mp4Parser#mp4_header{file_type = binary_to_list(Major), file_types = decode_atom(ftyp, CompatibleBrands, [])},
  MediaInfo;

ftyp(<<>>, BrandList) when is_list(BrandList) ->
  lists:reverse(BrandList);

ftyp(<<Brand:4/binary, CompatibleBrands/binary>>, BrandList) ->
  ftyp(CompatibleBrands, [Brand|BrandList]).
  
% Movie box
moov(Atom, MediaInfo) ->
  parse_atom(Atom, MediaInfo).

% MVHD atom
mvhd(<<0:8, _Flags:3/binary, _CTime:32, _MTime:32, TimeScale:32,
                    Duration:32, _Rest/binary>>, #media_info{} = MediaInfo) ->
  MediaInfo#media_info{timescale = TimeScale, duration = Duration, seconds = Duration/TimeScale}.

% Track box
trak(<<>>, MediaInfo) ->
  MediaInfo;
  
trak(Atom, #media_info{} = MediaInfo) ->
  Track = parse_atom(Atom, #mp4_track{}),
  fill_track_info(MediaInfo, Track).
  

% Track header
tkhd(<<0:8, _Flags:3/binary, _CTime:32, _MTime:32,
                    TrackID:32, _Reserved1:4/binary, 
                    Duration:32, _Reserved2:8/binary,
                    _Layer:16, _AlternateGroup:2/binary,
                    _Volume:2/binary, _Reserved3:2/binary,
                    _Matrix:36/binary, _TrackWidth:4/binary, _TrackHeigth:4/binary>>, Mp4Track) ->
  Mp4Track#mp4_track{track_id = TrackID, duration = Duration}.

% Media box
mdia(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

% Media header
mdhd(<<0:8, _Flags:24, _Ctime:32, 
                  _Mtime:32, TimeScale:32, Duration:32,
                  _Language:2/binary, _Quality:16>>, #mp4_track{} = Mp4Track) ->
  % ?D({"Timescale:", Duration, extract_language(_Language)}),
  _DecodedLanguate = extract_language(_Language),
  Mp4Track#mp4_track{timescale = TimeScale, duration = Duration};

mdhd(<<1:8, _Flags:24, _Ctime:64, 
                     _Mtime:64, TimeScale:32, Duration:64, 
                     _Language:2/binary, _Quality:16>>, Mp4Track) ->
  % ?D({"Timescale:", Duration, extract_language(_Language)}),
  Mp4Track#mp4_track{timescale = TimeScale, duration = Duration}.
  
% SMHD atom
smhd(<<0:8, _Flags:3/binary, 0:16/big-signed-integer, _Reserve:2/binary>>, Mp4Track) ->
  Mp4Track;

smhd(<<0:8, _Flags:3/binary, _Balance:16/big-signed-integer, _Reserve:2/binary>>, Mp4Track) ->
  Mp4Track.

% Media information
minf(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

% Sample table box
stbl(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

% Sample description
stsd(<<0:8, _Flags:3/binary, EntryCount:32, EntryData/binary>>, Mp4Track) ->
  stsd({EntryCount, EntryData}, Mp4Track);

stsd({0, _}, Mp4Track) ->
  Mp4Track;

stsd({_, <<>>}, Mp4Track) ->
  Mp4Track;
  
stsd({_EntryCount, <<_SampleDescriptionSize:32, 
                                  "mp4a", _Reserved:6/binary, _RefIndex:16, 
                                  _Unknown:8/binary, _ChannelsCount:32,
                                  _SampleSize:32, _SampleRate:32,
                                  Atom/binary>>}, Mp4Track) ->
  parse_atom(Atom, Mp4Track#mp4_track{data_format = aac});

stsd({_EntryCount, <<_SampleDescriptionSize:32, 
                                  "avc1", _Reserved:6/binary, _RefIndex:16, 
                                  _Unknown1:16/binary, 
                                  Width:16, Height:16,
                                  _HorizRes:32, _VertRes:32,
                                  _FrameCount:16, _CompressorName:32/binary,
                                  _Depth:16, _Predefined:16,
                                  _Unknown:4/binary,
                                  Atom/binary>>}, Mp4Track) ->
  % ?D({"Video size:", Width, Height}),
  parse_atom(Atom, Mp4Track#mp4_track{data_format = h264, width = Width, height = Height});

stsd({_EntryCount, <<_SampleDescriptionSize:32, 
                                  "s263", _Reserved:6/binary, _RefIndex:16, 
                                  _Unknown1:16/binary, 
                                  Width:16, Height:16,
                                  _HorizRes:32, _VertRes:32,
                                  _FrameCount:16, _CompressorName:32/binary,
                                  _Depth:16, _Predefined:16,
                                  _Unknown:4/binary,
                                  Atom/binary>>}, Mp4Track) ->
  % ?D({"Video size:", Width, Height}),
  parse_atom(Atom, Mp4Track#mp4_track{data_format = s263, width = Width, height = Height});

stsd({_EntryCount,   <<_SampleDescriptionSize:32, 
                                    "samr", _Reserved:2/binary, _RefIndex:16, 
                                    Atom/binary>> = AMR}, Mp4Track) ->
  ?D(AMR),
  parse_atom(Atom, Mp4Track#mp4_track{data_format = samr});



stsd({_EntryCount, <<SampleDescriptionSize:32, DataFormat:4/binary, 
                                 _Reserved:6/binary, _RefIndex:16, EntryData/binary>>}, Mp4Track) 
           when SampleDescriptionSize == size(EntryData) + 16 ->
  NewTrack = Mp4Track#mp4_track{data_format = binary_to_atom(DataFormat, utf8)},
  ?D({"Unknown sample description:", NewTrack#mp4_track.data_format, SampleDescriptionSize, size(EntryData), binary_to_list(EntryData)}),
  NewTrack.
  
% ESDS atom
esds(<<Version:8, _Flags:3/binary, DecoderConfig/binary>>, #mp4_track{data_format = aac} = Mp4Track) when Version == 0 ->
  ?D({"Extracted audio config", DecoderConfig}),
  Mp4Track#mp4_track{decoder_config = config_from_esds_tag(DecoderConfig)}.

% avcC atom
avcC(DecoderConfig, #mp4_track{} = Mp4Track) ->
  ?D({"Extracted video config", DecoderConfig, h264:unpack_config(DecoderConfig)}),
  Mp4Track#mp4_track{decoder_config = DecoderConfig}.

btrt(<<_BufferSize:32, _MaxBitRate:32, _AvgBitRate:32>>, #mp4_track{} = Mp4Track) ->
  Mp4Track.


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




%%%%%%%%%%%%%%%%%%%%%% STCO atom %%%%%%%%%%%%%%%%%%%%
% sample table chunk offset
%%
stco(<<0:8, _Flags:3/binary, OffsetCount:32, Offsets/binary>>, Mp4Track) ->
  read_stco(Offsets, OffsetCount, Mp4Track).

read_stco(_, 0, #mp4_track{chunk_offsets = ChunkOffsets} = Mp4Track) ->
  Mp4Track#mp4_track{chunk_offsets = lists:reverse(ChunkOffsets)};

read_stco(<<Offset:32, Rest/binary>>, OffsetCount, #mp4_track{chunk_offsets = ChunkOffsets} = Mp4Track) ->
  read_stco(Rest, OffsetCount - 1, Mp4Track#mp4_track{chunk_offsets = [Offset | ChunkOffsets]}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_language(<<L1:5, L2:5, L3:5, _:1>>) ->
  [L1+16#60, L2+16#60, L3+16#60].



fill_track_info(MediaInfo, #mp4_track{data_format = h264, decoder_config = DecoderConfig, width = Width, height = Height} = Track) ->
  % copy_track_info(MediaInfo#media_info{video_decoder_config = DecoderConfig, width = Width, height = Height, video}, Track);
  {Frames, MaxDTS} = fill_track(Track),
  _Seconds = case MediaInfo#media_info.seconds of
    undefined -> MaxDTS;
    S when S < MaxDTS -> MaxDTS;
    S -> S
  end,
  MediaInfo#media_info{video_config = DecoderConfig, width = Width, height = Height, video_track = Frames, seconds = MaxDTS};


fill_track_info(MediaInfo, #mp4_track{data_format = aac, decoder_config = DecoderConfig} = Track) ->
  % copy_track_info(MediaInfo#media_info{audio_decoder_config = DecoderConfig}, Track);
  {Frames, _MaxDTS} = fill_track(Track),
  % Seconds = case MediaInfo#media_info.seconds of
  %   undefined -> MaxDTS;
  %   S when S < MaxDTS -> MaxDTS;
  %   S -> S
  % end,
  MediaInfo#media_info{audio_config = DecoderConfig, audio_track = Frames};
  
fill_track_info(MediaInfo, #mp4_track{data_format = Unknown}) ->
  ?D({"Uknown data format", Unknown}),
  MediaInfo.


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
  Frames = ets:new(frames, [ordered_set, {keypos, #mp4_frame.id}]),
  
  #mp4_track{
    sample_sizes = SampleSizes,
    sample_dts = Timestamps,
    sample_offsets = Offsets,
    sample_composition = Compositions,
    keyframes = Keyframes,
    timescale = Timescale
  } = Track,
  MaxDTS = fill_track(SampleSizes, Offsets, Keyframes, Timestamps, Compositions, Timescale, Frames, 0, 0),
  {Frames, MaxDTS*1000/Timescale}.

fill_track([], [], [], [], [], _, _Frames, _, DTS) ->
  DTS;

fill_track([Size|SampleSizes], [Offset|Offsets], [Keyframe|Keyframes], [DTS|Timestamps], [PTS|Compositions], Timescale, Frames, Id, _) ->
  Frame = #mp4_frame{id = Id, dts = DTS*1000/Timescale, pts = (DTS+PTS)*1000/Timescale, size = Size, offset = Offset, keyframe = Keyframe},
  ets:insert(Frames, Frame),
  fill_track(SampleSizes, Offsets, Keyframes, Timestamps, Compositions, Timescale, Frames, Id+1, DTS).

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

%% FIXME: Code here must be relocated in some more generic place and way. 
%% Here goes not some esds tag, but IOD (Initial Object Description)
%% Look how to parse it at vlc/modules/demux/ts.c:2400
%%

config_from_esds_tag(Data) ->
  case mp4_read_tag(Data) of
    {?MP4ESDescrTag, <<_ID1:16, _Priority1, Description/binary>>, <<>>} ->
      config_from_esds_tag(Description);
    {?MP4DecConfigDescrTag, <<_ObjectType, _StreamType, _BufferSize:24, _MaxBitrate:32, _AvgBitrate:32>>, Rest} ->
      config_from_esds_tag(Rest);
    {?MP4DecConfigDescrTag, <<_:13/binary, Rest1/binary>>, Rest2} when size(Rest1) > 0 ->
      case config_from_esds_tag(Rest1) of
        undefined ->
          config_from_esds_tag(Rest2);
        Config ->
          Config
      end;
    {?MP4DecSpecificDescrTag, Config, _} ->
      Config;
    {?MP4Unknown6Tag, _Body, Rest} ->
      config_from_esds_tag(Rest);
    {_Tag, _Data, Rest} ->
      ?D({"Unknown esds tag. Send this line to max@maxidoors.ru: ", _Tag, _Data}),
      config_from_esds_tag(Rest);
    undefined ->
      undefined
  end.

  
%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").

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
  

esds_tag_test() ->
  ?assertEqual(undefined, config_from_esds_tag(<<3,21,0,2,0,4,13,64,21,0,0,0,0,0,100,239,0,0,0,0,6,1,2>>)),
  ?assertEqual(<<18,16>>, config_from_esds_tag(<<3,25,0,0,0,4,17,64,21,0,1,172,0,2,33,88,0,1,142,56,5,2,18,16,6,1,2>>)).
