%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @author     Takuma Mori <mori@sgra.co.jp> [http://www.sgra.co.jp/en/], SGRA Corporation
%%% @copyright  2008 Takuma Mori, 2009 Max Lapshin
%%% @doc        MP4 decoding module, rewritten from RubyIzumi
%%% @reference  See <a href="http://github.com/maxlapshin/erlyvideo" target="_top">http://github.com/maxlapshin/erlyvideo</a> for more information
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
-author('max@maxidoors.ru').
-include("../../include/ems.hrl").
-include("../../include/mp4.hrl").
-include("../../include/media_info.hrl").

-export([init/1, read_frame/2, metadata/1, codec_config/2]).
-behaviour(gen_format).

codec_config(video, MediaInfo) ->
  Config = decoder_config(video, MediaInfo),
  % ?D({"Video config", Config}),
  #video_frame{       
   	type          = ?FLV_TAG_TYPE_VIDEO,
   	decoder_config = true,
		timestamp      = 0,
		body          = Config,
		frame_type    = ?FLV_VIDEO_FRAME_TYPE_KEYFRAME,
		codec_id      = ?FLV_VIDEO_CODEC_AVC
	};

codec_config(audio, MediaInfo) ->
  Config = decoder_config(audio, MediaInfo),
  % ?D({"Audio config", Config}),
  #video_frame{       
   	type          = ?FLV_TAG_TYPE_AUDIO,
   	decoder_config = true,
		timestamp     = 0,
		body          = Config,
	  sound_format	= ?FLV_AUDIO_FORMAT_AAC,
	  sound_type	  = ?FLV_AUDIO_TYPE_STEREO,
	  sound_size	  = ?FLV_AUDIO_SIZE_16BIT,
	  sound_rate	  = ?FLV_AUDIO_RATE_44
	}.


read_frame(#media_info{frames = FrameTable} = MediaInfo, Key) ->
  [Frame] = ets:lookup(FrameTable, Key),
  #file_frame{offset = Offset, size = Size} = Frame,
	case read_data(MediaInfo, Offset, Size) of
		{ok, Data, _} -> video_frame(Frame, Data);
    eof -> done;
    {error, Reason} -> {error, Reason}
  end.
  

read_data(#media_info{device = IoDev} = MediaInfo, Offset, Size) ->
  case file:pread(IoDev, Offset, Size) of
    {ok, Data} ->
      {ok, Data, MediaInfo};
    Else -> Else
  end.

% read_data(#video_player{cache = Cache, cache_offset = CacheOffset} = Player, Offset, Size) when (CacheOffset =< Offset) and (Offset + Size - CacheOffset =< size(Cache)) ->
%   Seek = Offset - CacheOffset,
%   % ?D({"Cache hit", Offset, Size}),
%   <<_:Seek/binary, Data:Size/binary, Rest/binary>> = Cache,
%   {ok, Data, Player};
% 
% 

% read_data(#video_player{media_info = #media_info{device = IoDev}} = Player, Offset, Size) ->
%   CacheSize = 60000 + Size,
%   case file:pread(IoDev, Offset, CacheSize) of
%     {ok, CacheList} ->
%       Cache = iolist_to_binary(CacheList),
%       <<Data:Size/binary, _/binary>> = Cache,
%       {ok, Data, Player#video_player{cache_offset = Offset, cache = Cache}};
%     Else -> Else
%   end.
%   

video_frame(#file_frame{type = video, timestamp = Timestamp, keyframe = Keyframe}, Data) ->
  #video_frame{
   	type          = ?FLV_TAG_TYPE_VIDEO,
		timestamp     = Timestamp,
		body          = Data,
		frame_type    = case Keyframe of
		  true ->	?FLV_VIDEO_FRAME_TYPE_KEYFRAME;
		  _ -> ?FLV_VIDEO_FRAME_TYPEINTER_FRAME
	  end,
		codec_id      = ?FLV_VIDEO_CODEC_AVC
  };  

video_frame(#file_frame{type = audio, timestamp = Timestamp}, Data) ->
  #video_frame{       
   	type          = ?FLV_TAG_TYPE_AUDIO,
  	timestamp     = Timestamp,
  	body          = Data,
    sound_format	= ?FLV_AUDIO_FORMAT_AAC,
    sound_type	  = ?FLV_AUDIO_TYPE_STEREO,
    sound_size	  = ?FLV_AUDIO_SIZE_16BIT,
    sound_rate	  = ?FLV_AUDIO_RATE_44
  }.



init(#media_info{header = undefined} = MediaInfo) -> 
  init(MediaInfo#media_info{header = #mp4_header{}, frames = ets:new(frames, [ordered_set, {keypos, #file_frame.id}])});

init(MediaInfo) -> 
  init(MediaInfo, 0).

init(MediaInfo, Pos) -> 
  case next_atom(#media_info{device = Device} = MediaInfo, Pos) of
    {eof} -> {ok, MediaInfo};
    {error, Reason} -> {error, Reason};
    {atom, mdat, Offset, Length} ->
      init(MediaInfo, Offset + Length);
    {atom, AtomName, Offset, Length} -> 
      {ok, AtomData} = file:pread(Device, Offset, Length),
      NewInfo = decode_atom(AtomName, AtomData, MediaInfo),
      init(NewInfo, Offset + Length)
  end.
  
metadata(#media_info{width = Width, height = Height, seconds = Duration}) -> 
  [{width, Width}, 
   {height, Height}, 
   {duration, Duration},
   {videocodecid, ?FLV_VIDEO_CODEC_AVC},
   {audiosamplerate, ?FLV_AUDIO_RATE_44}].
  
  
decoder_config(video, #media_info{video_decoder_config = DecoderConfig}) -> DecoderConfig;
decoder_config(audio, #media_info{audio_decoder_config = DecoderConfig}) -> DecoderConfig.



  
parse_atom(<<>>, Mp4Parser) ->
  Mp4Parser;
  
parse_atom(Atom, _) when size(Atom) < 4 ->
  {error, "Invalid atom"};
  
parse_atom(<<AllAtomLength:32/big-integer, BinaryAtomName:4/binary, AtomRest/binary>>, Mp4Parser) when (size(AtomRest) >= AllAtomLength - 8) ->
  AtomLength = AllAtomLength - 8,
  <<Atom:AtomLength/binary, Rest/binary>> = AtomRest,
  AtomName = binary_to_atom(BinaryAtomName, utf8),
  NewMp4Parser = decode_atom(AtomName, Atom, Mp4Parser),
  parse_atom(Rest, NewMp4Parser);
  
parse_atom(<<AllAtomLength:32/big-integer, BinaryAtomName:4/binary, _Rest/binary>>, Mp4Parser) ->
  ?D({"Invalid atom", AllAtomLength, binary_to_atom(BinaryAtomName, utf8), size(_Rest)}),
  Mp4Parser;

parse_atom(<<0:32/big-integer>>, Mp4Parser) ->
  ?D("NULL atom"),
  Mp4Parser.

  
% FTYP atom
decode_atom(ftyp, <<_Major:4/binary, _Minor:4/binary, _CompatibleBrands/binary>>, MediaInfo) ->
  % NewParser = Mp4Parser#mp4_header{file_type = binary_to_list(Major), file_types = decode_atom(ftyp, CompatibleBrands, [])},
  MediaInfo;

decode_atom(ftyp, <<>>, BrandList) ->
  lists:reverse(BrandList);

decode_atom(ftyp, <<Brand:4/binary, CompatibleBrands/binary>>, BrandList) ->
  decode_atom(ftyp, CompatibleBrands, [binary_to_list(Brand)|BrandList]);
  
% MOOV atom
decode_atom(moov, Atom, MediaInfo) ->
  parse_atom(Atom, MediaInfo);

% MVHD atom
decode_atom(mvhd, <<0:8/integer, _Flags:3/binary, _CTime:32/big-integer, _MTime:32/big-integer, TimeScale:32/big-integer,
                    Duration:32/big-integer, _Rest/binary>>, #media_info{} = MediaInfo) ->
  MediaInfo#media_info{timescale = TimeScale, duration = Duration, seconds = Duration/TimeScale};

% TRAK atom
decode_atom(trak, <<>>, MediaInfo) ->
  MediaInfo;
  
decode_atom(trak, Atom, #media_info{} = MediaInfo) ->
  Track = decode_atom(trak, Atom, #mp4_track{}),
  fill_track_info(MediaInfo, Track);
  
decode_atom(trak, <<>>, #mp4_track{} = Mp4Track) ->
  Mp4Track;
decode_atom(trak, Atom, #mp4_track{} = Mp4Track) ->
  parse_atom(Atom, Mp4Track);

% TKHD atom
decode_atom(tkhd, <<0:8/integer, _Flags:3/binary, _CTime:32/big-integer, _MTime:32/big-integer,
                    TrackID:32/big-integer, _Reserved1:4/binary, 
                    Duration:32/big-integer, _Reserved2:8/binary,
                    _Layer:16/big-integer, _AlternateGroup:2/binary,
                    _Volume:2/binary, _Reserved3:2/binary,
                    _Matrix:36/binary, _TrackWidth:4/binary, _TrackHeigth:4/binary>>, Mp4Track) ->
  Mp4Track#mp4_track{track_id = TrackID, duration = Duration};

%MDIA atom
decode_atom(mdia, Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track);

% MDHD atom
decode_atom(mdhd,<<0:8/integer, _Flags:24/integer, _Ctime:32/big-integer, 
                  _Mtime:32/big-integer, TimeScale:32/big-integer, Duration:32/big-integer,
                  _Language:2/binary, _Quality:16/big-integer>>, #mp4_track{} = Mp4Track) ->
  % ?D({"Timescale:", Duration, extract_language(_Language)}),
  _DecodedLanguate = extract_language(_Language),
  Mp4Track#mp4_track{timescale = TimeScale, duration = Duration};

decode_atom(mdhd, <<1:8/integer, _Flags:24/integer, _Ctime:64/big-integer, 
                     _Mtime:64/big-integer, TimeScale:32/big-integer, Duration:64/big-integer, 
                     _Language:2/binary, _Quality:16/big-integer>>, Mp4Track) ->
  % ?D({"Timescale:", Duration, extract_language(_Language)}),
  Mp4Track#mp4_track{timescale = TimeScale, duration = Duration};
  
% SMHD atom
decode_atom(smhd, <<0:8/integer, _Flags:3/binary, 0:16/big-signed-integer, _Reserve:2/binary>>, Mp4Track) ->
  Mp4Track;

decode_atom(smhd, <<0:8/integer, _Flags:3/binary, _Balance:16/big-signed-integer, _Reserve:2/binary>>, Mp4Track) ->
  Mp4Track;

% MINF atom
decode_atom(minf, Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track);

% STBL atom
decode_atom(stbl, Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track);

% STSD atom
decode_atom(stsd, <<0:8/integer, _Flags:3/binary, EntryCount:32/big-integer, EntryData/binary>>, Mp4Track) ->
  decode_atom(stsd, {EntryCount, EntryData}, Mp4Track);

decode_atom(stsd, {0, _}, Mp4Track) ->
  Mp4Track;

decode_atom(stsd, {_, <<>>}, Mp4Track) ->
  Mp4Track;
  
decode_atom(stsd, {_EntryCount, <<_SampleDescriptionSize:32/big-integer, 
                                  "mp4a", _Reserved:6/binary, _RefIndex:16/big-integer, 
                                  _Unknown:8/binary, _ChannelsCount:32/big-integer,
                                  _SampleSize:32/big-integer, _SampleRate:32/big-integer,
                                  Atom/binary>>}, Mp4Track) ->
  parse_atom(Atom, Mp4Track#mp4_track{data_format = mp4a});

decode_atom(stsd, {_EntryCount, <<_SampleDescriptionSize:32/big-integer, 
                                  "avc1", _Reserved:6/binary, _RefIndex:16/big-integer, 
                                  _Unknown1:16/binary, 
                                  Width:16/big-integer, Height:16/big-integer,
                                  _HorizRes:32/big-integer, _VertRes:32/big-integer,
                                  _FrameCount:16/big-integer, _CompressorName:32/binary,
                                  _Depth:16/big-integer, _Predefined:16/big-integer,
                                  _Unknown:4/binary,
                                  Atom/binary>>}, Mp4Track) ->
  % ?D({"Video size:", Width, Height}),
  parse_atom(Atom, Mp4Track#mp4_track{data_format = avc1, width = Width, height = Height});



decode_atom(stsd, {_EntryCount, <<SampleDescriptionSize:32/big-integer, DataFormat:4/binary, 
                                 _Reserved:6/binary, _RefIndex:16/big-integer, EntryData/binary>>}, Mp4Track) 
           when SampleDescriptionSize == size(EntryData) + 16 ->
  NewTrack = Mp4Track#mp4_track{data_format = binary_to_atom(DataFormat, utf8)},
  ?D({"Unknown sample description:", NewTrack#mp4_track.data_format, SampleDescriptionSize, size(EntryData), binary_to_list(EntryData)}),
  NewTrack;
  
% ESDS atom
decode_atom(esds, <<Version:8/integer, _Flags:3/binary, DecoderConfig/binary>>, #mp4_track{data_format = mp4a} = Mp4Track) when Version == 0 ->
  % ?D({"Extracted audio config", DecoderConfig}),
  Mp4Track#mp4_track{decoder_config = esds_tag(DecoderConfig)};

% avcC atom
decode_atom(avcC, DecoderConfig, #mp4_track{} = Mp4Track) ->
  % ?D({"Extracted video config"}),
  parse_avc_decoder_config(DecoderConfig),
  Mp4Track#mp4_track{decoder_config = DecoderConfig};

decode_atom(btrt, <<_BufferSize:32/big-integer, _MaxBitRate:32/big-integer, _AvgBitRate:32/big-integer>>, #mp4_track{} = Mp4Track) ->
  Mp4Track;

% STSZ atom
decode_atom(stsz, <<_Version:8/integer, _Flags:24/integer, 0:32/big-integer, SampleCount:32/big-integer, SampleSizeData/binary>>, Mp4Track) ->
  Mp4Track#mp4_track{sample_sizes = decode_atom(stsz, {SampleCount, SampleSizeData}, [])};
  
decode_atom(stsz, {0, _}, SampleSizes) ->
  lists:reverse(SampleSizes);

decode_atom(stsz, {_, <<>>}, SampleSizes) ->
  lists:reverse(SampleSizes);

decode_atom(stsz, {SampleCount, <<Size:32/big-integer, Rest/binary>>}, SampleSizes) ->
  decode_atom(stsz, {SampleCount - 1, Rest}, [Size | SampleSizes]);

decode_atom(stsz, {_, <<Rest/binary>>}, SampleSizes) ->
  ?D("Invalid stsz atom"),
  decode_atom(stsz, {0, Rest}, SampleSizes);
  

% STTS atom
decode_atom(stts, <<0:8/integer, _Flags:3/binary, EntryCount:32/big-integer, Rest/binary>>, #mp4_track{} = Mp4Track) ->
  Table = decode_atom(stts, {EntryCount, Rest}, []),
  % ?D({"Sample time table", Table}),
  Mp4Track#mp4_track{sample_durations = lists:reverse(Table)};

decode_atom(stts, {0, _}, Table) ->
  Table;
  
decode_atom(stts, {_, <<>>}, Table) ->
  Table;
  
decode_atom(stts, {EntryCount, <<SampleCount:32/big-integer, SampleDuration:32/big-integer, Rest/binary>>}, Table) ->
  decode_atom(stts, {EntryCount - 1, Rest}, [{SampleCount, SampleDuration} | Table]);
  
% STSC atom
decode_atom(stsc, <<0:8/integer, _Flags:3/binary, EntryCount:32/big-integer, Rest/binary>>, #mp4_track{} = Mp4Track) ->
  Table = decode_atom(stsc, {EntryCount, Rest}, []),
  % ?D({"Sample chunk table", lists:reverse(Table)}),
  Mp4Track#mp4_track{chunk_table = lists:reverse(Table)};

decode_atom(stsc, {0, _}, Table) ->
  Table;
  
decode_atom(stsc, {_, <<>>}, Table) ->
  Table;
  
decode_atom(stsc, {EntryCount, <<FirstChunk:32/big-integer, SamplesPerChunk:32/big-integer, SampleID:32/big-integer, Rest/binary>>}, Table) ->
  decode_atom(stsc, {EntryCount - 1, Rest}, [{FirstChunk, SamplesPerChunk, SampleID} | Table]);

% STSS atom
% List of keyframes
decode_atom(stss, <<0:8/integer, _Flags:3/binary, SampleCount:32/big-integer, Samples/binary>>, #mp4_track{} = Mp4Track) when size(Samples) == SampleCount*4->
  NewTrack = Mp4Track#mp4_track{keyframes = decode_atom(stss, Samples, [])},
  case NewTrack#mp4_track.keyframes of
    [1 | _] ->
      NewTrack#mp4_track{key_offset = 1};
    _ ->
      NewTrack
  end;

decode_atom(stss, <<>>, SampleList) ->
  lists:reverse(SampleList);
  
decode_atom(stss, <<Sample:32/big-integer, Rest/binary>>, SampleList) ->
  decode_atom(stss, Rest, [Sample | SampleList]);

% STCO atom
% sample table chunk offset
decode_atom(stco, <<0:8/integer, _Flags:3/binary, OffsetCount:32/big-integer, Offsets/binary>>, #mp4_track{} = Mp4Track) when size(Offsets) == OffsetCount*4 ->
  Mp4Track#mp4_track{chunk_offsets = decode_atom(stco, Offsets, [])};

decode_atom(stco, <<>>, OffsetList) ->
  lists:reverse(OffsetList);
  
decode_atom(stco, <<Offset:32/big-integer, Rest/binary>>, OffsetList) ->
  decode_atom(stco, Rest, [Offset | OffsetList]);


% FALLBACK  
decode_atom(AtomName, _, Mp4Parser) ->
  ?D({"Unknown atom", AtomName}),
  Mp4Parser.

extract_language(<<L1:5/integer, L2:5/integer, L3:5/integer, _:1/integer>>) ->
  [L1+16#60, L2+16#60, L3+16#60].

next_atom(#media_info{device = Device}, Pos) ->
  case file:pread(Device, Pos, 4) of
    {ok, Data} ->
      <<AtomLength:32/big-integer>> = Data,
      if 
        AtomLength < 8 -> throw(invalid_atom_size_0);
        true ->
          {ok, AtomName} = file:pread(Device, Pos+4, 4),
          {atom, binary_to_atom(AtomName, utf8), Pos + 8, AtomLength - 8}
      end;
    eof -> 
      {eof};
    {error, Reason} -> 
      {error, Reason}           
  end.

%  Decoder config from one file:
%   <<1,77,0,50,255,225,0,22,103,77,0,50,154,118,4,1,141,8,0,0,31,72,0,5,220,4,120,193,137,192,1,0,4,104,238,60,128>>
%     1,77,0,50,255,225,0,22,103,77,0,50,154,118,4,1,141,8,0,0,31,72,0,5,220,4,120,193,137,192,1,0,4,104,238,60,128
% 1 — version
% 77 — profile
% 0 — Compat
% 50 — Level 5.0
% 2 bytes for slice length
% 1 SPS
% 22 bytes length of SPS:  103,77,0,50,154,118,4,1,141,8,0,0,31,72,0,5,220,4,120,193,137,192
% 1 PPS
% 4 bytes length of PPS: 104,238,60,128
% 
% Other decoder config
% <<1,66,192,21,253,225,0,23,103,66,192,21,146,68,15,4,127,88,8,128,0,1,244,0,0,97,161,71,139,23,80,1,0,4,104,206,50,200>>
parse_avc_decoder_config(<<_Version, _Profile, _ProfileCompat, _Level, _:6, _LengthSize:2, _:3, SPSCount:5, Rest/binary>> = _DecoderConfig) ->
  {_SPS, <<PPSCount, PPSRest/binary>>} = parse_avc_sps(Rest, SPSCount, []),
  {_PPS, _Rest1} = parse_avc_pps(PPSRest, PPSCount, []),
  % ?D({"Length size", LengthSize+1, SPSCount, Version, Profile, ProfileCompat, Level/10.0, SPS, PPS}),
  ok.

parse_avc_sps(Rest, 0, SPS) -> {SPS, Rest};
parse_avc_sps(<<Length:16, SPSData:Length/binary, Rest/binary>>, SPSCount, SPS) -> 
  parse_avc_sps(Rest, SPSCount - 1, [SPSData|SPS]).
  
parse_avc_pps(Rest, 0, PPS) -> {PPS, Rest};
parse_avc_pps(<<Length:16, PPSData:Length/binary, Rest/binary>>, PPSCount, PPS) -> 
  parse_avc_pps(Rest, PPSCount - 1, [PPSData|PPS]).

% Internal structure to parse all moov data, untill it reaches mp4_frame table
-record(mp4_frames, {
  data_format,
  timescale,
  index = 1,
  dts = 0,
  chunk_table = [],
  chunk_offsets = [],
  keyframes = [],
  sample_sizes = [],
  durations = [],
  duration = 0
}).


fill_track_info(MediaInfo, #mp4_track{data_format = avc1, decoder_config = DecoderConfig, width = Width, height = Height} = Track) ->
  calculate_sample_offsets(MediaInfo#media_info{video_decoder_config = DecoderConfig, width = Width, height = Height}, Track);



fill_track_info(MediaInfo, #mp4_track{data_format = mp4a, decoder_config = DecoderConfig} = Track) ->
  calculate_sample_offsets(MediaInfo#media_info{audio_decoder_config = DecoderConfig}, Track);
  
fill_track_info(MediaInfo, #mp4_track{data_format = Unknown}) ->
  ?D({"Uknown data format", Unknown}),
  MediaInfo.
  


next_duration(#mp4_frames{durations = []}) ->
  {error};

next_duration(#mp4_frames{durations = [{0, _} | Durations]} = FrameReader) ->
  next_duration(FrameReader#mp4_frames{durations = Durations});

next_duration(#mp4_frames{durations = [{DurationCount, Duration} | Durations], duration = TotalDuration} = FrameReader) ->
  {TotalDuration, FrameReader#mp4_frames{durations = [{DurationCount - 1, Duration} | Durations], duration = TotalDuration + Duration}}.


chunk_samples_count(#mp4_frames{chunk_table = [{_, SamplesInChunk, _}]} = FrameReader) ->
  {SamplesInChunk, FrameReader};
  
chunk_samples_count(#mp4_frames{chunk_table = [{FirstChunk, _, _} | [{NextChunk, NextSamplesInChunk, SampleId} | ChunkTable]]} = FrameReader) when FirstChunk == NextChunk ->
  {NextSamplesInChunk, FrameReader#mp4_frames{chunk_table = [{NextChunk + 1, NextSamplesInChunk, SampleId} | ChunkTable]}};

chunk_samples_count(#mp4_frames{chunk_table = [{FirstChunk, SamplesInChunk, SampleId} | ChunkTable]} = FrameReader) ->
  {SamplesInChunk, FrameReader#mp4_frames{chunk_table = [{FirstChunk + 1, SamplesInChunk, SampleId} | ChunkTable]}}.

  
calculate_samples_in_chunk(_FrameTable, _SampleOffset, 0, #mp4_frames{} = FrameReader) ->
  FrameReader;

calculate_samples_in_chunk(FrameTable, SampleOffset, SamplesInChunk, 
  #mp4_frames{index = Index, data_format = DataFormat, keyframes = Keyframes, timescale = Timescale,
    sample_sizes = [SampleSize | SampleSizes]} = FrameReader) ->
  % add dts field
  {Dts, FrameReader1} = next_duration(FrameReader),
  TimestampMS = round(Dts * 1000 / Timescale),
  {FrameType, Id} = case DataFormat of
    avc1 -> {video, TimestampMS*3 + 1 + 3};
    mp4a -> {audio, TimestampMS*3 + 2 + 3}
  end,
  Frame = #file_frame{id = Id, timestamp = TimestampMS, type = FrameType, offset = SampleOffset, size = SampleSize, keyframe = lists:member(Index, Keyframes)},
  % ~D([Id, TimestampMS, SampleOffset, SampleSize, Dts, lists:member(Index, Keyframes)]),
  ets:insert(FrameTable, Frame),
  FrameReader2 = FrameReader1#mp4_frames{sample_sizes = SampleSizes, index = Index + 1},
  calculate_samples_in_chunk(FrameTable, SampleOffset + SampleSize, SamplesInChunk - 1, FrameReader2).
  
calculate_sample_offsets(_FrameTable, #mp4_frames{chunk_offsets = []} = FrameReader) ->
  FrameReader;
  
calculate_sample_offsets(FrameTable, #mp4_frames{chunk_offsets = [ChunkOffset | ChunkOffsets]} = FrameReader) ->
  {SamplesInChunk, FrameReader1} = chunk_samples_count(FrameReader),
  
  % io:format("~p~n", [[ChunkOffset, SamplesInChunk]]),
  FrameReader2 = calculate_samples_in_chunk(FrameTable, ChunkOffset, SamplesInChunk, FrameReader1),
  calculate_sample_offsets(FrameTable, FrameReader2#mp4_frames{chunk_offsets = ChunkOffsets});

calculate_sample_offsets(#media_info{frames = FrameTable} = MediaInfo, Track) ->
  #mp4_track{
    chunk_offsets = ChunkOffsets, 
    chunk_table = ChunkTable, 
    keyframes = Keyframes, 
    sample_sizes = SampleSizes, 
    sample_durations = Durations,
    data_format = DataFormat,
    timescale = Timescale} = Track,
      
  calculate_sample_offsets(FrameTable, 
    #mp4_frames{
      chunk_offsets = ChunkOffsets, 
      chunk_table = ChunkTable, 
      keyframes = Keyframes, 
      sample_sizes = SampleSizes, 
      durations = Durations, 
      data_format = DataFormat,
      timescale = Timescale}),
  MediaInfo.

  
-define(MP4ESDescrTag, 3).
-define(MP4DecConfigDescrTag, 4).
-define(MP4DecSpecificDescrtag, 5).

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
  ?D({"MP4 desc length", TagLength}),
  <<Rest1:TagLength/binary, Rest2/binary>> = Rest,
  {Rest1, Rest2}.

esds_tag(<<3, Rest/binary>>) ->
  {DecoderConfigTag, _Other1} = mp4_desc_length(Rest),
  <<_HardcodedOffset:3/binary, 4, Rest1/binary>> = DecoderConfigTag,
  {SpecificInfoTag, _Other2} = mp4_desc_length(Rest1),
  <<_HardcodedOffset1:13/binary, ?MP4DecSpecificDescrtag, ConfigData/binary>> = SpecificInfoTag,
  {Config, _Other3} = mp4_desc_length(ConfigData),
  % ?D({"MP4DecSpecificDescrtag", Length, Config}),
  <<Config/binary, 6>>;

esds_tag(<<_HardcodedOffset:20/binary, ?MP4DecSpecificDescrtag, Length/integer, Config:Length/binary, _Rest/binary>>) ->
  % ?D({"MP4DecSpecificDescrtag", Length, Config}),
  <<Config/binary, 6>>.
  
    
    
    