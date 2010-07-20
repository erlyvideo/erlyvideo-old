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
-include("../include/mp4.hrl").
-include("../include/video_frame.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("log.hrl").

-export([ftyp/2, moov/2, mvhd/2, trak/2, tkhd/2, mdia/2, mdhd/2, stbl/2, stsd/2, esds/2, avcC/2]).
-export([btrt/2, stsz/2, stts/2, stsc/2, stss/2, stco/2, smhd/2, minf/2, ctts/2]).
-export([mp4a/2, avc1/2, s263/2, samr/2]).
-export([hdlr/2, vmhd/2, dinf/2, dref/2, 'url '/2]).
-export([extract_language/1]).


-export([mp4_desc_length/1, read_header/1, read_frame/2, frame_count/1, seek/3, mp4_read_tag/1]).

-define(FRAMESIZE, 32).

read_header(Reader) ->
  read_header(#mp4_media{}, Reader, 0).


read_header(Mp4Media, {Module, Device} = Reader, Pos) -> 
  case read_atom_header(Reader, Pos) of
    eof -> {ok, Mp4Media};
    {error, Reason} -> {error, Reason};
    {atom, mdat, Offset, Length} ->
      read_header(Mp4Media, Reader, Offset + Length);
    {atom, _AtomName, Offset, 0} -> 
      read_header(Mp4Media, Reader, Offset);
    {atom, AtomName, Offset, Length} -> 
      ?D({"Root atom", AtomName, Length}),
      {ok, AtomData} = Module:pread(Device, Offset, Length),
      NewMedia = case erlang:function_exported(mp4, AtomName, 2) of
        true -> mp4:AtomName(AtomData, Mp4Media);
        false -> ?D({"Unknown atom", AtomName}), Mp4Media
      end,
      read_header(NewMedia, Reader, Offset + Length)
  end.


read_atom_header({Module, Device}, Pos) ->
  case Module:pread(Device, Pos, 8) of
    {ok, <<AtomLength:32, AtomName/binary>>} when AtomLength >= 8 ->
      % ?D({"Atom", binary_to_atom(AtomName, latin1), Pos, AtomLength}),
      {atom, binary_to_atom(AtomName, utf8), Pos + 8, AtomLength - 8};
    eof ->
      eof;
    Else -> 
      {error, Else}
  end.


seek(#mp4_track{frames = Frames}, BeforeAfter, Timestamp) ->
  seek(Frames, BeforeAfter, Timestamp);

seek(Frames, before, Timestamp) ->
  seek(Frames, before, Timestamp, 0, undefined).

seek(<<1:1, _Size:63, _Offset:64, DTS:64/float, _PTS:64/float, _/binary>>, before, Timestamp, _, {FoundId,FoundDTS}) when DTS > Timestamp ->
  {FoundId,FoundDTS};

seek(<<1:1, _Size:63, _Offset:64, DTS:64/float, _PTS:64/float, Frames/binary>>, before, Timestamp, Id, _)  ->
  seek(Frames, before, Timestamp, Id+1, {Id, DTS});

seek(<<_:?FRAMESIZE/binary, Frames/binary>>, Direction, Timestamp, Id, Found) ->
  seek(Frames, Direction, Timestamp, Id+1, Found);
  
seek(<<>>, _, _, _, Found) ->
  Found.
  
read_frame(#mp4_track{frames = Frames}, Id) when Id*?FRAMESIZE < size(Frames) ->
  read_frame(Frames, Id);

read_frame(Frames, Id) when Id*?FRAMESIZE < size(Frames) ->
  FrameOffset = Id*?FRAMESIZE,
  % ?D({read_frame,Id, size(Frames) div ?FRAMESIZE}),
  <<_:FrameOffset/binary, FKeyframe:1, Size:63, Offset:64, DTS:64/float, PTS:64/float, _/binary>> = Frames,
  Keyframe = case FKeyframe of
    1 -> true;
    0 -> false
  end,
  #mp4_frame{id = Id, dts = DTS, pts = PTS, size = Size, offset = Offset, keyframe = Keyframe}.
  

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
  AtomName = binary_to_atom(BinaryAtomName, utf8),
  NewMp4Parser = case erlang:function_exported(?MODULE, AtomName, 2) of
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
mvhd(<<0:32, CTime:32, MTime:32, TimeScale:32, Duration:32, Rate:16, _RateDelim:16,
      Volume:16, 0:16, _Reserved1:64, Matrix:36/binary, _Reserved2:24/binary, NextTrackId:32>>, #mp4_media{} = Media) ->
        
  Meta = [{ctime,CTime},{mtime,MTime},{timescale,TimeScale},{duration,Duration},{rate,Rate},
          {volume,Volume},{matrix,Matrix},{next_track,NextTrackId}],
  ?D(Meta),
  Media#mp4_media{timescale = TimeScale, duration = Duration, seconds = Duration/TimeScale}.

% Track box
trak(<<>>, MediaInfo) ->
  MediaInfo;
  
trak(Atom, MediaInfo) ->
  Track = parse_atom(Atom, #mp4_track{}),
  fill_track_info(MediaInfo, Track).
  

% Track header
tkhd(<<0, Flags:24, CTime:32, MTime:32, TrackID:32, _Reserved1:32, 
       Duration:32, _Reserved2:64, Layer:16, _AlternateGroup:16,
       Volume, VolDelim, _Reserved3:16, Matrix:36/binary, Width:16, WidthDelim:16, Height:16, HeightDelim:16>>, Mp4Track) ->
  Meta = [{flags,Flags},{ctime,CTime},{mtime,MTime},{track_id,TrackID},{duration,Duration},{layer,Layer},
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
  [L1+16#60, L2+16#60, L3+16#60].


%% Handler Reference Box
hdlr(<<0:32, 0:32, "vide", 0:96, NameNull/binary>>, Mp4Track) ->
  Len = (size(NameNull) - 1),
  Name = case NameNull of
    <<N:Len/binary, 0>> -> N;
    _ -> NameNull
  end,
  ?D({hdlr, video, Name}),
  Mp4Track;

hdlr(<<0:32, 0:32, "soun", 0:96, NameNull/binary>>, Mp4Track) ->
  Len = (size(NameNull) - 1),
  Name = case NameNull of
    <<N:Len/binary, 0>> -> N;
    _ -> NameNull
  end,
  ?D({hdlr, audio, Name}),
  Mp4Track;

hdlr(<<0:32, 0:32, "hint", 0:96, NameNull/binary>>, Mp4Track) ->
  Len = (size(NameNull) - 1),
  Name = case NameNull of
    <<N:Len/binary, 0>> -> N;
    _ -> NameNull
  end,
  ?D({hdlr, hint, Name}),
  Mp4Track;

hdlr(<<0:32, 0:32, Handler:4/binary, 0:96, NameNull/binary>>, Mp4Track) ->
  Len = (size(NameNull) - 1),
  Name = case NameNull of
    <<N:Len/binary, 0>> -> N;
    _ -> NameNull
  end,
  ?D({hdlr, Handler, Name}),
  Mp4Track.
  
  
% SMHD atom
smhd(<<0:8, _Flags:3/binary, 0:16/big-signed-integer, _Reserve:2/binary>>, Mp4Track) ->
  Mp4Track;

smhd(<<0:8, _Flags:3/binary, _Balance:16/big-signed-integer, _Reserve:2/binary>>, Mp4Track) ->
  Mp4Track.

% Media information
minf(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

%% Video Media Header Box
vmhd(<<Version:32, Mode:16, R:16, G:16, B:16>>, Mp4Track) ->
  _VMHD = {vmhd, Version, Mode, R, G, B},
  ?D(_VMHD),
  Mp4Track.
  
dinf(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).
  
dref(<<0:32, Count:32, Atom/binary>> = Dref, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

'url '(URL, Mp4Track) ->
  ?D({url, URL}),
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

  
% ESDS atom
esds(<<Version:8, _Flags:3/binary, DecoderConfig/binary>>, #mp4_track{data_format = aac} = Mp4Track) when Version == 0 ->
  ?D({"Extracted audio config", DecoderConfig}),
  Mp4Track#mp4_track{decoder_config = config_from_esds_tag(DecoderConfig)}.

% avcC atom
avcC(DecoderConfig, #mp4_track{} = Mp4Track) ->
  ?D({"Extracted video config", DecoderConfig, h264:unpack_config(DecoderConfig)}),
  Mp4Track#mp4_track{decoder_config = DecoderConfig}.

btrt(<<_BufferSize:32, MaxBitRate:32, AvgBitRate:32>>, #mp4_track{} = Mp4Track) ->
  Mp4Track#mp4_track{max_bitrate = MaxBitRate, bitrate = AvgBitRate}.


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


clean_track(#mp4_track{} = Track) ->
  Track#mp4_track{sample_sizes = [], sample_dts = [], sample_offsets = [], sample_composition = [],
                  keyframes = [], chunk_offsets = [], chunk_sizes = []}.

append_track(#mp4_media{video_tracks = Tracks} = MediaInfo, #mp4_track{data_format = h264, width = Width, height = Height} = Track) ->
  MediaInfo#mp4_media{width = Width, height = Height, video_tracks = [clean_track(Track)|Tracks]};

append_track(#mp4_media{audio_tracks = Tracks} = MediaInfo, #mp4_track{data_format = aac} = Track) ->
  MediaInfo#mp4_media{audio_tracks = [clean_track(Track)|Tracks]};
  
append_track(MediaInfo, #mp4_track{data_format = Format}) ->
  ?D({"Unknown format", Format}),
  MediaInfo.

fill_track_info(#mp4_media{} = MediaInfo, #mp4_track{} = Track) ->
  {Frames, MaxDTS} = fill_track(Track),
  Seconds = max_duration(MediaInfo, MaxDTS),
  Media1 = append_track(MediaInfo, Track#mp4_track{frames = Frames}),
  Media1#mp4_media{seconds = Seconds}.
  
max_duration(#mp4_media{seconds = undefined}, Seconds2) -> Seconds2;
max_duration(#mp4_media{seconds = Seconds1}, Seconds2) when Seconds2 > Seconds1 -> Seconds2;
max_duration(#mp4_media{seconds = Seconds1}, _) -> Seconds1.


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
  % Frames = ets:new(frames, [ordered_set, {keypos, #mp4_frame.id}]),
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
  ?D({max_dts, MaxDTS, size(Filled) div ?FRAMESIZE}),
  {Filled, MaxDTS}.

fill_track(Frames, [], [], [], [], [], _, _, DTS) ->
  {Frames, DTS};

fill_track(Frames, [Size|SampleSizes], [Offset|Offsets], [Keyframe|Keyframes], [DTS|Timestamps], [CTime|Compositions], Timescale, Id, _) ->
  % Frame = #mp4_frame{id = Id, dts = DTS*1000/Timescale, pts = (DTS+PTS)*1000/Timescale, size = Size, offset = Offset, keyframe = Keyframe},
  % ets:insert(Frames, Frame),
  FDTS = DTS*1000/Timescale,
  FPTS = (DTS+CTime)*1000/Timescale,
  FKeyframe = case Keyframe of
    true -> 1;
    false -> 0
  end,
  fill_track(<<Frames/binary, FKeyframe:1, Size:63, Offset:64, FDTS:64/float, FPTS:64/float>>,
             SampleSizes, Offsets, Keyframes, Timestamps, Compositions, Timescale, Id+1, FDTS).

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
