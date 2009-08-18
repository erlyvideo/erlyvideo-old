%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @author     Takuma Mori <mori@sgra.co.jp> [http://www.sgra.co.jp/en/], SGRA Corporation
%%% @copyright  2008 Takuma Mori, 2009 Max Lapshin
%%% @doc        MP4 decoding module, rewritten from RubyIzumi
%%% @reference  See <a href="http://github.com/maxlapshin/erlyvideo" target="_top">http://github.com/maxlapshin/erlyvideo</a> for more information
%%% @end
%%%
%%%
%%% Copyright (c) 2008 Takuma Mori, 2009 Max Lapshin
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

-module(ems_mp4).
-author('max@maxidoors.ru').
-include("../include/ems.hrl").

-export([read_header/1]).

% -export([read_header/1,read_frame/1,read_frame/2,to_tag/2,header/1, parse_meta/1, encodeTag/2]).

read_header(IoDev) -> 
  read_header(IoDev, #mp4_parser{}).

read_header(IoDev, Mp4Parser) -> 
  case next_atom(IoDev) of
    {eof} -> {eof};
    {error, Reason} -> {error, Reason};
    {AtomName, Atom} -> 
      NewParser = decode_atom(AtomName, Atom, Mp4Parser),
      read_header(IoDev, NewParser);
    Else -> Else
  end.
  
  
parse_atom(Atom, Mp4Parser) when size(Atom) == 0 ->
  Mp4Parser;
  
parse_atom(Atom, _) when size(Atom) < 4 ->
  {error, "Invalid atom"};
  
parse_atom(<<AllAtomLength:32/big-integer, BinaryAtomName:4/binary, AtomRest/binary>>, Mp4Parser) when (size(AtomRest) >= AllAtomLength - 8) ->
  AtomLength = AllAtomLength - 8,
  <<Atom:AtomLength/binary, Rest/binary>> = AtomRest,
  AtomName = binary_to_atom(BinaryAtomName, utf8),
  % ?D({"Atom", AtomName}),
  NewMp4Parser = decode_atom(AtomName, Atom, Mp4Parser),
  parse_atom(Rest, NewMp4Parser);
  
parse_atom(<<AllAtomLength:32/big-integer, BinaryAtomName:4/binary, _/binary>>, Mp4Parser) ->
  ?D({"Invalid atom", AllAtomLength, binary_to_atom(BinaryAtomName, utf8)}),
  Mp4Parser.
  
% FTYP atom
decode_atom(ftyp, <<Major:4/binary, _Minor:4/binary, CompatibleBrands/binary>>, #mp4_parser{} = Mp4Parser) ->
  NewParser = Mp4Parser#mp4_parser{file_type = binary_to_list(Major), file_types = decode_atom(ftyp, CompatibleBrands, [])},
  ?D({"File type:", NewParser#mp4_parser.file_type, NewParser#mp4_parser.file_types}),
  NewParser;

decode_atom(ftyp, <<>>, BrandList) ->
  lists:reverse(BrandList);

decode_atom(ftyp, <<Brand:4/binary, CompatibleBrands/binary>>, BrandList) ->
  decode_atom(ftyp, CompatibleBrands, [binary_to_list(Brand)|BrandList]);
  
% MOOV atom
decode_atom(moov, Atom, #mp4_parser{tracks = Tracks} = Mp4Parser) ->
  NewParser = parse_atom(Atom, Mp4Parser),
  NewParser#mp4_parser{tracks = lists:reverse(Tracks)};

% MVHD atom
decode_atom(mvhd, <<0:8/integer, _Flags:3/binary, _CTime:32/big-integer, _MTime:32/big-integer, TimeScale:32/big-integer,
                    Duration:32/big-integer, _Rest/binary>>, #mp4_parser{} = Mp4Parser) ->
  ?D({"Movie header:", Duration}),
  Mp4Parser#mp4_parser{timescale = TimeScale, duration = Duration, seconds = Duration/TimeScale};

decode_atom(mvhd, <<Version:8/integer, Rest/binary>>, Mp4Parser) ->
  ?D({"Invalid mvhd structure v.", Version, size(Rest)+1}),
  Mp4Parser;

% TRAK atom
decode_atom(trak, <<>>, #mp4_parser{} = Mp4Parser) ->
  Mp4Parser;
decode_atom(trak, Atom, #mp4_parser{tracks = Tracks} = Mp4Parser) ->
  Track = decode_atom(trak, Atom, #mp4_track{}),  
  Mp4Parser#mp4_parser{tracks = [Track | Tracks]};
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
  ?D({"Track header:", TrackID, Duration}),
  Mp4Track#mp4_track{track_id = TrackID, duration = Duration};

%MDIA atom
decode_atom(mdia, Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track);

% MDHD atom
decode_atom(mdhd,<<0:8/integer, _Flags:24/integer, _Ctime:32/big-integer, 
                  _Mtime:32/big-integer, TimeScale:32/big-integer, Duration:32/big-integer,
                  _Language:2/binary, _Quality:16/big-integer>>, #mp4_track{} = Mp4Track) ->
  ?D({"Timescale:", Duration, extract_language(_Language)}),
  Mp4Track#mp4_track{timescale = TimeScale, duration = Duration};

decode_atom(mdhd, <<1:8/integer, _Flags:24/integer, _Ctime:64/big-integer, 
                     _Mtime:64/big-integer, TimeScale:32/big-integer, Duration:64/big-integer, 
                     _Language:2/binary, _Quality:16/big-integer>>, Mp4Track) ->
  ?D({"Timescale:", Duration, extract_language(_Language)}),
  Mp4Track#mp4_track{timescale = TimeScale, duration = Duration};
  
% SMHD atom
decode_atom(smhd, <<0:8/integer, _Flags:3/binary, 0:16/big-signed-integer, _Reserve:2/binary>>, Mp4Track) ->
  Mp4Track;

decode_atom(smhd, <<0:8/integer, _Flags:3/binary, Balance:16/big-signed-integer, _Reserve:2/binary>>, Mp4Track) ->
  ?D({"Audio balance:", Balance}),
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
  
decode_atom(stsd, {_EntryCount, <<SampleDescriptionSize:32/big-integer, 
                                  "mp4a", _Reserved:6/binary, _RefIndex:16/big-integer, 
                                  _Unknown:8/binary, _ChannelsCount:32/big-integer,
                                  _SampleSize:32/big-integer, _SampleRate:32/big-integer,
                                  Atom/binary>>}, Mp4Track) ->
  parse_atom(Atom, Mp4Track#mp4_track{data_format = mp4a});

decode_atom(stsd, {_EntryCount, <<SampleDescriptionSize:32/big-integer, 
                                  "avc1", _Reserved:6/binary, _RefIndex:16/big-integer, 
                                  _Unknown1:16/binary, 
                                  _Width:16/big-integer, _Height:16/big-integer,
                                  _HorizRes:32/big-integer, _VertRes:32/big-integer,
                                  _FrameCount:16/big-integer, _CompressorName:32/binary,
                                  _Depth:16/big-integer, _Predefined:16/big-integer,
                                  _Unknown:4/binary,
                                  Atom/binary>>}, Mp4Track) ->
  ?D({"Video sample:", _Width, _Height, _CompressorName}),
  parse_atom(Atom, Mp4Track#mp4_track{data_format = avc1});


decode_atom(stsd, {_EntryCount, <<SampleDescriptionSize:32/big-integer, DataFormat:4/binary, 
                                 _Reserved:6/binary, _RefIndex:16/big-integer, EntryData/binary>>}, Mp4Track) 
           when SampleDescriptionSize == size(EntryData) + 16 ->
  NewTrack = Mp4Track#mp4_track{data_format = binary_to_atom(DataFormat, utf8)},
  ?D({"Sample description:", _RefIndex, NewTrack#mp4_track.data_format, SampleDescriptionSize, size(EntryData), binary_to_list(EntryData)}),
  % FIXME: parse extra data, like in RubyIzumi
  NewTrack;
  
% ESDS atom
decode_atom(esds, <<0:8/integer, _Flags:3/binary, DecoderConfig/binary>>, #mp4_track{data_format = mp4a} = Mp4Track) ->
  ?D("Extracted audio config"),
  Mp4Track#mp4_track{decoder_config = DecoderConfig};

% avcC atom
decode_atom(avcC, <<_Version:8/integer, _Flags:3/binary, DecoderConfig/binary>>, #mp4_track{} = Mp4Track) ->
  ?D("Extracted video config"),
  Mp4Track#mp4_track{decoder_config = DecoderConfig};

decode_atom(btrt, <<BufferSize:32/big-integer, MaxBitRate:32/big-integer, AvgBitRate:32/big-integer>>, #mp4_track{data_format = avc1} = Mp4Track) ->
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
  ?D({"Sample time table", Table}),
  Mp4Track#mp4_track{sample_time_table = lists:reverse(Table)};

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
  Mp4Track#mp4_track{sample_chunk_table = lists:reverse(Table)};

decode_atom(stsc, {0, _}, Table) ->
  Table;
  
decode_atom(stsc, {_, <<>>}, Table) ->
  Table;
  
decode_atom(stsc, {EntryCount, <<FirstChunk:32/big-integer, SamplesPerChunk:32/big-integer, SampleID:32/big-integer, Rest/binary>>}, Table) ->
  decode_atom(stsc, {EntryCount - 1, Rest}, [{FirstChunk, SamplesPerChunk, SampleID} | Table]);

% FALLBACK  
decode_atom(AtomName, _, Mp4Parser) ->
  ?D({"Unknown atom", AtomName}),
  Mp4Parser.

extract_language(<<L1:5/integer, L2:5/integer, L3:5/integer, _:1/integer>>) ->
  [L1+16#60, L2+16#60, L3+16#60].

next_atom(IoDev) ->
  case file:read(IoDev, 8) of
    {ok, Data} ->
      <<AtomLength:32/big-integer, AtomName:4/binary>> = list_to_binary(Data),
      case AtomName of
        <<"mdat">> ->
          {eof};
        _ ->
          % ?D({"Atom: ", binary_to_atom(AtomName, utf8), AtomLength}),
          case file:read(IoDev, AtomLength - 8) of
            {ok, Atom} ->
              {binary_to_atom(AtomName, utf8), list_to_binary(Atom)};
            eof ->
              {error, unexpected_eof};
            {error, Reason} ->
              {error, Reason}         
          end
      end;
    eof -> 
      {eof};
    {error, Reason} -> 
      {error, Reason}           
  end.
    
    
    
    
    
    