%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @doc        Matroska file reader
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% Source documentation is here: http://matroska.org/technical/specs/index.html
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
-module(mkv).
-author('Max Lapshin <max@maxidoors.ru>').
-compile(export_all).

-include("log.hrl").

-record(tag, {
  name,
  type,
  length,
  size
}).

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)];

int_to_hex(N) when is_integer(N) ->
  int_to_hex(N div 256) ++ int_to_hex(N rem 256);
  
int_to_hex(L) when is_list(L) ->
  string:join(lists:map(fun int_to_hex/1, L), "").


hex(N) when N < 10 ->
  $0+N;
hex(N) when N >= 10, N < 16 ->
  $A + (N-10).
  
open_file(Path) ->
  {ok, File} = file:open(Path, [read, binary]),
  {EBML, Offset2} = read_tag_from_file(File, 0),
  ?D(EBML),
  peek_container_in_file(File, EBML#tag.length, EBML#tag.size),

  % {ok, EBMLBin} = file:pread(File, 0 + EBML#tag.length, EBML#tag.size),
  % ?D(read_container(EBMLBin)),
  
  {Segment, _} = read_tag_from_file(File, Offset2),
  ?D(Segment),
  
  peek_container_in_file(File, Offset2 + Segment#tag.length, Segment#tag.size),
  % {Tag1, _, Off1} = read_tag_from_file(File, Offset2 + Segment#tag.length),
  % ?D(Tag1),
  
  % {ok, Data1} = file:pread(File, 0, 16),
  % {tag, ClassId, Size, Length} = next_tag(Data1),
  % ?D({"Tag1", int_to_hex(ClassId), Size, Length}),
  
  file:close(File).
  
read_tag_from_file(File, Offset) ->
  case file:pread(File, Offset, 12) of
    {ok, Data} ->
      Tag = next_tag(Data),
      {Tag, Offset + Tag#tag.size + Tag#tag.length};
    eof ->
      eof
  end.


peek_container_in_file(_, _, 0) -> ok;

peek_container_in_file(File, Offset, Size) ->
  {Tag, NextOffset} = read_tag_from_file(File, Offset),
  case {Tag#tag.name, Tag#tag.type} of
    {simpleblock, _} ->
      ok;
    {_Name, box} ->
      % ?D(Tag),
      ?D({_Name, '>>>>>>>>'}),
      peek_container_in_file(File, Offset + Tag#tag.length, Tag#tag.size),
      ?D('<<<<<<<<');
    {_, Type} when Type == string orelse Type == utf8 ->
      {ok, String} = file:pread(File, Offset + Tag#tag.length, Tag#tag.size),
      ?D({Tag#tag.name, String});
    {_, uint} ->
      {ok, Bin} = file:pread(File, Offset + Tag#tag.length, Tag#tag.size),
      Len = 8*size(Bin),
      <<Val:Len>> = Bin,
      ?D({Tag#tag.name, uint, Val});
    {_, float} ->
      {ok, Bin} = file:pread(File, Offset + Tag#tag.length, Tag#tag.size),
      Len = 8*size(Bin),
      <<Val:Len/float>> = Bin,
      ?D({Tag#tag.name, float, round(Val)});
    {_, _} ->
      ?D(Tag)
  end,
  peek_container_in_file(File, NextOffset, Size - Tag#tag.size - Tag#tag.length).

% match_int(#video_player{pos = Pos, device = IoDev}, Bytes) ->
%   {ok, Data} = file:pread(IoDev, Pos, Bytes),
%   BitSize = Bytes*7,
%   BData = iolist_to_binary(Data),
%   <<_Shift:Bytes/big-integer, Int:BitSize/big-integer>> = BData,
%   ByteSize = Bytes*8,
%   <<WholeInt:ByteSize/big-integer>> = BData,
%   ?D({"Int", Int, WholeInt}),
%   {ok, Int, Bytes}.
%   
class_id(<<0:3, 1:1, Value:(4+8*3), Rest/binary>>) -> {Value + 16#10000000, 4, Rest};
class_id(<<0:2, 1:1, Value:(5+8*2), Rest/binary>>) -> {Value + 16#200000, 3, Rest};
class_id(<<0:1, 1:1, Value:(6+8*1), Rest/binary>>) -> {Value + 16#4000, 2, Rest};
class_id(<<     1:1, Value:(7+8*0), Rest/binary>>) -> {Value + 16#80, 1, Rest}.



uint(<<0:7, 1:1, Value:(0+8*7), Rest/binary>>) -> {Value, 8, Rest};
uint(<<0:6, 1:1, Value:(1+8*6), Rest/binary>>) -> {Value, 7, Rest};
uint(<<0:5, 1:1, Value:(2+8*5), Rest/binary>>) -> {Value, 6, Rest};
uint(<<0:4, 1:1, Value:(3+8*4), Rest/binary>>) -> {Value, 5, Rest};
uint(<<0:3, 1:1, Value:(4+8*3), Rest/binary>>) -> {Value, 4, Rest};
uint(<<0:2, 1:1, Value:(5+8*2), Rest/binary>>) -> {Value, 3, Rest};
uint(<<0:1, 1:1, Value:(6+8*1), Rest/binary>>) -> {Value, 2, Rest};
uint(<<     1:1, Value:(7+8*0), Rest/binary>>) -> {Value, 1, Rest}.


%% EBML basics
tag(16#1A45DFA3) -> {ebml, box};
tag(16#4286) -> {ebml_version, uint};
tag(16#42F7) -> {ebml_read_version, uint};
tag(16#42F2) -> {ebml_max_id_length, uint};
tag(16#42F3) -> {ebml_max_size_length, uint};
tag(16#4282) -> {doctype, string};
tag(16#4287) -> {doctype_version, uint};
tag(16#4285) -> {doctype_read_version, uint};
%% Global elements
tag(16#BF)   -> {crc32, bin};
tag(16#EC)   -> {void, bin};
%% Segment
tag(16#18538067) -> {segment, box};
%% Meta Seek information
tag(16#114D9B74) -> {seek_head, box};
tag(16#4DBB) -> {seek, box};
tag(16#53AB) -> {seek_id, bin};
tag(16#53AC) -> {seek_position, uint};
%% Segment information
tag(16#1549A966) -> {info, box};
tag(16#73A3) -> {segment_uid, bin};
tag(16#2AD7B1) -> {timecode_scale, uint};
tag(16#4D80) -> {muxing_app, utf8};
tag(16#5741) -> {writing_app, utf8};
tag(16#73A4) -> {segment_uid, bin};
tag(16#4489) -> {duration, float};
%% Cluster
tag(16#1F43B675) -> {cluster, box};
tag(16#E7) -> {timecode, uint};
tag(16#A7) -> {position, uint};
tag(16#AB) -> {prevsize, uint};
tag(16#A0) -> {blockgroup, box};
tag(16#A1) -> {block, bin};
tag(16#A3) -> {simpleblock, bin};
%% Track
tag(16#1654AE6B) -> {tracks, box};
tag(16#AE) -> {track, box};
tag(16#D7) -> {track_number, uint};
tag(16#73C5) -> {track_uid, uint};
tag(16#9C) -> {flag_lacing, uint};
tag(16#23314F) -> {timescale, float};
tag(16#22B59C) -> {language, string};
tag(16#86) -> {codec_id, string};
tag(16#83) -> {track_type, uint};
%% Video
tag(16#E0) -> {video, box};
tag(16#B0) -> {width, uint};
tag(16#BA) -> {height, uint};
tag(16#54B0) -> {display_width, uint};
tag(16#54BA) -> {display_height, uint};
tag(16#63A2) -> {codec_private, bin};
%% Audio
tag(16#E1) -> {audio, box};
tag(16#9F) -> {channels, uint};
tag(16#B5) -> {frequency, float};
%% Cueing data
tag(16#1C53BB6B) -> {cues, box};
tag(16#BB) -> {cuepoint, box};
tag(16#B3) -> {cue_time, uint};
tag(16#B7) -> {cue_track_positions, box};
tag(16#F7) -> {cue_track, uint};
tag(16#F1) -> {cue_cluster, uint};
tag(16#5378) -> {cue_block_number, uint};
tag(ClassId) -> {int_to_hex(ClassId), bin}.

next_tag(Binary) ->
  {ClassId, Length1, Rest1} = class_id(Binary),
  {Size, Length2, _} = uint(Rest1),
  {Name, Type} = tag(ClassId),
  #tag{name = Name, type = Type, size = Size, length = Length1 + Length2}.
  
  
read_container(Binary) -> 
  read_container(Binary, []).
  
read_container(<<>>, Tags) -> 
  lists:reverse(Tags);
  
read_container(Binary, Tags) ->
  Tag = next_tag(Binary),
  #tag{size = Size, length = Length} = Tag,
  <<_:Length/binary, Body:Size/binary, Rest/binary>> = Binary,
  % ?D({Tag, Body, Rest}),
  read_container(Rest, [{Tag, Body} | Tags]).
  
%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").


