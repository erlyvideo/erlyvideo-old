%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @doc        Matroska file reader
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
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-module(mkv).
-author('Max Lapshin <max@maxidoors.ru>').
-compile(export_all).


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
  Offset1 = read_tag_from_file(File, 0),
  read_tag_from_file(File, Offset1),
  
  % {ok, Data1} = file:pread(File, 0, 16),
  % {tag, ClassId, Size, Length} = next_tag(Data1),
  % ?D({"Tag1", int_to_hex(ClassId), Size, Length}),
  
  file:close(File).
  
read_tag_from_file(File, Offset) ->
  {ok, Data} = file:pread(File, Offset, 16),
  {tag, ClassId, Size, Length} = next_tag(Data),
  ?D({ClassId, Size, Length}),
  % {ok, Bin} = file:pread(File, Length, Size),
  % ?D({"Tag1", ClassId, Size, Length, Bin}),
  % 
  % ?D(read_container(Bin1)),
  Offset + Size + Length.

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



data_size(<<0:7, 1:1, Value:(0+8*7), Rest/binary>>) -> {Value, 8, Rest};
data_size(<<0:6, 1:1, Value:(1+8*6), Rest/binary>>) -> {Value, 7, Rest};
data_size(<<0:5, 1:1, Value:(2+8*5), Rest/binary>>) -> {Value, 6, Rest};
data_size(<<0:4, 1:1, Value:(3+8*4), Rest/binary>>) -> {Value, 5, Rest};
data_size(<<0:3, 1:1, Value:(4+8*3), Rest/binary>>) -> {Value, 4, Rest};
data_size(<<0:2, 1:1, Value:(5+8*2), Rest/binary>>) -> {Value, 3, Rest};
data_size(<<0:1, 1:1, Value:(6+8*1), Rest/binary>>) -> {Value, 2, Rest};
data_size(<<     1:1, Value:(7+8*0), Rest/binary>>) -> {Value, 1, Rest}.


tag(16#1A45DFA3) -> ebml;
tag(16#4286) -> ebml_version;
tag(16#42F7) -> ebml_read_version;
tag(16#42F2) -> ebml_max_id_length;
tag(16#4282) -> doctype;
tag(16#4287) -> doctype_version;
tag(16#4285) -> doctype_read_version;

tag(16#18538067) -> segment;
tag(ClassId) -> int_to_hex(ClassId).

next_tag(Binary) ->
  {ClassId, Length1, Rest1} = class_id(Binary),
  {Size, Length2, _} = data_size(Rest1),
  {tag, tag(ClassId), Size, Length1 + Length2}.
  
  
read_container(Binary) -> 
  read_container(Binary, []).
  
read_container(<<>>, Tags) -> 
  lists:reverse(Tags);
  
read_container(Binary, Tags) ->
  Tag = next_tag(Binary),
  {tag, _, Size, Length} = Tag,
  AllLength = (Size + Length),
  <<_:Length/binary, Body:Size/binary, Rest/binary>> = Binary,
  ?D({Tag, Body, Rest}),
  read_container(Rest, [{Tag, Body} | Tags]).
  
%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").


