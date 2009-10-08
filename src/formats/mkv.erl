%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @doc        Matroska file reader
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

-module(mkv).
-author('max@maxidoors.ru').
-include("../../include/ems.hrl").
-export([init/1, read_frame/1]).
-behaviour(gen_format).


init(#video_player{header = undefined} = Player) -> 
  % init(Player#video_player{frames = ets:new(frames, [ordered_set, {keypos, 2}])});


% init(#video_player{frames = _FrameTable} = Player) -> 
  read_element(Player#video_player{pos = 0}),
  {ok, Player}.


read_frame(#video_player{pos = '$end_of_table'}) ->
  {ok, done};


read_frame(#video_player{pos = undefined, frames = FrameTable} = Player) ->
  read_frame(Player#video_player{pos = ets:first(FrameTable)});
  
read_frame(#video_player{}) ->
  {ok, done}.


match_int(#video_player{pos = Pos, device = IoDev}, Bytes) ->
  {ok, Data} = file:pread(IoDev, Pos, Bytes),
  BitSize = Bytes*7,
  <<_:Bytes/bits, Int:BitSize/integer>> = iolist_to_binary(Data),
  {ok, Int, Bytes}.
  
  
decode_int(Player, <<1:1, Int:7>>) -> {ok, Int, 1};
decode_int(Player, <<0:1, 1:1, _/bits>>) -> match_int(Player, 2);
decode_int(Player, <<0:2, 1:1, _/bits>>) -> match_int(Player, 3);
decode_int(Player, <<0:3, 1:1, _/bits>>) -> match_int(Player, 4);
decode_int(Player, <<0:4, 1:1, _/bits>>) -> match_int(Player, 5);
decode_int(Player, <<0:5, 1:1, _/bits>>) -> match_int(Player, 6);
decode_int(Player, <<0:6, 1:1, _/bits>>) -> match_int(Player, 7);
decode_int(Player, <<0:7, 1:1>>)      -> match_int(Player, 8).
  

next_int(#video_player{pos = Pos, device = IoDev} = Player) ->
  case file:pread(IoDev, Pos, 1) of
    {ok, Data} -> decode_int(Player, iolist_to_binary(Data));
    {eof} -> eof
  end.

read_element(#video_player{pos = Pos, device = IoDev} = Player) ->
  {ok, ClassId, IdBytes} = next_int(Player),
  {ok, Size, SizeBytes} = next_int(Player#video_player{pos = Pos + IdBytes}),
  ?D({"Read", ClassId, Size})
  .
