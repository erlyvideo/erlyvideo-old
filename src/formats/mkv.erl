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

init(#video_player{header = undefined} = Player) -> 
  init(Player#video_player{frames = ets:new(frames, [ordered_set, {keypos, 2}])});


init(#video_player{header = Header, device = IoDev, frames = FrameTable} = Player) -> 
  {ok, Player}.


read_frame(#video_player{pos = '$end_of_table'}) ->
  {ok, done};


read_frame(#video_player{pos = undefined, frames = FrameTable} = Player) ->
  read_frame(Player#video_player{pos = ets:first(FrameTable)});
  
read_frame(#video_player{}) ->
  {ok, done}.

