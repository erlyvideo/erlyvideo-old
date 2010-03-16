%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @doc        Gen_format
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% 
%%% Required functions: 
%%%
%%% init(#media_info{device = File} = MediaInfo) -> {ok, MediaInfo} | {error, Reason}
%%%  This function receives already opened file.
%%%  You may fill fields in media_info:  
%%%    frames, video_track, audio_track, header, width, height, duration, framerate, timescale, audio_config, video_config
%%%    
%%%
%%% first(MediaInfo) -> FirstOffset
%%%  This function must return offset of first frame in file. Offset is internal number
%%%  for format. It may be offset in file, or anything else.
%%%
%%% read_frame(MediaInfo, Offset) -> {#video_frame{}, NextOffset}
%%%  Client calls this method frame by frame, retrieving NextOffset
%%%
%%% seek(MediaInfo, Timestamp) -> {Offset, RealTimestamp} | undefined
%%%  This function returns new Offset, which must be used in read_frame
%%%
%%% codec_config(MediaInfo, audio|video) -> #video_frame{} | undefined
%%%  Returns codec
%%%
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

-module(gen_format).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../../include/ems.hrl").

-export([behaviour_info/1]).

%%-------------------------------------------------------------------------
%% @spec (Callbacks::atom()) -> CallBackList::list()
%% @doc  List of require functions in a video file reader
%% @hidden
%% @end
%%-------------------------------------------------------------------------
behaviour_info(callbacks) -> [{init, 1}, {first, 1}, {seek, 2}, {read_frame, 2}, {codec_config, 2}];
behaviour_info(_Other) -> ?D({"Behaviour", _Other}), undefined.
