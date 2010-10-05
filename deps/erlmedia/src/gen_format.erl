%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @doc        Gen_format
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% 
%%% Required functions: 
%%%
%%% ```init(#media_info{device = File} = MediaInfo) -> {ok, MediaInfo} | {error, Reason}'''
%%%  This function receives already opened file.
%%%  You may fill fields in media_info:  
%%%    frames, video_track, audio_track, header, width, height, duration, framerate, timescale, audio_config, video_config
%%%    
%%%
%%% ```read_frame(MediaInfo, Offset) -> #video_frame{next_id = NextOffset}'''
%%%  Client calls this method frame by frame, retrieving NextOffset
%%%  First frame is called with Offset = undefined. 
%%%
%%% ```seek(MediaInfo, BeforeAfter, Timestamp) -> {Offset, RealTimestamp} | undefined'''
%%%  This function returns new Offset, which must be used in read_frame. 
%%%  BeforeAfter means if we need to return keyframe before Timestamp or after
%%%
%%% ```properties(MediaInfo) -> Proplist | undefined'''
%%%  This function returns metadata in usual erlang proplist format.
%%%  file_media will use it to send to client.
%%%
%%% ```can_open_file(Name) -> boolean'''
%%%  Can this reader open file Name? Should check, basing on extension.
%%%  Mention, that Name is string()
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------

-module(gen_format).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([behaviour_info/1]).

%%-------------------------------------------------------------------------
%% @spec (Callbacks::atom()) -> CallBackList::list()
%% @doc  List of require functions in a video file reader
%% @hidden
%% @end
%%-------------------------------------------------------------------------
behaviour_info(callbacks) -> [{init, 2}, {seek, 3}, {read_frame, 2}, {properties, 1}, {can_open_file, 1}, {write_frame, 2}];
behaviour_info(_Other) -> ?D({"Behaviour", _Other}), undefined.
