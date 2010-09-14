%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        MPEG TS constants module
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlang-mpegts.
%%% 
%%% erlang-mpegts is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-mpegts is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-mpegts.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-define(TYPE_VIDEO_MPEG1, 1).
-define(TYPE_VIDEO_MPEG2, 2).
-define(TYPE_VIDEO_MPEG4, 16).
-define(TYPE_VIDEO_H264,  27).
-define(TYPE_VIDEO_VC1,   234).
-define(TYPE_VIDEO_DIRAC, 209).
-define(TYPE_AUDIO_MPEG1, 3).
-define(TYPE_AUDIO_MPEG2, 4).
-define(TYPE_AUDIO_AAC,   15).
-define(TYPE_AUDIO_AAC2,  17).
-define(TYPE_AUDIO_AC3,   129).
-define(TYPE_AUDIO_DTS,   138).

-define(MPEGTS_STREAMID_AAC, 192).
-define(MPEGTS_STREAMID_H264, 224).

-define(DESCRIPTOR_IOD, 29).
-define(DESCRIPTOR_CA, 9).
-define(DESCRIPTOR_SL, 31).

-include("log.hrl").


-record(mpegts_pat, {
  descriptors
}).

