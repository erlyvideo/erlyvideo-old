%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Internal module with flv constants
%%% It has many functions, but you need only several of them: read_header/1, header/1, read_tag/2, encode_tag/1
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org</a> for more information
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
%% FLV header
-define(FLV_HEADER_LENGTH,          9).
-define(FLV_HEAD_SIG,    <<70,76,86>>).
-define(FLV_HEAD_OFFSET,  <<0,0,0,9>>).
-define(FLV_PREV_TAG_SIZE_LENGTH,   4).
-define(FLV_TAG_HEADER_LENGTH,     11).

%% FLV tag
-define(FLV_TAG_TYPE_AUDIO, 8).
-define(FLV_TAG_TYPE_VIDEO, 9).
-define(FLV_TAG_TYPE_META,  18).




%% FLV audio
-define(FLV_AUDIO_TYPE_MONO,   0).
-define(FLV_AUDIO_TYPE_STEREO, 1).
-define(FLV_AUDIO_SIZE_8BIT,   0).
-define(FLV_AUDIO_SIZE_16BIT,  1).
-define(FLV_AUDIO_RATE_5_5, 0).
-define(FLV_AUDIO_RATE_11, 1).
-define(FLV_AUDIO_RATE_22, 2).
-define(FLV_AUDIO_RATE_44, 3).
-define(FLV_AUDIO_FORMAT_PCM,          0).
-define(FLV_AUDIO_FORMAT_ADPCM,        1).
-define(FLV_AUDIO_FORMAT_MP3,          2).
-define(FLV_AUDIO_FORMAT_PCM_LE,       3).
-define(FLV_AUDIO_FORMAT_NELLYMOSER8,  5).
-define(FLV_AUDIO_FORMAT_NELLYMOSER,   6).
-define(FLV_AUDIO_FORMAT_A_G711,       7).
-define(FLV_AUDIO_FORMAT_MU_G711,      8).
-define(FLV_AUDIO_FORMAT_RESERVED,     9).
-define(FLV_AUDIO_FORMAT_AAC,          10).
-define(FLV_AUDIO_FORMAT_SPEEX,        11).
-define(FLV_AUDIO_FORMAT_MP3_8KHZ,     14).
-define(FLV_AUDIO_FORMAT_DEVICE,       15).
-define(FLV_AUDIO_AAC_SEQUENCE_HEADER, 0).
-define(FLV_AUDIO_AAC_RAW,             1).

%% FLV video
-define(FLV_VIDEO_FRAME_TYPE_KEYFRAME,        1).
-define(FLV_VIDEO_FRAME_TYPEINTER_FRAME,      2).
-define(FLV_VIDEO_FRAME_TYPEDISP_INTER_FRAME, 3).
-define(FLV_VIDEO_FRAME_TYPE_GENERATED,       4).
-define(FLV_VIDEO_FRAME_TYPE_COMMAND,         5).

-define(FLV_VIDEO_CODEC_JPEG,                 1).
-define(FLV_VIDEO_CODEC_SORENSON,             2).
-define(FLV_VIDEO_CODEC_SCREENVIDEO,          3).
-define(FLV_VIDEO_CODEC_ON2VP6,               4).
-define(FLV_VIDEO_CODEC_ON2VP6_ALPHA,         5).
-define(FLV_VIDEO_CODEC_SCREENVIDEO2,         6).
-define(FLV_VIDEO_CODEC_AVC,                  7).

-define(FLV_VIDEO_AVC_SEQUENCE_HEADER,        0).
-define(FLV_VIDEO_AVC_NALU,                   1).
-define(FLV_VIDEO_AVC_SEQUENCE_END,           2).

