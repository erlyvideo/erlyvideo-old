%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        WAV (un)packing module
%%% Read https://ccrma.stanford.edu/courses/422/projects/WaveFormat/ for description of header
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
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
-module(wav).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include("../include/wav.hrl").

-export([header_size/0, read_header/1, write_header/1, write_header/3]).


header_size() ->
  44.

read_header(<<"RIFF", _TotalSize:32/little, "WAVE", "fmt ", 16:32/little, 
              AudioFormat:16/little, Channels:16/little, Rate:32/little, _ByteRate:32/little,
              _BlockAlign:16/little, BitsPerSample:16/little, "data", _Sub2Size:32/little>>) ->
  #wav_header{audio = AudioFormat, channels = Channels, rate = Rate, bps = BitsPerSample}.

audio_format(pcm_le) -> ?WAV_PCM_LE;
audio_format(pcma) -> ?WAV_PCMA;
audio_format(pcmu) -> ?WAV_PCMU;
audio_format(Format) when is_integer(Format) -> Format.

bps(pcm_le) -> 16;
bps(pcma) -> 8;
bps(pcmu) -> 8.

write_header(AudioFormat, Channels, Rate) ->
  write_header(#wav_header{audio = AudioFormat, channels = Channels, rate = Rate, bps = bps(AudioFormat)}).

write_header(#wav_header{audio = AudioFormat, channels = Channels, rate = Rate, bps = BitsPerSample}) ->
  Format = audio_format(AudioFormat),
  BlockAlign = BitsPerSample div 8,
  ByteRate = BlockAlign * Rate,
  DataSize = 0,
  TotalSize = DataSize + 36,
  <<"RIFF", TotalSize:32/little, "WAVE", "fmt ", 16:32/little,
    Format:16/little, Channels:16/little, Rate:32/little, ByteRate:32/little,
    BlockAlign:16/little, BitsPerSample:16/little, "data", DataSize:32/little>>.
  
              
              