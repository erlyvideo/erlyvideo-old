%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        WAV (un)packing module
%%% Read https://ccrma.stanford.edu/courses/422/projects/WaveFormat/ for description of header
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
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
-module(wav).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include("../include/wav.hrl").

-export([header_size/0, read_header/1, write_header/1]).


header_size() ->
  44.

read_header(<<"RIFF", _TotalSize:32/little, "WAVE", "fmt ", 16:32/little, 
              AudioFormat:16/little, Channels:16/little, Rate:32/little, _ByteRate:32/little,
              _BlockAlign:16/little, BitsPerSample:16/little, "data", _Sub2Size:32/little>>) ->
  #wav_header{audio = AudioFormat, channels = Channels, rate = Rate, bps = BitsPerSample}.

write_header(#wav_header{audio = AudioFormat, channels = Channels, rate = Rate, bps = BitsPerSample}) ->
  BlockAlign = BitsPerSample div 8,
  ByteRate = BlockAlign * Rate,
  DataSize = 0,
  TotalSize = DataSize + 36,
  <<"RIFF", TotalSize:32/little, "WAVE", "fmt ", 16:32/little,
    AudioFormat:16/little, Channels:16/little, Rate:32/little, ByteRate:32/little,
    BlockAlign:16/little, BitsPerSample:16/little, "data", DataSize:32/little>>.
  
              
              