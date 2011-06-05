%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @author     Alexander Songe <a@songe.me>
%%% @copyright  2010 Max Lapshin
%%% @doc        Module to read mp3 files
%%% http://www.datavoyage.com/mpgscript/mpeghdr.htm
%%% http://www.hydrogenaudio.org/forums/lofiversion/index.php/t43172.html
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
-module(mp3).
-author('Max Lapshin <max@maxidoors.ru>').
-author('Alexander Songe <a@songe.me>').
-include("../include/mp3.hrl").
-include("log.hrl").

-export([decode/1]).

-export([header_size/0, read/1, frame_length/1]).

header_size() -> 4.

frame_length(<<2#11111111111:11, VsnBits:2, LayerBits:2, _:1, BitRate:4, SampleRate:2, Padding:1, 
         _Private:1, _Channels:2, _Joint:2, _Copyright:1, _Original:1, _Emphasise:2, _/binary>>) ->
  Layer = layer(LayerBits),
  Version = version(VsnBits),
  Length = framelength(Version, Layer, bitrate({Version,Layer}, BitRate), samplerate({Version,Layer}, SampleRate), Padding),
  true = is_integer(Length),
  Length.

read(<<2#11111111111:11, VsnBits:2, LayerBits:2, _:1, BitRate:4, SampleRate:2, _Padding:1, 
     _Private:1, Channels:2, Joint:2, _Copyright:1, _Original:1, _Emphasise:2, _/binary>> = Packet) ->
  Length = frame_length(Packet),
  Layer = layer(LayerBits),
  Version = version(VsnBits),
  case Packet of
    <<Frame:Length/binary, Rest/binary>> -> 
      BRate = bitrate({Version,Layer}, BitRate),
      SRate = samplerate({Version,Layer}, SampleRate),
      Samples = samples({Version,Layer}),
      Chan = decode_channels(Channels),
      {ok, #mp3_frame{sample_rate = SRate, bitrate = BRate, channels = Chan, joint = Joint, samples = Samples, body = Frame}, Rest};
    _ ->
      {more, Length - size(Packet)}
  end;

read(<<2#11111111111:11, _:5, _/binary>>) ->
  {more, 2};

read(<<_, Rest/binary>>) ->
  read(Rest);

read(<<>>) ->
  {more, 4}.

decode(Header) when size(Header) < 4 ->
  {more, undefined};
decode(<<2#11111111111:11, VsnBits:2, LayerBits:2, _:1, BitRate:4, SampleRate:2, Padding:1, 
         _Private:1, Channels:2, _Joint:2, _Copyright:1, _Original:1, _Emphasise:2, _/binary>> = Packet) ->
  Layer = layer(LayerBits),
  Version = version(VsnBits),
  try framelength(Version, Layer, bitrate({Version,Layer}, BitRate), samplerate({Version,Layer}, SampleRate), Padding) of
    Length when is_integer(Length) ->
      case Packet of
        <<Frame:Length/binary, Rest/binary>> -> {ok, Frame, Rest};
        _ -> {more, undefined}
      end;
    _ ->
      {error, unknown}
  catch
    Error ->
      io:format("~p~n", [[{error, Error},{version, Version},{layer, Layer},{bitrate, BitRate},{samplerate, SampleRate},{padding,Padding},{channels,Channels}]]),
      {error, unknown}
  end;

decode(_) ->
  {error, unknown}.


decode_channels(0) -> 2;
decode_channels(1) -> 2;
decode_channels(2) -> 2;
decode_channels(3) -> 1.

% 2.5 and 2 have the same bitrate table
bitrate({{2,5}, Sub},Bitrate) ->
  bitrate({2, Sub}, Bitrate);
% Layer 2 and 3 on version 2 and 2.5 have the same bitrate table
bitrate({2,2}, Bitrate) ->
  bitrate({2,3}, Bitrate);
bitrate({1,1},Bitrate) ->
  element(Bitrate+1, {free, 32000, 64000, 96000, 128000, 160000, 192000, 224000, 256000,
                      288000, 320000, 352000, 384000, 416000, 448000, badbitrate});
bitrate({1,2},Bitrate) ->
  element(Bitrate+1, {free, 32000, 48000, 56000, 64000, 80000, 96000, 112000, 128000,
                      160000, 192000, 224000, 256000, 320000, 384000, badbitrate});
bitrate({1,3},Bitrate) ->
  element(Bitrate+1, {free, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000,
                      128000, 160000, 192000, 224000, 256000, 320000, badbitrate});
bitrate({2,1}, Bitrate) ->
  element(Bitrate+1, {free, 32000, 48000, 56000, 64000, 80000, 96000, 112000, 128000,
                      144000, 160000, 176000, 192000, 224000, 256000, badbitrate});
bitrate({2,3}, Bitrate) when Bitrate < 16 ->
  element(Bitrate+1, {free, 8000, 16000, 24000, 32000, 40000, 48000, 56000, 64000,
                      80000, 96000, 112000, 128000, 144000, 160000, badbitrate}).

framelength(_, _, Bitrate, _, _) when is_atom(Bitrate) ->
  {error, bitrate, Bitrate};
framelength(_, _, _, Samplerate, _) when is_atom(Samplerate) ->
  {error, samplerate, Samplerate};
% Layer 2.5 and 2 are the same. 
framelength({2,5}, Layer, Bitrate, Samplerate, Padding) ->
  framelength(2, Layer, Bitrate, Samplerate, Padding);
% Layer 1 is always 48
framelength(_Version, 1, Bitrate, Samplerate, Padding) ->
  48 * Bitrate div Samplerate + Padding * 4;
% Version 2, Layer 2 is always 72
framelength(2, 3, Bitrate, Samplerate, Padding) ->
  72 * Bitrate div Samplerate + Padding;
% Everything else is 72.
framelength(_, _, Bitrate, Samplerate, Padding) ->
  144 * Bitrate div Samplerate + Padding.
  
samples({0,1}) -> 192;
samples({0,_}) -> 576;
samples({1,1}) -> 384;
samples({1,_}) -> 1152;
samples({2,1}) -> 96;
samples({2,_}) -> 288.

samplerate({1,_}, Samplerate) ->
  element(Samplerate+1, {44100, 48000, 32000, reserved});
samplerate({2,_}, Samplerate) when is_integer(Samplerate) andalso Samplerate < 4 ->
  element(Samplerate+1, {22050, 24000, 16000, reserved});
samplerate({{2,5},_}, Samplerate) ->
  element(Samplerate+1, {32000, 16000, 8000, reverved}).

layer(0) -> reserved;
layer(1) -> 3;
layer(2) -> 2;
layer(3) -> 1.

version(0) -> {2,5};
version(1) -> reserved;
version(2) -> 2;
version(3) -> 1.
