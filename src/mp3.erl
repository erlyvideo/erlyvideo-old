% http://www.datavoyage.com/mpgscript/mpeghdr.htm
-module(mp3).
-author('Max Lapshin <max@maxidoors.ru>').
-author('Alexander Songe <a@songe.me>').

-export([decode/1]).

decode(Header) when size(Header) < 4 ->
  {more, undefined};
decode(<<2#11111111111:11, VsnBits:2, LayerBits:2, _:1, BitRate:4, SampleRate:2, Padding:1, 
         _Private:1, Channels:2, _Joint:2, _Copyright:1, _Original:1, _Emphasise:2, _/binary>> = Packet) ->
  Layer = layer(LayerBits),
  Version = version(VsnBits),
  try framelength(bitrate({Version,Layer}, BitRate), samplerate({Version,Layer}, SampleRate), Padding, Channels) of
    Length when is_integer(Length) ->
      case Packet of
        <<Frame:Length/binary, Rest/binary>> -> {ok, Frame, Rest};
        _ -> {more, undefined}
      end
  catch
    Error ->
      io:format("~p~n", [[{error, Error},{version, Version},{layer, Layer},{bitrate, BitRate},{samplerate, SampleRate},{padding,Padding},{channels,Channels}]]),
      {error, unknown}
  end;

decode(_) ->
  {error, unknown}.

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

% Support for mono mp3 streams
framelength(Bitrate, Samplerate, Padding, 3) ->
  72 * Bitrate div Samplerate + Padding;
% mono * 2
framelength(Bitrate, Samplerate, Padding, _) ->
  144 * Bitrate div Samplerate + Padding.

samplerate({1,_}, Samplerate) ->
  element(Samplerate+1, {44100, 48000, 32000, reserved});
samplerate({2,_}, Samplerate) when is_integer(Samplerate) andalso Samplerate < 4 ->
  element(Samplerate+1, {22050, 24000, 16000, reserved});
samplerate({{2,5},_}) ->
  element(Samplerate+1, {32000, 16000, 8000, reverved}).

layer(0) -> reserved;
layer(1) -> 3;
layer(2) -> 2;
layer(3) -> 1.

version(0) -> {2,5};
version(1) -> reserved;
version(2) -> 2;
version(3) -> 1.
