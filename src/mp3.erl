% http://www.datavoyage.com/mpgscript/mpeghdr.htm
-module(mp3).
-author('Max Lapshin <max@maxidoors.ru>').
-author('Alexander Songe <a@songe.me>').

-export([decode/1]).

decode(Header) when size(Header) < 4 ->
  {more, undefined};
decode(<<16#FFF:12, 0:1, 2#01:2, 1:1, BitRate:4, SampleRate:2, Padding:1, 
         _Private:1, Channels:2, _Joint:2, _Copyright:1, _Original:1, _Emphasise:2, _/binary>> = Packet) ->
  Length = framelength(bitrate({2,3}, BitRate), samplerate({2,3}, SampleRate), Padding, Channels),
  case Packet of
    <<Frame:Length/binary, Rest/binary>> -> {ok, Frame, Rest};
    _ -> {more, undefined}
  end;
  
decode(_) ->
  {error, unknown}.

bitrate({{2,5},3}, Bitrate) ->
  bitrate({2,3}, Bitrate);
bitrate({2,3}, Bitrate) when Bitrate < 16 ->
  element(Bitrate+1, {free, 8000, 16000, 24000, 32000, 40000, 48000, 56000, 64000,
                      80000, 96000, 112000, 128000, 144000, 160000, 176000, 192000, badbitrate}).

% Support for mono mp3 streams
framelength(Bitrate, Samplerate, Padding, 3) ->
  72 * Bitrate div Samplerate + Padding;
% mono * 2
framelength(Bitrate, Samplerate, Padding, _) ->
  2*framelength(Bitrate, Samplerate, Padding, 3).

samplerate({2,_}, Samplerate) when is_integer(Samplerate) andalso Samplerate < 4 ->
  element(Samplerate+1, {22050, 24000, 16000, reserved}).
