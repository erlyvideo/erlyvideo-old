% http://www.datavoyage.com/mpgscript/mpeghdr.htm
-module(mp3).
-author('Max Lapshin <max@maxidoors.ru>').

-export([decode/1]).

decode(<<16#FFF:12, 0:1, 2#01:2, 1:1, BitRate:4, SampleRate:2, Padding:1, 
         _Private:1, Channels:2, Joint:2, _Copyright:1, _Original:1, _Emphasise:2, _/binary>> = Packet) ->
  Length = 144 * BitRate / SampleRate + Padding,
  TotalLength = Length + 4,
  case Packet of
    <<Frame:TotalLength/binary, Rest/binary>> -> {ok, Frame, Rest};
    _ -> {more, undefined}
  end;
  
decode(_) ->
  {error, unknown}.

