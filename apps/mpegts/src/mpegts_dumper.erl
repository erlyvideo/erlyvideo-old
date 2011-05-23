%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @private
%%% @doc        MPEG-TS dump tool
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
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
-module(mpegts_dumper).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("mpegts/include/mpegts.hrl").

-compile(export_all).

dump_pes(Reader, PES) ->
  {ok, Reader1, Frames} = mpegts_reader:decode_pes(Reader, PES),
  {Reader1, length(Frames)}.

% dump_pes(Reader, #pes_packet{codec = Codec, dts = DTS, body = Body} = PES) ->
%   io:format("PES(~p) ~p, ~p~n", [Codec, round(DTS), size(Body)]),
%   {ok, Reader1, Frames} = mpegts_reader:decode_pes(Reader, PES),
%   [dump_frame(Frame) || Frame <- Frames],
%   {Reader1, length(Frames)}.

dump_frames(_, _, Count) when Count > 30000 ->
  {ok, Count};

dump_frames(File, Reader, Count) ->
  case file:read(File, 188) of
    {ok, <<16#47, Bin/binary>>} ->
      case mpegts_reader:decode_ts(Bin, Reader) of
        {ok, Reader1, undefined} ->
          dump_frames(File, Reader1, Count);
        {ok, Reader1, PES} ->
          {Reader2, Cnt} = dump_pes(Reader1, PES),
          dump_frames(File, Reader2, Count + Cnt)
      end;
    eof ->
      {ok, Reader1, PES1} = mpegts_reader:decode_ts({eof, h264}, Reader),
      {Reader2, _} = dump_pes(Reader1, PES1),
      
      {ok, Reader3, PES2} = mpegts_reader:decode_ts({eof, aac}, Reader2),
      {_Reader2, _} = dump_pes(Reader3, PES2),
      
      {ok, Count}
  end.            

dump_frame(#video_frame{flavor = keyframe, codec = h264, dts = DTS, body = Body}) ->
  io:format("  h264(keyframe) ~p(~p)~n", [round(DTS), iolist_size(Body)]);

dump_frame(#video_frame{flavor = Flavor, codec = Codec, dts = DTS, body = Body}) ->
  io:format("  ~p(~p): ~p (~p)~n", [Codec, Flavor, round(DTS), iolist_size(Body)]),
  ok.
