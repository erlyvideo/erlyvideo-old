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

-define(D(X), io:format("~p~n", [X])).

-compile(export_all).

% dump_pes(Reader, PES) ->
%   {ok, Reader1, Frames} = mpegts_reader:decode_pes(Reader, PES),
%   {Reader1, length(Frames)}.

dump_pes(Reader, PES) ->
  % PesStart = case Body of
  %   <<PesStart_:20/binary, _/binary>> -> PesStart_;
  %   _ -> Body
  % end,
  % io:format("PES(~p) ~p, ~p ~p~n", [Codec, round(DTS), size(Body), PesStart]),
  io:format("PES(~p) ~p, ~p~n", [PES#pes_packet.codec, round(PES#pes_packet.dts), size(PES#pes_packet.body)]),
  case PES#pes_packet.codec of
    h264 -> dump_avc(PES#pes_packet.body);
    _ -> ok
  end,
  {ok, Reader1, Frames} = mpegts_reader:decode_pes(Reader, PES),
  [dump_frame(Frame) || Frame <- Frames],
  {Reader1, length(Frames)}.

dump_avc(Body) ->
  dump_avc(Body, []).

dump_avc(Body, Acc) ->
  case mpegts_reader:extract_nal(Body) of
    undefined -> 
      NALS = lists:reverse(Acc),
      NextNal = case Body of
        <<1:24, Rest/binary>> -> Rest;
        <<1:32, Rest/binary>> -> Rest;
        _ -> <<>>
      end,
      Dump = "  " ++ [io_lib:format("NAL(~p) ~p, ", [h264:type(NAL), size(NAL)]) || NAL <- NALS] ++ 
      io_lib:format("left: ~p (~p)~n", [size(NextNal), case NextNal of <<>> -> empty ; _ -> h264:type(NextNal) end]),
      io:format(Dump);
    {ok, NAL, Rest} ->
      dump_avc(Rest, [NAL|Acc])
  end.
    

% dump_frames(_, _, Count) when Count > 30000 ->
%   {ok, Count};

dump_frames(File, Reader, Count) ->
  put(start_time, erlang:now()),
  dump_frames(File, 0, Reader, Count).

dump_frames(File, Position, Reader, Count) ->
  PrevIOCount = get(prev_io_count),
  if
    Count rem 1000 == 0 andalso Count > 0 andalso Count =/= PrevIOCount -> 
      put(prev_io_count, Count),
      DeltaTime = timer:now_diff(erlang:now(), get(start_time)) div 1000,
      ?D({Count, DeltaTime, Count / DeltaTime, Position div (DeltaTime*1000)});
    true -> ok
  end,
  case file:pread(File, Position, 188) of
    {ok, <<16#47, Bin/binary>>} ->
      % <<_Error:1, PayloadStart:1, _TransportPriority:1, Pid:13, _/binary>> = Bin,
      % io:format("TS: ~p (~p) ~s~n", [Pid, PayloadStart, mpegts_reader:adapt_field_info(Bin)]),
      case mpegts_reader:decode_ts(Bin, Reader) of
        {ok, Reader1, undefined} ->
          dump_frames(File, Position+188, Reader1, Count);
        {ok, Reader1, PES} ->
          % ?D({read_pes, PES}),
          {Reader2, Cnt} = dump_pes(Reader1, PES),
          dump_frames(File, Position+188, Reader2, Count + Cnt)
      end;
    {ok, <<_/binary>>} ->
      ?D(desync),
      dump_frames(File, Position + 1, Reader, Count);
    eof ->
      {ok, Reader1, PES1} = mpegts_reader:decode_ts({eof, h264}, Reader),
      Reader2 = case PES1 of
        undefined -> Reader1;
        _ ->
          % ?D({old_pes,PES1}),
          {__Reader2, _} = dump_pes(Reader1, PES1),
          __Reader2
      end,
      
      {ok, Reader3, PES2} = mpegts_reader:decode_ts({eof, aac}, Reader2),
      case PES2 of
        undefined -> ok;
        _ -> dump_pes(Reader3, PES2)
      end,
      
      {ok, Count}
  end.            

% dump_frame(#video_frame{flavor = keyframe, codec = h264, dts = DTS, body = Body}) ->
%   io:format("  h264(keyframe) ~p(~p)~n", [round(DTS), iolist_size(Body)]);

dump_frame(#video_frame{flavor = config, codec = aac, dts = DTS, body = Body}) ->
  io:format("  aac(config) ~p(~p)~n", [round(DTS), Body]);

dump_frame(#video_frame{flavor = Flavor, codec = Codec, dts = DTS, body = Body}) ->
  io:format("  ~p(~p): ~p (~p)~n", [Codec, Flavor, round(DTS), iolist_size(Body)]),
  ok.
