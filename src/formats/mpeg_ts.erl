%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        MPEG TS stream module
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%%
%%% Copyright (c) 2009 Max Lapshin
%%%    This program is free software: you can redistribute it and/or modify
%%%    it under the terms of the GNU Affero General Public License as
%%%    published by the Free Software Foundation, either version 3 of the
%%%    License, or any later version.
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

-module(mpeg_ts).
-author('max@maxidoors.ru').
-include("../../include/ems.hrl").
-include("../../include/video_frame.hrl").

-export([play/3, play/1]).



play(_Name, Player, Req) ->
  ?D({"Player starting", _Name, Player}),
  Req:stream(head, [{"Content-Type", "video/mpeg2"}, {"Connection", "close"}]),
  Req:stream(<<"MPEG TS\r\n\n\n">>),
  process_flag(trap_exit, true),
  link(Req:socket_pid()),
  Player ! start,
  send_pat(Req),
  % ?D({"MPEG TS", Req}),
  Req:stream(close),
  ok.

mux(Data, Req, Pid) ->
  Start = 1,
  Counter = 0,
  mux_parts(Data, Req, Pid, Start, Counter).
  
  
% 4 bytes header, 188 packet, so data is 184
mux_parts(<<Data:184/binary, Rest/binary>>, Req, Pid, Start, Counter) ->
  TEI = 0,
  Priority = 0,
  Scrambling = 0,
  Adapt = 0,
  HasPayload = 1,
  % Adaptation = <<>>,
  % (size(Adaptation)), Adaptation/binary
  Part = <<16#47, TEI:1, Start:1, Priority:1, Pid:13, Scrambling:2, Adapt:1, HasPayload:1, Counter:4, Data/binary>>,
  Req:stream(Part),
  mux_parts(Rest, Req, Pid, 0, Counter+1);
  
mux_parts(Data, Req, Pid, Start, Counter) ->
  TEI = 0,
  Priority = 0,
  Scrambling = 0,
  Adapt = 1,
  HasPayload = 1,
  Adaptation = <<>>,
  Part = <<16#47, TEI:1, Start:1, Priority:1, Pid:13, Scrambling:2, Adapt:1, HasPayload:1, Counter:4, (size(Adaptation)), Adaptation/binary, Data/binary>>,
  Req:stream(Part);

mux_parts(<<>>, Req, _Pid, _Start, _Counter) ->
  ok.
    
  
send_pat(Req) ->
  PAT = <<1:16, 0:3, 100:13>>,
  Length = size(PAT)+5,
  mux(<<0, 0, 2#10:2, 2#11:2, Length:12, 0:16, 16#040000:24, PAT/binary>>, Req, 0),
  ?MODULE:play(Req).
  
  
play(Req) ->
  receive
    #video_frame{} = Frame ->
      Req:stream(<<"frame\n">>),
      ?MODULE:play(Req);
    {'EXIT', _, _} ->
      ?D({"MPEG TS reader disconnected"}),
      ok;
    Message -> 
      ?D(Message),
      ?MODULE:play(Req)
  after
    ?TIMEOUT ->
      ?D("MPEG TS player stopping"),
      ok
  end.
  
  

  
  
  
  
  
  
  