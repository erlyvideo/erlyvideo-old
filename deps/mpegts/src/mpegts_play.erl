%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Output streamer of MPEG-TS
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
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
-module(mpegts_play).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-include_lib("erlmedia/include/video_frame.hrl").


-export([play/3, play/4, play/1]).

-record(http_player, {
  player,
  streamer,
  req,
  buffer = [],
  audio_buffer = [],
  audio_dts,
  interleave 
}).

-define(TIMEOUT, 6000).

play(Name, Player, Req) ->
  play(Name, Player, Req, []).

play(_Name, Player, Req, Options) ->
  % ?D({"Player starting", _Name, Player}),
  erlang:monitor(process,Player),
  MS1 = erlang:now(),
  Interleave = case proplists:get_value(interleave, Options) of
    Num when is_number(Num) andalso Num > 0 -> Num;
    _ -> false
  end,
  Streamer = #http_player{player = Player, interleave = Interleave, streamer = mpegts:init()},
  case proplists:get_value(buffered, Options) of
    true -> 
      #http_player{buffer = MPEGTSBuffer} = Streamer1 = ?MODULE:play(Streamer#http_player{buffer = []}),
      {_Streamer2, Padding} = mpegts:pad_continuity_counters(Streamer1#http_player.streamer),
      Buffer = [Padding|MPEGTSBuffer],
      Req:stream(head, [{"Content-Type", "video/MP2T"}, {"Content-Length", integer_to_list(iolist_size(Buffer))}]),
      MS2 = erlang:now(),
      Req:stream(lists:reverse(Buffer));
    _ ->
      Req:stream(head, [{"Content-Type", "video/mpeg2"}, {"Connection", "close"}]),
      MS2 = erlang:now(),
      ?MODULE:play(Streamer#http_player{req = Req})
  end,      
  % Req:stream(close),
  MS3 = erlang:now(),
  ?D({mpegts, _Name, time, timer:now_diff(MS2,MS1) div 1000, timer:now_diff(MS3,MS2) div 1000}),
  ok.

play(#http_player{} = Player) ->
  receive
    Message ->
      case handle_msg(Player, Message) of
        {ok, Player1} -> ?MODULE:play(Player1);
        {stop, Player1} -> 
          {ok, Player2} = flush_audio(Player1),
          Player2
      end
  after
    ?TIMEOUT ->
      ?D("MPEG TS player timeout, no frames received"),
      Player
  end.
  

handle_msg(#http_player{audio_dts = undefined} = Player, #video_frame{content = audio, dts = DTS} = Frame) ->
  handle_msg(Player#http_player{audio_dts = DTS}, Frame);

handle_msg(#http_player{} = Player, #video_frame{content = audio, flavor = config} = F) ->
  send_frame(Player, F);

handle_msg(#http_player{interleave = false} = Player, #video_frame{content = audio, codec = aac} = Frame) ->
  send_frame(Player, Frame);

handle_msg(#http_player{audio_buffer = Audio, streamer = Streamer, interleave = Interleave} = Player,
           #video_frame{content = audio, codec = aac, body = Body}) when length(Audio) < Interleave->
  % ?D({audio,length(Audio)+1}),
  ADTS = aac:pack_adts(Body, mpegts:audio_config(Streamer)),
  {ok, Player#http_player{audio_buffer = [ADTS|Audio]}};
  
handle_msg(#http_player{} = Player, #video_frame{content = audio, codec = aac} = Frame) ->
  {ok, Player1} = flush_audio(Player),
  handle_msg(Player1, Frame);

handle_msg(#http_player{} = HTTPPlayer, #video_frame{} = Frame) ->
  % ?D({mpegts,Frame#video_frame.codec,Frame#video_frame.flavor,round(Frame#video_frame.dts)}),
  send_frame(HTTPPlayer, Frame);

handle_msg(#http_player{streamer = Streamer} = State, {'DOWN', _, process, Pid, _}) ->
  ?D({"MPEG TS reader disconnected", Pid, Streamer}),
  {stop, State};

handle_msg(#http_player{} = State, {ems_stream, _,play_complete,_}) ->
  {stop, State};

handle_msg(#http_player{} = State, {tcp_closed, _Socket}) ->
  {stop, State};

handle_msg(Streamer, {ems_stream, _, _}) ->
  {ok, Streamer};
  
handle_msg(#http_player{} = Streamer, Message) ->
  ?D(Message),
  {ok, Streamer}.

flush_audio(#http_player{audio_buffer = Audio, audio_dts = DTS} = Player) ->
  % ?D({flush_adts, length(Audio)}),
  send_frame(Player#http_player{audio_buffer = [], audio_dts = undefined}, #video_frame{
    content = audio,
    codec = adts,
    flavor = frame,
    dts = DTS,
    pts = DTS,
    body = iolist_to_binary(lists:reverse(Audio))
  }).

send_frame(#http_player{req = Req, buffer = Buffer, streamer = Streamer} = HTTPPlayer, #video_frame{} = Frame) ->
  case mpegts:encode(Streamer, Frame) of
    {Streamer1, none} -> 
      {ok, HTTPPlayer#http_player{streamer = Streamer1}};
    {Streamer1, Bin} when Req == undefined ->
      {ok, HTTPPlayer#http_player{buffer = [Bin|Buffer], streamer = Streamer1}};
    {Streamer1, Bin} ->
      Req:stream(Bin),
      {ok, HTTPPlayer#http_player{streamer = Streamer1}}
  end.
  

