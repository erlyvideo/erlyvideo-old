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


-export([play/3, play/4, play/5, play/1]).

-record(http_player, {
  player,
  streamer,
  req,
  buffer = []
}).

-define(TIMEOUT, 6000).

play(Name, Player, Req) ->
  play(Name, Player, Req, []).

play(Name, Player, Req, Options) ->
  play(Name, Player, Req, Options, {0,0,0,0}).

play(_Name, Player, Req, Options, Counters) ->
  ?D({"Player starting", _Name, Player}),
  link(Player),
  link(Req:socket_pid()),
  erlang:monitor(process,Player),
  erlang:monitor(process,Req:socket_pid()),
  Streamer = #http_player{player = Player, streamer = mpegts:init(Counters)},
  case proplists:get_value(buffered, Options) of
    true -> 
      {NextCounters, #http_player{buffer = Buffer}} = ?MODULE:play(Streamer#http_player{buffer = []}),
      ?D({iphone_segment, iolist_size(Buffer)}),
      Req:stream(head, [{"Content-Type", "video/MP2T"}, {"Connection", "close"}, {"Content-Length", integer_to_list(iolist_size(Buffer))}]),
      Req:stream(lists:reverse(Buffer));
    _ ->
      Req:stream(head, [{"Content-Type", "video/mpeg2"}, {"Connection", "close"}]),
      {NextCounters, _} = ?MODULE:play(Streamer#http_player{req = Req})
  end,      
  
  Req:stream(close),
  NextCounters.

play(#http_player{streamer = Streamer} = Player) ->
  receive
    Message -> handle_msg(Player, Message)
  after
    ?TIMEOUT ->
      ?D("MPEG TS player timeout, no frames received"),
      {mpegts:continuity_counters(Streamer), Player}
  end.

handle_msg(#http_player{req = Req, buffer = Buffer, streamer = Streamer} = HTTPPlayer, #video_frame{} = Frame) ->
  case mpegts:encode(Streamer, Frame) of
    {Streamer1, none} -> 
      ?MODULE:play(HTTPPlayer#http_player{streamer = Streamer1});
    {Streamer1, Bin} when Req == undefined ->
      ?MODULE:play(HTTPPlayer#http_player{buffer = [Bin|Buffer], streamer = Streamer1});
    {Streamer1, Bin} ->
      Req:stream(Bin),
      ?MODULE:play(HTTPPlayer#http_player{streamer = Streamer1})
  end;

handle_msg(#http_player{streamer = Streamer} = State, {'DOWN', _, process, Pid, _}) ->
  Counters = mpegts:continuity_counters(Streamer),
  ?D({"MPEG TS reader disconnected", Pid, Streamer, Counters}),
  {Counters, State};

handle_msg(#http_player{streamer = Streamer} = State, {ems_stream, _,play_complete,_}) ->
  {mpegts:continuity_counters(Streamer), State};

handle_msg(#http_player{} = Streamer, Message) ->
  ?D(Message),
  ?MODULE:play(Streamer).
