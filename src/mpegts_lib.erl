-module(mpegts_lib).
-author('Max Lapshin <max@maxidoors.ru>').

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-define(TIMEOUT, 4000).
-include_lib("erlmedia/include/video_frame.hrl").


-export([play/3, play/1]).

-record(http_player, {
  player,
  streamer,
  req
}).



play(_Name, Player, Req) ->
  ?D({"Player starting", _Name, Player}),
  process_flag(trap_exit, true),
  link(Player),
  link(Req:socket_pid()),
  Player ! start,
  Streamer = #http_player{player = Player, req = Req, streamer = mpegts:init()},
  ?MODULE:play(Streamer),
  Req:stream(close),
  ok.

play(#http_player{player = Player} = Streamer) ->
  receive
    Message -> handle_msg(Streamer, Message)
  after
    ?TIMEOUT ->
      ?D("MPEG TS player stopping"),
      Player ! stop,
      ok
  end.

handle_msg(#http_player{req = Req, streamer = Streamer} = HTTPPlayer, #video_frame{} = Frame) ->
  case mpegts:encode(Streamer, Frame) of
    {Streamer1, none} -> 
      ?MODULE:play(HTTPPlayer#http_player{streamer = Streamer1});
    {Streamer1, Bin} ->
      Req:stream(Bin),
      ?MODULE:play(HTTPPlayer#http_player{streamer = Streamer1})
  end;

handle_msg(#http_player{player = Player, req = Req, streamer = Streamer}, {'EXIT', _, _}) ->
  ?D({"MPEG TS reader disconnected", Streamer}),
  {_Streamer1, Bin} = mpegts:pad_continuity_counters(Streamer),
  Req:stream(Bin),
  Player ! stop,
  ok;

handle_msg(#http_player{} = Streamer, Message) ->
  ?D(Message),
  ?MODULE:play(Streamer).
