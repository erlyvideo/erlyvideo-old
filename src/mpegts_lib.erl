-module(mpegts_lib).
-author('Max Lapshin <max@maxidoors.ru>').

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-define(TIMEOUT, 4000).
-include_lib("erlmedia/include/video_frame.hrl").


-export([play/3, play/4, play/1]).

-record(http_player, {
  player,
  streamer,
  req
}).


play(Name, Player, Req) ->
  play(Name, Player, Req, {0,0,0,0}).

play(_Name, Player, Req, Counters) ->
  ?D({"Player starting", _Name, Player}),
  link(Player),
  link(Req:socket_pid()),
  erlang:monitor(process,Player),
  erlang:monitor(process,Req:socket_pid()),
  Player ! start,
  Streamer = #http_player{player = Player, req = Req, streamer = mpegts:init(Counters)},
  NextCounters = ?MODULE:play(Streamer),
  Req:stream(close),
  NextCounters.

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

handle_msg(#http_player{player = Player, req = Req, streamer = Streamer}, {'DOWN', _, process, Pid, _}) ->
  Counters = mpegts:continuity_counters(Streamer),
  ?D({"MPEG TS reader disconnected", Pid, Streamer, Counters}),
  % {_Streamer1, Bin} = mpegts:pad_continuity_counters(Streamer),
  % Req:stream(Bin),
  Player ! exit,
  Counters;

handle_msg(#http_player{player = Player, streamer = Streamer}, {ems_stream, _,play_complete,_}) ->
  Player ! exit,
  mpegts:continuity_counters(Streamer);

handle_msg(#http_player{} = Streamer, Message) ->
  ?D(Message),
  ?MODULE:play(Streamer).
