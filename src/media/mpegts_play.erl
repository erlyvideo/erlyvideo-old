-module(mpegts_play).
-author('Max Lapshin <max@maxidoors.ru>').

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-define(TIMEOUT, 4000).
-include_lib("erlmedia/include/video_frame.hrl").


-export([play/3, play/4, play/5, play/1]).

-record(http_player, {
  player,
  streamer,
  req,
  buffer = []
}).

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
