-module(media_ticker).
-include_lib("erlmedia/include/video_frame.hrl").

-export([start_link/3, init/3, loop/1, handle_message/2]).

-record(ticker, {
  media,
  consumer,
  stream_id,
  pos,
  dts,
  frame
}).


start_link(Media, Consumer, StreamId) ->
  proc_lib:start_link(?MODULE, init, [Media, Consumer, StreamId]).
  
init(Media, Consumer, StreamId) ->
  erlang:monitor(process, Media),
  erlang:monitor(process, Consumer),
  proc_lib:init_ack(Media, {ok, self()}),
  ?MODULE:loop(#ticker{media = Media, consumer = Consumer, stream_id = StreamId}).
  
loop(Ticker) ->
  receive
    Message ->
      ?MODULE:handle_message(Message, Ticker)
  end.

handle_message({'DOWN', _Ref, process, _Pid, _Reason}, _Ticker) ->
  ok;

handle_message(stop, _Ticker) ->
  ok;

handle_message(start, Ticker) ->
  self() ! tick,
  ?MODULE:loop(Ticker);

handle_message(tick, #ticker{media = Media, pos = undefined, consumer = Consumer} = Ticker) ->
  {ok, Frame} = ems_media:read_frame(Media, Consumer, undefined),
  #video_frame{dts = NewDTS, next_id = NewPos} = Frame,
  handle_message(tick, Ticker#ticker{pos = NewPos, dts = NewDTS, frame = Frame});
  
handle_message(tick, #ticker{media = Media, pos = Pos, dts = DTS, frame = PrevFrame, consumer = Consumer, stream_id = StreamId} = Ticker) ->
  Consumer ! PrevFrame#video_frame{stream_id = StreamId},
  {ok, Frame} = ems_media:read_frame(Media, Pos),
  #video_frame{dts = NewDTS, next_id = NewPos} = Frame,
  Ticker1 = Ticker#ticker{pos = NewPos, frame = Frame},
  Timeout = NewDTS - DTS,
  receive
    Message ->
      ?MODULE:handle_message(Message, Ticker1)
  after
    Timeout -> 
      self() ! tick,  
      ?MODULE:loop(Ticker1)
  end.


