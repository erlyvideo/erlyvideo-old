-module(media_ticker).
-include_lib("erlmedia/include/video_frame.hrl").

-export([start_link/3, init/3, loop/1, handle_message/2, handle_call/3]).
-export([seek/3]).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-record(ticker, {
  media,
  consumer,
  stream_id,
  pos,
  dts,
  frame
}).


seek(Ticker, Pos, DTS) ->
  Ticker ! {seek, Pos, DTS}.

start_link(Media, Consumer, Options) ->
  proc_lib:start_link(?MODULE, init, [Media, Consumer, Options]).
  
init(Media, Consumer, Options) ->
  erlang:monitor(process, Media),
  erlang:monitor(process, Consumer),
  proc_lib:init_ack({ok, self()}),
  StreamId = proplists:get_value(stream_id, Options),
  ?MODULE:loop(#ticker{media = Media, consumer = Consumer, stream_id = StreamId}).
  
loop(Ticker) ->
  receive
    Message ->
      ?MODULE:handle_message(Message, Ticker)
  end.


handle_call({seek, BeforeAfter, DTS}, _From, #ticker{media = Media} = Ticker) ->
  ?D({seek_request, DTS}),
  Seek = ems_media:seek(Media, BeforeAfter, DTS),
  ?D(Seek),
  {reply, ok, Ticker};

handle_call({start, _Consumer}, _From, Ticker) ->
  self() ! tick,
  {reply, ok, Ticker}.

handle_message({'$gen_call', From, Call}, Ticker) ->
  ?D({gen_call, Call, self()}),
  {reply, Reply, Ticker1} = ?MODULE:handle_call(Call, From, Ticker),
  gen_server:reply(From, Reply),
  ?MODULE:loop(Ticker1);
  

handle_message({'DOWN', _Ref, process, _Pid, _Reason}, _Ticker) ->
  ok;

handle_message(stop, _Ticker) ->
  ok;

handle_message(start, Ticker) ->
  self() ! tick,
  ?MODULE:loop(Ticker);
  
handle_message({seek, Pos, DTS}, #ticker{} = Ticker) ->
  ?D({seeked, Pos,DTS}),
  self() ! tick,
  ?MODULE:loop(Ticker#ticker{pos = Pos, dts = DTS, frame = undefined});

handle_message(tick, #ticker{media = Media, pos = Pos, frame = undefined, consumer = Consumer, stream_id = StreamId} = Ticker) ->
  Frame = ems_media:read_frame(Media, Pos),
  #video_frame{dts = NewDTS, next_id = NewPos} = Frame,
  ?D({preread_frame, NewPos,NewDTS}),
  Metadata = ems_media:metadata(Media),
  Meta = #video_frame{type = metadata, body = [<<"onMetaData">>, Metadata], dts = NewDTS, pts = NewDTS, stream_id = StreamId},
  Consumer ! Meta,
  self() ! tick,
  ?MODULE:loop(Ticker#ticker{pos = NewPos, dts = NewDTS, frame = Frame});
  
handle_message(tick, #ticker{media = Media, pos = Pos, dts = DTS, frame = PrevFrame, consumer = Consumer, stream_id = StreamId} = Ticker) ->
  Consumer ! PrevFrame#video_frame{stream_id = StreamId},
  % ?D({tick,Pos}),
  case ems_media:read_frame(Media, Pos) of
    eof ->
      Consumer ! {ems_stream, StreamId, play_complete, DTS},
      ok;
      
    #video_frame{dts = NewDTS, next_id = NewPos} = Frame ->
      Ticker1 = Ticker#ticker{pos = NewPos, dts = NewDTS, frame = Frame},
      Timeout = round(NewDTS - DTS),
      receive
        Message ->
          ?MODULE:handle_message(Message, Ticker1)
      after
        Timeout -> 
          self() ! tick,  
          ?MODULE:loop(Ticker1)
      end
  end.


