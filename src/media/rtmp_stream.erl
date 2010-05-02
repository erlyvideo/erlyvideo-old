%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        Player module
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
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

-module(rtmp_stream).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../../include/ems.hrl").
-include_lib("erlmedia/include/video_frame.hrl").

-behaviour(gen_consumer).
% gen_consumer behaviour
-export([handle_frame/2, handle_control/2, handle_info/2, init/2, terminate/1]).

-export([start_link/1]).


-record(rtmp_stream, {
  host,
  consumer,
  stream_id,
  base_dts,
  name,
  bytes_sent = 0
}).

start_link(Options) ->
  gen_consumer:start_link(?MODULE, Options, []).

bin_size(#video_frame{type = Type, body = Body} = _Frame) when (Type == audio orelse Type == video) andalso (is_list(Body) orelse is_binary(Body)) ->
  iolist_size(Body);
  
bin_size(_) -> 0.
  

init(Options, []) ->
  Consumer = proplists:get_value(consumer, Options),
  erlang:monitor(process, Consumer),
  link(Consumer),
  StreamId = proplists:get_value(stream_id, Options),
  Host = proplists:get_value(host, Options),
  {ok, #rtmp_stream{consumer = Consumer, stream_id = StreamId, host = Host}}.


handle_frame(#video_frame{dts = DTS, pts = PTS} = Frame, #rtmp_stream{base_dts = undefined} = Stream) ->
  ?D({"Shifted rtmp_stream by", DTS}),
  handle_frame(Frame#video_frame{dts = 0, pts = PTS-DTS}, Stream#rtmp_stream{base_dts = DTS});
  
handle_frame(Frame, #rtmp_stream{consumer = Consumer, stream_id = StreamId, bytes_sent = Sent} = Stream) ->
  Consumer ! Frame#video_frame{stream_id = StreamId},
  {noreply, Stream#rtmp_stream{bytes_sent = Sent + bin_size(Frame)}}.

handle_control({notfound, Reason}, #rtmp_stream{consumer = Consumer, stream_id = StreamId} = Stream) ->
  Consumer ! {ems_stream, StreamId, {notfound, Reason}},
  {noreply, Stream};

handle_control({start_play, Name}, #rtmp_stream{host = Host, consumer = Consumer, stream_id = StreamId} = Stream) ->
  Consumer ! {ems_stream, StreamId, start_play},
  ems_event:user_play(Host, Consumer, Name),
  {noreply, Stream#rtmp_stream{name = Name}};
  
handle_control({play_complete, Length}, #rtmp_stream{consumer = Consumer, stream_id = StreamId} = Stream) ->
  Consumer ! {ems_stream, StreamId, play_complete, Length},
  notify_stats(Stream),
  {noreply, Stream};

handle_control(stop, #rtmp_stream{} = Stream) ->
  notify_stats(Stream),
  {noreply, Stream};

handle_control({seek_failed, _Timestamp}, #rtmp_stream{consumer = Consumer, stream_id = StreamId} = Stream) ->
  Consumer ! {ems_stream, StreamId, seek_failed},
  {noreply, Stream};

handle_control({seek_notify, Timestamp}, #rtmp_stream{consumer = Consumer, stream_id = StreamId} = Stream) ->
  Consumer ! {ems_stream, StreamId, seek_notify, Timestamp},
  {noreply, Stream}.


handle_info({client, Pid, Ref}, #rtmp_stream{consumer = Consumer} = Stream) ->
  Pid ! {gen_fsm:sync_send_event(Consumer, info), Ref},
  {noreply, Stream};

handle_info({'DOWN', _Ref, process, Consumer, _Reason}, #rtmp_stream{consumer = Consumer} = Stream)
 ->
  notify_stats(Stream),
  stop;

handle_info(Else, Stream) ->
  ?D({"Unknown message", Else}),
  {noreply, Stream}.
  
  
terminate(#rtmp_stream{} = Stream) ->
  notify_stats(Stream).
  
notify_stats(#rtmp_stream{host = Host, consumer = User, name = Name, bytes_sent = Sent}) ->
  ems_event:user_stop(Host, User, Name, Sent).
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

