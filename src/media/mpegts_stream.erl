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

-module(mpegts_stream).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../../include/ems.hrl").
-include_lib("erlmedia/include/video_frame.hrl").

-behaviour(gen_consumer).
% gen_consumer behaviour
-export([handle_frame/2, handle_control/2, handle_info/2, init/2, terminate/1]).

-export([start_link/1]).


-record(mpegts_stream, {
  host,
  consumer,
  name
}).

start_link(Options) ->
  gen_consumer:start_link(?MODULE, Options, []).

init(Options, []) ->
  Consumer = proplists:get_value(consumer, Options),
  erlang:monitor(process, Consumer),
  link(Consumer),
  Host = proplists:get_value(host, Options),
  {ok, #mpegts_stream{consumer = Consumer, host = Host}}.


handle_frame(Frame, #mpegts_stream{consumer = Consumer} = Stream) ->
  Consumer ! Frame,
  {noreply, Stream}.
  





handle_control({notfound, _Name, Reason}, #mpegts_stream{consumer = Consumer} = Stream) ->
  Consumer ! {ems_stream, 1, {notfound, Reason}},
  {noreply, Stream};

handle_control({start_play, Name}, #mpegts_stream{host = Host, consumer = Consumer} = Stream) ->
  Consumer ! {ems_stream, 1, start_play},
  ems_event:user_play(Host, Consumer, Name),
  {noreply, Stream#mpegts_stream{name = Name}};
  
handle_control({play_complete, Length}, #mpegts_stream{consumer = Consumer} = Stream) ->
  Consumer ! {ems_stream, 1, play_complete, Length},
  {noreply, Stream};

handle_control(stop, #mpegts_stream{} = Stream) ->
  {noreply, Stream};

handle_control({seek_failed, _Timestamp}, #mpegts_stream{consumer = Consumer} = Stream) ->
  Consumer ! {ems_stream, 1, seek_failed},
  {noreply, Stream};

handle_control({seek_notify, Timestamp}, #mpegts_stream{consumer = Consumer} = Stream) ->
  Consumer ! {ems_stream, 1, seek_notify, Timestamp},
  {noreply, Stream}.


handle_info({client, Pid, Ref}, #mpegts_stream{consumer = Consumer} = Stream) ->
  Pid ! {gen_fsm:sync_send_event(Consumer, info), Ref},
  {noreply, Stream};

handle_info({'DOWN', _Ref, process, Consumer, _Reason}, #mpegts_stream{consumer = Consumer} = Stream)
 ->
  ?D({"Down consumer", Consumer}),
  stop;

handle_info(Else, Stream) ->
  ?D({"Unknown message", Else}),
  {noreply, Stream}.
  
  
terminate(#mpegts_stream{} = Stream) ->
  ok.
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

