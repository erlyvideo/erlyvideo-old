%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        Player module
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
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

-module(stream_play).
-author('Max Lapshin <max@maxidoors.ru>').

-include("../../include/ems.hrl").
-include_lib("erlyvideo/include/video_frame.hrl").

-export([start_link/2, client/1]).

-export([init/2, ready/1]).



-record(stream_player, {
  consumer,
  media_info,
	stream_id,
	paused = false,
	base_ts = undefined,
	send_audio = true,
	send_video = true,
	sent_video_decoder = false,
	sent_audio_decoder = false,
	synced = false
}).



start_link(MediaEntry, Options) ->
  {ok, spawn_link(?MODULE, init, [MediaEntry, Options])}.
  
client(Player) ->
  Ref = erlang:make_ref(),
  Player ! {client, self(), Ref},
  receive 
    {Info, Ref} -> Info
  after 100 ->
    {undefined, undefined}
  end.

  
init(MediaEntry, Options) ->
  Consumer = proplists:get_value(consumer, Options),
  % ?D({"Starting stream play for consumer", Consumer}),
  erlang:monitor(process, Consumer),
  erlang:monitor(process, MediaEntry),
  ?MODULE:ready(#stream_player{consumer = Consumer,
                      stream_id = proplists:get_value(stream_id, Options, undefined),
                      media_info = MediaEntry}).
  
	
ready(#stream_player{consumer = Consumer} = State) ->
  receive
    {client_buffer, _ClientBuffer} ->
      ?MODULE:ready(State);
      
    start ->
      erlang:yield(),
      ?MODULE:ready(State);
      
    {client, Pid, Ref} ->
      Pid ! {gen_fsm:sync_send_event(Consumer, info), Ref},
      ?MODULE:ready(State);
      
    pause ->
      ?D("Player paused"),
      ?MODULE:ready(State#stream_player{paused = true});

    resume ->
      ?D("Player resumed"),
      ?MODULE:ready(State#stream_player{paused = false});
      
    {send_audio, Audio} ->
      ?D({"Send audio", Audio}),
      ?MODULE:ready(State#stream_player{send_audio = Audio});

    {send_video, Video} ->
      ?D({"Send video", Video}),
      ?MODULE:ready(State#stream_player{send_video = Video});

    {seek, Timestamp} ->
      ?D({"Requested to seek in stream", Timestamp}),
      ?MODULE:ready(State);
      
    {data, Data} ->
      gen_fsm:send_event(Consumer, {send, Data}),
      ?MODULE:ready(State);

    #video_frame{} = Frame ->
      send_frame(State, Frame);
      
    eof ->
      ?D("MPEG TS finished"),
      ok;
    
    stop -> 
      ?D({"stream play stop", self()}),
      ok;
  
    exit ->
      % ?D({"stream play exit", self(), State#stream_player.media_info}),
      ok;
      
  	{tcp_closed, _Socket} ->
      error_logger:info_msg("~p Video player lost connection.\n", [self()]),
      ok;
    
    {'DOWN', _Ref, process, _Consumer, _Reason} ->
      ok;
    
  	Else ->
  	  ?D({"Unknown message", self(), Else}),
  	  ?MODULE:ready(State)
  end.

send_frame(#stream_player{sent_video_decoder = true} = Player, #video_frame{decoder_config = true, type = video}) ->
  ?MODULE:ready(Player);

send_frame(#stream_player{sent_audio_decoder = true} = Player, #video_frame{decoder_config = true, type = audio}) ->
  ?MODULE:ready(Player);

send_frame(#stream_player{synced = false} = Player, #video_frame{decoder_config = false, frame_type = frame}) ->
  ?MODULE:ready(Player);

send_frame(#stream_player{synced = false} = Player, #video_frame{decoder_config = false, frame_type = keyframe} = VideoFrame) ->
  send_frame(Player#stream_player{synced = true}, VideoFrame);



send_frame(#stream_player{base_ts = undefined} = Player, #video_frame{dts = Ts} = Frame) when is_number(Ts) andalso Ts > 0 ->
  send_frame(Player#stream_player{base_ts = Ts}, Frame);

send_frame(#stream_player{consumer = Consumer, stream_id = StreamId, base_ts = BaseTs} = Player, 
           #video_frame{dts = DTS1, pts = PTS1, decoder_config = Decoder, type = Type} = Frame) ->
  DTS2 = case BaseTs of
    undefined -> 0;
    _ when BaseTs < DTS1 -> DTS1 - BaseTs;
    _ -> 0
  end,
  PTS2 = case {PTS1, BaseTs} of
    {undefined, _} -> DTS2;
    {_, undefined} -> 0;
    _ when BaseTs < PTS1 -> PTS1 - BaseTs;
    _ -> 0
  end,
  Consumer ! Frame#video_frame{stream_id = StreamId, dts = DTS2, pts = PTS2},
  % ?D({"Frame", Type, round(DTS2), round(PTS2 - DTS2)}),
  Player1 = case {Decoder, Type} of
    {true, audio} -> Player#stream_player{sent_audio_decoder = true};
    {true, video} -> Player#stream_player{sent_video_decoder = true};
    _ -> Player
  end,
  ?MODULE:ready(Player1).


