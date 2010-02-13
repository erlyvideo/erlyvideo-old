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

-module(file_play).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-author('Max Lapshin <max@maxidoors.ru>').

-include("../../include/ems.hrl").
-include_lib("erlyvideo/include/video_frame.hrl").

-export([file_dir/1, file_format/1, start_link/1, start_link/2, client/1]).

-export([init/2, ready/1, play/1]).



-record(file_player, {
  consumer,
  media_info,
  client_buffer,
  sent_video_config = false,
  sent_audio_config = false,
  prepush = 0,
  play_end = undefined,
  seek = undefined,
	stream_id,
	buffer,
	timer_start,
	timer_ref,
	playing_from = 0,
	ts_prev = 0,
	pos = 0,
	paused = false,
	stopped = false,
	send_audio = true,
	send_video = true
}).



start_link(MediaEntry) -> start_link(MediaEntry, []).

start_link(MediaEntry, Options) ->
  {ok, spawn_link(?MODULE, init, [MediaEntry, Options])}.
  
client(Player) ->
  Ref = erlang:make_ref(),
  Player ! {client, self(), Ref},
  receive 
    {Info, Ref} -> Info
  after 1000 ->
    {undefined, undefined, Player}
  end.

  
init(MediaEntry, Options) ->
  Consumer = proplists:get_value(consumer, Options),
  link(Consumer),
  Seek = proplists:get_value(seek, Options),
  PlayEnd = case proplists:get_value(duration_before, Options) of
    undefined -> undefined;
    Duration -> 
      case file_media:seek(MediaEntry, Seek + Duration) of
        {_Pos, NewTimestamp} -> NewTimestamp;
        _ -> undefined
      end
  end,
  ready(#file_player{consumer = Consumer,
                     stream_id = proplists:get_value(stream_id, Options),
                     pos = undefined,
                     media_info = MediaEntry,
                     seek = Seek,
                     play_end = PlayEnd,
                     client_buffer = proplists:get_value(client_buffer, Options, 10000),
                     timer_start = erlang:now()}).
  

ready(#file_player{media_info = MediaInfo, 
                    consumer = Consumer, 
                    client_buffer = ClientBuffer,
                    stream_id = StreamId,
                    timer_ref = Timer} = State) ->
  receive
    {client_buffer, NewClientBuffer} -> 
      ?MODULE:ready(State#file_player{client_buffer = NewClientBuffer});
      
    start ->
      case file_media:metadata(MediaInfo) of
        undefined -> ok;
        MetaData -> Consumer ! #video_frame{type = metadata, stream_id = StreamId, body = [<<?AMF_COMMAND_ONMETADATA>>, MetaData]}
      end,
      case State#file_player.seek of
        undefined -> 
        	self() ! play,
          ?MODULE:ready(State#file_player{prepush = ClientBuffer, stopped = false, paused = false});
        Seek ->
          ?D({"Seeking to", Seek}),
          self() ! {seek, Seek},
          ?MODULE:ready(State#file_player{prepush = ClientBuffer, stopped = false, paused = false, seek = undefined})
      end;
      
    {client, Pid, Ref} ->
      Pid ! {gen_fsm:sync_send_event(Consumer, info), Ref},
      ?MODULE:ready(State);
      
    pause ->
      ?D("Player paused"),
      timer:cancel(Timer),
      ?MODULE:ready(State#file_player{paused = true});

    resume ->
      ?D("Player resumed"),
      self() ! play,
      ?MODULE:ready(State#file_player{paused = false});
      
    {send_audio, Audio} ->
      ?D({"Send audio", Audio}),
      ?MODULE:ready(State#file_player{send_audio = Audio});

    {send_video, Video} ->
      ?D({"Send video", Video}),
      ?MODULE:ready(State#file_player{send_video = Video});

    {seek, Timestamp} ->
      case file_media:seek(MediaInfo, Timestamp) of
        {Pos, NewTimestamp} ->
          timer:cancel(Timer),
          ?D({"Player real seek to", round(Timestamp), NewTimestamp}),
          self() ! play,
          ?MODULE:ready(State#file_player{pos = Pos, 
                                          ts_prev = NewTimestamp, 
                                          playing_from = NewTimestamp, 
                                          prepush = ClientBuffer});
        undefined ->
          ?D({"Seek beyong current borders"}),
          ?MODULE:ready(State)
      end;

    stop -> 
      ok;
  
    exit ->
      ok;
      
    play ->
      play(State);
    	
  	{tcp_closed, _Socket} ->
      error_logger:info_msg("~p Video player lost connection.\n", [self()]),
      ok;
  	Else ->
  	  ?D({"Unknown message", Else}),
  	  ?MODULE:ready(State)
  end.


play(#file_player{stopped = true} = State) ->
  ?MODULE:ready(State);

play(#file_player{paused = true} = State) ->
  ?MODULE:ready(State);


play(#file_player{sent_audio_config = false, media_info = MediaInfo, pos = Pos} = Player) ->
  % ?D({"Sent audio config"}),
  send_frame(Player#file_player{sent_audio_config = true}, {file_media:codec_config(MediaInfo, audio), Pos});

play(#file_player{sent_video_config = false, media_info = MediaInfo, pos = Pos} = Player) ->
  % ?D({"Sent video config", file_media:codec_config(MediaInfo, video)}),
  send_frame(Player#file_player{sent_video_config = true}, {file_media:codec_config(MediaInfo, video), Pos});
    

play(#file_player{media_info = MediaInfo, pos = Key} = Player) ->
  Reply = file_media:read_frame(MediaInfo, Key),
  send_frame(Player, Reply);
  
play(Else) ->
  ?D(Else),
  ok.

send_frame(#file_player{play_end = PlayEnd}, {#video_frame{timestamp = Timestamp}, _}) when PlayEnd =< Timestamp ->
  ok;

send_frame(Player, undefined) ->
  self() ! play,
  ?MODULE:ready(Player);

send_frame(Player, {undefined, undefined}) ->
  self() ! play,
  ?MODULE:ready(Player);

send_frame(Player, {#video_frame{body = undefined}, Next}) ->
  self() ! play,
  ?MODULE:ready(Player#file_player{pos = Next});
  
send_frame(#file_player{} = Player, {done, undefined}) ->
  ok;

send_frame(#file_player{consumer = Consumer, stream_id = StreamId} = Player, done) ->
  ok;

send_frame(#file_player{consumer = Consumer, stream_id = StreamId} = Player, {#video_frame{} = Frame, Next}) ->
  Consumer ! Frame#video_frame{stream_id = StreamId},
  timeout_play(Frame, Player#file_player{pos = Next}).
  




%%-------------------------------------------------------------------------
%% @spec () -> FileName::string()
%% @doc retrieves FLV video file folder from application environment
%% @end
%%-------------------------------------------------------------------------	
file_dir(Host) ->
  ems:get_var(file_dir, Host, undefined).



file_format(Name) ->
  case filename:extension(Name) of
      ".flv" -> flv;
      ".FLV" -> flv;
      ".3gp" -> mp4;
      ".mp4" -> mp4;
      ".MP4" -> mp4;
      ".mov" -> mp4;
      ".m4v" -> mp4;
      ".mkv" -> mkv;
      ".MKV" -> mkv;
      _ -> flv
  end.
  

%%-------------------------------------------------------------------------
%% @spec (AbsTime::integer(), TimerStart::integer(), ClientBuffer::integer()) -> [TimeOut::integer() | 0]
%% @doc calculates timeout to playback of next FLV Tag 
%% @end
%%-------------------------------------------------------------------------	

timeout_play(#video_frame{timestamp = AbsTime}, #file_player{timer_start = TimerStart, client_buffer = ClientBuffer, playing_from = PlayingFrom, prepush = Prepush} = Player) ->
  SeekTime = AbsTime - PlayingFrom,
  Timeout = SeekTime - ClientBuffer - trunc(timer:now_diff(now(), TimerStart) / 1000),
  % ?D({"Timeout", Timeout, SeekTime, ClientBuffer, trunc(timer:now_diff(now(), TimerStart) / 1000)}),
  if
  (Prepush > SeekTime) ->
    ?MODULE:play(Player#file_player{prepush = Prepush - SeekTime});
	(Timeout > 0) -> 
    ?MODULE:ready(Player#file_player{timer_ref = timer:send_after(Timeout, play)});
  true -> 
    ?MODULE:play(Player)
  end.

 
