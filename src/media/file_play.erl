%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        Player module
%%% @reference  See <a href="http://github.com/maxlapshin/erlyvideo" target="_top">http://github.com/maxlapshin/erlyvideo</a> for more information
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
-author('max@maxidoors.ru').

-include("../../include/ems.hrl").

-export([file_dir/0, file_format/1, start/1, start/2, client/1]).

-export([init/2, ready/1]).



-record(file_player, {
  consumer,
  media_info,
  client_buffer = ?MIN_CLIENT_BUFFER,
  sent_video_config = false,
  sent_audio_config = false,
  prepush = 0,
	stream_id,
	buffer,
	timer_start,
	timer_ref,
	playing_from = 0,
	ts_prev = 0,
	pos = 0,
	paused = false
}).



start(MediaEntry) -> start(MediaEntry, []).

start(MediaEntry, Options) ->
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
  ?D({"Starting file play for consumer", proplists:get_value(consumer, Options)}),
  ready(#file_player{consumer = proplists:get_value(consumer, Options),
                      stream_id = proplists:get_value(stream_id, Options, 1),
                      pos = undefined,
                      media_info = MediaEntry,
                      client_buffer = proplists:get_value(client_buffer, Options, 10000),
                      timer_start = erlang:now()}).
  
	
ready(#file_player{media_info = MediaInfo, 
                    consumer = Consumer, 
                    client_buffer = ClientBuffer,
                    timer_ref = Timer} = State) ->
  receive
    {client_buffer, ClientBuffer} -> 
      ready(State#file_player{client_buffer = ClientBuffer});
    start ->
      case file_media:metadata(MediaInfo) of
        undefined -> ok;
        MetaData -> gen_fsm:send_event(Consumer, {metadata, ?AMF_COMMAND_ONMETADATA, MetaData, 1})
      end,
    	self() ! play,
    	?D({"Player starting with pid", self(), MediaInfo}),
      ?MODULE:ready(State#file_player{prepush = ClientBuffer, paused = false});
      
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

    {seek, Timestamp} ->
      {Pos, NewTimestamp} = file_media:seek(MediaInfo, Timestamp),
      timer:cancel(Timer),
      % ?D({"Player seek to", Timestamp, Pos, NewTimestamp}),
      self() ! play,
      ?MODULE:ready(State#file_player{pos = Pos, ts_prev = NewTimestamp, playing_from = NewTimestamp, prepush = ClientBuffer});

    stop -> 
      ?D("Player stopping"),
      timer:cancel(Timer),
      ?MODULE:ready(State#file_player{ts_prev = 0, pos = undefined, playing_from = 0});
  
    exit ->
      ok;
      
    play ->
      % {_, Sec1, MSec1} = erlang:now(),
      play(State);
    	
  	{tcp_closed, _Socket} ->
      error_logger:info_msg("~p Video player lost connection.\n", [self()]),
      ok;
  	Else ->
  	  ?D({"Unknown message", Else}),
  	  ?MODULE:ready(State)
  end.



play(#file_player{paused = true} = State) ->
  ?MODULE:ready(State);


play(#file_player{sent_audio_config = false, media_info = MediaInfo} = Player) ->
  % ?D({"Sent audio config"}),
  send_frame(Player#file_player{sent_audio_config = true}, {ok, file_media:codec_config(MediaInfo, audio)});

play(#file_player{sent_video_config = false, media_info = MediaInfo} = Player) ->
  % ?D({"Sent video config"}),
  send_frame(Player#file_player{sent_video_config = true}, {ok, file_media:codec_config(MediaInfo, video)});
    

play(#file_player{media_info = MediaInfo, pos = Key} = Player) ->
  send_frame(Player, file_media:read_frame(MediaInfo, Key)).

send_frame(Player, {ok, undefined}) ->
  self() ! play,
  ?MODULE:ready(Player);
  
send_frame(#file_player{consumer = Consumer} = Player, {ok, done}) ->
  ?D("Video file finished"),
  gen_fsm:send_event(Consumer, {status, ?NS_PLAY_COMPLETE, 1}),
	?MODULE:ready(Player#file_player{sent_video_config = false, sent_audio_config = false, ts_prev = 0, pos = undefined});

send_frame(#file_player{consumer = Consumer, stream_id = StreamId} = Player, {ok, #video_frame{nextpos = NextPos} = Frame}) ->
  % ?D({"Frame", Key, Frame#video_frame.timestamp_abs, NextPos}),
  TimeStamp = Frame#video_frame.timestamp_abs - Player#file_player.ts_prev,
  ems_play:send(Consumer, Frame#video_frame{timestamp=TimeStamp, streamid = StreamId}),
  Player1 = timeout_play(Frame, Player),
  % ?D({"Frame", Consumer, Frame#video_frame.timestamp_abs, Player#file_player.timer_start, TimeStamp, Timeout}),
  NextState = Player1#file_player{ts_prev = Frame#video_frame.timestamp_abs, pos = NextPos},
  % {_, Sec2, MSec2} = erlang:now(),
  %       _Delta = (Sec2*1000 + MSec2) - (Sec1*1000 + MSec1),
  % ?D({"Read frame", Delta}),
  ?MODULE:ready(NextState).




%%-------------------------------------------------------------------------
%% @spec () -> FileName::string()
%% @doc retrieves FLV video file folder from application environment
%% @end
%%-------------------------------------------------------------------------	
file_dir() ->
  ems:get_var(file_dir, "/tmp").



file_format(Name) ->
  case filename:extension(Name) of
      ".flv" -> flv;
      ".FLV" -> flv;
      ".mp4" -> mp4;
      ".MP4" -> mp4;
      ".mov" -> mp4;
      ".mkv" -> mkv;
      ".MKV" -> mkv
  end.
  

%%-------------------------------------------------------------------------
%% @spec (AbsTime::integer(), TimerStart::integer(), ClientBuffer::integer()) -> [TimeOut::integer() | 0]
%% @doc calculates timeout to playback of next FLV Tag 
%% @end
%%-------------------------------------------------------------------------	

timeout_play(#video_frame{timestamp_abs = AbsTime}, #file_player{timer_start = TimerStart, client_buffer = ClientBuffer, playing_from = PlayingFrom, prepush = Prepush} = Player) ->
  SeekTime = AbsTime - PlayingFrom,
  Timeout = SeekTime - ClientBuffer - trunc(timer:now_diff(now(), TimerStart) / 1000),
  % ?D({"Timeout", Timeout, SeekTime, ClientBuffer, trunc(timer:now_diff(now(), TimerStart) / 1000)}),
  if
  (Prepush > SeekTime) ->
    self() ! play,
    Player#file_player{prepush = Prepush - SeekTime};
	(Timeout > 0) -> 
    Player#file_player{timer_ref = timer:send_after(Timeout, play)};
  true -> 
    self() ! play,
    Player
  end.

 
