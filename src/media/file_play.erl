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

-export([file_dir/0, file_format/1, start_link/1, start_link/2, client/1]).

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
  ready(#file_player{consumer = Consumer,
                      stream_id = proplists:get_value(stream_id, Options),
                      pos = undefined,
                      media_info = MediaEntry,
                      client_buffer = proplists:get_value(client_buffer, Options, 10000),
                      timer_start = erlang:now()}).
  
	
ready(#file_player{media_info = MediaInfo, 
                    consumer = Consumer, 
                    client_buffer = ClientBuffer,
                    stream_id = StreamId,
                    timer_ref = Timer} = State) ->
  receive
    {client_buffer, ClientBuffer} -> 
      ?MODULE:ready(State#file_player{client_buffer = ClientBuffer});
      
    start ->
      case file_media:metadata(MediaInfo) of
        undefined -> ok;
        MetaData -> gen_fsm:send_event(Consumer, {metadata, ?AMF_COMMAND_ONMETADATA, MetaData, StreamId})
      end,
    	self() ! play,
      ?MODULE:ready(State#file_player{prepush = ClientBuffer, stopped = false, paused = false});
      
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
      {Pos, NewTimestamp} = file_media:seek(MediaInfo, Timestamp),
      timer:cancel(Timer),
      % ?D({"Player seek to", Timestamp, Pos, NewTimestamp}),
      self() ! play,
      ?MODULE:ready(State#file_player{pos = Pos, ts_prev = NewTimestamp, playing_from = NewTimestamp, prepush = ClientBuffer});

    stop -> 
      ?D("Player stopping"),
      timer:cancel(Timer),
      ?MODULE:ready(State#file_player{ts_prev = 0, pos = undefined, stopped = true, playing_from = 0});
  
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


play(#file_player{sent_audio_config = false, media_info = MediaInfo} = Player) ->
  % ?D({"Sent audio config"}),
  send_frame(Player#file_player{sent_audio_config = true}, file_media:codec_config(MediaInfo, audio));

play(#file_player{sent_video_config = false, media_info = MediaInfo} = Player) ->
  % ?D({"Sent video config"}),
  send_frame(Player#file_player{sent_video_config = true}, file_media:codec_config(MediaInfo, video));
    

play(#file_player{media_info = MediaInfo, pos = Key} = Player) ->
  {Frame, Next} = file_media:read_frame(MediaInfo, Key),
  send_frame(Player#file_player{pos = Next}, Frame);
  
play(Else) ->
  ?D(Else),
  ok.

send_frame(Player, undefined) ->
  self() ! play,
  ?MODULE:ready(Player);
  
send_frame(#file_player{consumer = Consumer, stream_id = StreamId}, done) ->
  gen_fsm:send_event(Consumer, {status, ?NS_PLAY_COMPLETE, StreamId}),
  ok;

send_frame(#file_player{consumer = Consumer, stream_id = StreamId} = Player, #video_frame{} = Frame) ->
  ems_play:send(Consumer, Frame#video_frame{stream_id = StreamId}),
  Player1 = timeout_play(Frame, Player),
  ?MODULE:ready(Player1).




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
    self() ! play,
    Player#file_player{prepush = Prepush - SeekTime};
	(Timeout > 0) -> 
    Player#file_player{timer_ref = timer:send_after(Timeout, play)};
  true -> 
    self() ! play,
    Player
  end.

 
