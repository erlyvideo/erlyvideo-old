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

-export([file_dir/0, file_format/1, start/1, start/2]).

-export([init/2, ready/1]).


start(MediaEntry) -> start(MediaEntry, []).

start(MediaEntry, Options) ->
  {ok, spawn_link(?MODULE, init, [MediaEntry, Options])}.

  
init(MediaEntry, Options) ->
  ready(#video_player{consumer = proplists:get_value(consumer, Options),
	                          stream_id = proplists:get_value(stream_id, Options, 1),
	                          pos = undefined,
	                          media_info = MediaEntry,
	                          client_buffer = proplists:get_value(client_buffer, Options, 10000),
	                          timer_start = erlang:now()}).
  
	
ready(#video_player{media_info = MediaInfo, 
                    consumer = Consumer, 
                    client_buffer = ClientBuffer,
                    timer_ref = Timer,
                    stream_id = StreamId} = State) ->
  receive
    {client_buffer, ClientBuffer} -> ready(State#video_player{client_buffer = ClientBuffer});
    {start} ->
      case media_entry:metadata(MediaInfo) of
        undefined -> ok;
        MetaData -> gen_fsm:send_event(Consumer, {metadata, ?AMF_COMMAND_ONMETADATA, MetaData, 1})
      end,
    	self() ! {play},
    	NextState = State#video_player{prepush = ClientBuffer},
    	?D({"Player starting with pid", self(), MediaInfo}),
      ?MODULE:ready(NextState);
      
    {pause} ->
      ?D("Player paused"),
      timer:cancel(Timer),
      ?MODULE:ready(State);

    {resume} ->
      ?D("Player resumed"),
      self() ! {play},
      ?MODULE:ready(State);

    {seek, Timestamp} ->
      {Pos, NewTimestamp} = media_entry:seek(MediaInfo, Timestamp),
      timer:cancel(Timer),
      % ?D({"Player seek to", Timestamp, Pos, NewTimestamp}),
      self() ! {play},
      ?MODULE:ready(State#video_player{pos = Pos, ts_prev = NewTimestamp, playing_from = NewTimestamp, prepush = ClientBuffer});

    {stop} -> 
      ?D("Player stopping"),
      timer:cancel(Timer),
      ?MODULE:ready(State#video_player{ts_prev = 0, pos = media_entry:first(MediaInfo), playing_from = 0});
  
    {exit} ->
      ok;
      
    {play} ->
      % {_, Sec1, MSec1} = erlang:now(),
    	case media_entry:read(MediaInfo, State) of
    		{ok, done} ->
    		  ?D("Video file finished"),
    		  gen_fsm:send_event(Consumer, {status, ?NS_PLAY_COMPLETE, 1}),
      		?MODULE:ready(State);
    		{ok, #video_frame{type = _Type} = Frame, Player} -> 
    			TimeStamp = Frame#video_frame.timestamp_abs - State#video_player.ts_prev,
    			ems_play:send(Consumer, Frame#video_frame{timestamp=TimeStamp, streamid = StreamId}),
    			{Timeout, Player1} = timeout(Frame, Player),
          % ?D({"Frame", Frame#video_frame.timestamp_abs, Player#video_player.timer_start, TimeStamp, Timeout}),
    			NextState = Player1#video_player{
    			                  timer_ref = timer:send_after(Timeout, {play}),
    											  ts_prev = Frame#video_frame.timestamp_abs,
    											  pos = Frame#video_frame.nextpos},
          % {_, Sec2, MSec2} = erlang:now(),
          %       _Delta = (Sec2*1000 + MSec2) - (Sec1*1000 + MSec1),
          % ?D({"Read frame", Delta}),
          ?MODULE:ready(NextState);
    		{error, _Reason} ->
    			?D({"Ems player stopping", _Reason}),
    			erlang:error(_Reason)
    	end;
    	
    	{tcp_closed, _Socket} ->
        error_logger:info_msg("~p Video player lost connection.\n", [self()]),
        ok;
    	Else ->
    	  ?D({"Unknown message", Else}),
    	  ?MODULE:ready(State)
    end.



% ready(file_name, _From, #video_player{media_info = MediaInfo} = State) ->
%   {reply, media_entry:file_name(MediaInfo), ready, State}.







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

timeout(#video_frame{timestamp_abs = AbsTime}, #video_player{timer_start = TimerStart, client_buffer = ClientBuffer, playing_from = PlayingFrom, prepush = Prepush} = Player) ->
  SeekTime = AbsTime - PlayingFrom,
  Timeout = SeekTime - ClientBuffer - trunc(timer:now_diff(now(), TimerStart) / 1000),
  % ?D({"Timeout", Timeout, SeekTime, ClientBuffer, trunc(timer:now_diff(now(), TimerStart) / 1000)}),
  if
  (Prepush > SeekTime) ->
    {0, Player#video_player{prepush = Prepush - SeekTime}};
	(Timeout > 0) -> 
    {Timeout, Player}; 
  true -> 
    {0, Player}
  end.

 
