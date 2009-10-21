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

-export([file_dir/0, file_format/1]).

-behaviour(gen_fsm).
-export([init/1, handle_info/3, code_change/4, handle_event/3, handle_sync_event/4, terminate/3]).
-export([ready/2, ready/3, stop/2]).

  
init({FileName, StreamId, #ems_fsm{client_buffer = ClientBuffer} = _State, Parent}) ->
  MediaEntry = media_provider:open(FileName),
  {ok, ready, #video_player{consumer = Parent,
	                          stream_id = StreamId,
	                          pos = undefined,
	                          media_info = MediaEntry,
	                          client_buffer = ClientBuffer,
	                          timer_start = erlang:now()}}.
  
	
stop(_, State) ->
  {stop, normal, State}.

ready({client_buffer, ClientBuffer}, State) ->
  {next_state, ready, State#video_player{client_buffer = ClientBuffer}};


ready({start}, #video_player{media_info = _MediaInfo, consumer = _Consumer, client_buffer = ClientBuffer} = State) ->
  % #media_info{format = FileFormat} = MediaInfo,
  % case FileFormat of
  %   mp4 -> gen_fsm:send_event(Consumer, {metadata, ?AMF_COMMAND_ONMETADATA, FileFormat:metadata(MediaInfo), 1});
  %   _ -> ok
  % end,
	Timer = gen_fsm:start_timer(1, play),
	NextState = State#video_player{timer_ref  = Timer, prepush = ClientBuffer},
	?D({"Player starting with pid", self()}),
  {next_state, ready, NextState};
  
ready({pause}, #video_player{timer_ref = Timer} = State) ->
  ?D("Player paused"),
  gen_fsm:cancel_timer(Timer),
  {next_state, ready, State};

ready({resume}, State) ->
  ?D("Player resumed"),
  {next_state, ready, State#video_player{timer_ref = gen_fsm:start_timer(1, play)}};

ready({seek, Timestamp}, #video_player{timer_ref = Timer, client_buffer = ClientBuffer, media_info = MediaEntry} = State) ->
  {Pos, NewTimestamp} = media_entry:seek(MediaEntry, Timestamp),
  gen_fsm:cancel_timer(Timer),
  % ?D({"Player seek to", Timestamp, Pos, NewTimestamp}),
  {next_state, ready, State#video_player{pos = Pos, ts_prev = NewTimestamp, timer_ref = gen_fsm:start_timer(0, play), playing_from = NewTimestamp, prepush = ClientBuffer}};

ready({stop}, #video_player{timer_ref = Timer, media_info = MediaEntry} = State) ->
  ?D("Player stopping"),
  gen_fsm:cancel_timer(Timer),
  {next_state, ready, State#video_player{ts_prev = 0, pos = media_entry:first(MediaEntry), playing_from = 0}};

ready({timeout, _, play}, #video_player{stream_id = StreamId, media_info = MediaEntry, consumer = Consumer} = State) ->
  {_, Sec1, MSec1} = erlang:now(),
	case media_entry:read(MediaEntry, State) of
		{ok, done} ->
		  ?D("Video file finished"),
		  gen_fsm:send_event(Consumer, {status, ?NS_PLAY_COMPLETE, 1}),
  		{next_state, ready, State};
		{ok, #video_frame{type = _Type} = Frame, Player} -> 
			TimeStamp = Frame#video_frame.timestamp_abs - State#video_player.ts_prev,
			ems_play:send(Consumer, Frame#video_frame{timestamp=TimeStamp, streamid = StreamId}),
			{Timeout, Player1} = timeout(Frame, Player),
      % ?D({"Frame", Frame#video_frame.timestamp_abs, Player#video_player.timer_start, TimeStamp, Timeout}),
			NextState = Player1#video_player{
			                  timer_ref = gen_fsm:start_timer(Timeout, play),
											  ts_prev = Frame#video_frame.timestamp_abs,
											  pos = Frame#video_frame.nextpos},
			{_, Sec2, MSec2} = erlang:now(),
      _Delta = (Sec2*1000 + MSec2) - (Sec1*1000 + MSec1),
      % ?D({"Read frame", Delta}),
			{next_state, ready, NextState};
		{error, _Reason} ->
			?D({"Ems player stopping", _Reason}),
			{stop, _Reason, State}
	end.



ready(file_name, _From, #video_player{media_info = MediaInfo} = State) ->
  {reply, media_entry:file_name(MediaInfo), ready, State}.







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
  
handle_event(Event, StateName, StateData) ->
  ?D({"Unknown event in player", Event, StateName}),
    {stop, {StateName, undefined_event, Event}, StateData}.


  
handle_sync_event(Event, _From, StateName, StateData) ->
  io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, got_sync_request2]),
  {stop, {StateName, undefined_event, Event}, StateData}.

handle_info({tcp_closed, _Socket}, _StateName,
            #video_player{} = StateData) ->
    error_logger:info_msg("~p Video player lost connection.\n", [self()]),
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
  ?D({"Unknown info in player", _Info, StateName}),
  {noreply, StateName, StateData}.

terminate(_Reason, _StateName, #video_player{media_info = #media_info{device = IoDev}} = _State) ->
  ?D("Video player exit"),
  file:close(IoDev),
  ok.
 
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

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

 
