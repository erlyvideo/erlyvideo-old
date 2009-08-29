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

-module(ems_play).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-author('max@maxidoors.ru').

-import(ems_mp4).
-import(ems_flv).
-include("../include/ems.hrl").

-export([play/3, file_dir/0, normalize_filename/1, channel_id/2]).

-behaviour(gen_fsm).
-export([init/1, handle_info/3, code_change/4, handle_event/3, handle_sync_event/4, terminate/3]).
-export([ready/2, stop/2]).


play(FileName, StreamId, _) ->
  gen_fsm:start_link(?MODULE, {FileName, StreamId, self()}, []).
  
  
init({FileName, StreamId, Parent}) ->
	{ok, IoDev} = file:open(FileName, [read, read_ahead]),
	FileFormat = file_format(FileName),
	case FileFormat:init(#video_player{device = IoDev, 
	                                   file_name = FileName,
	                                   consumer = Parent,
	                                   stream_id = StreamId,
	                                   format = FileFormat}) of
		{ok, VideoPlayerState} -> 
      {ok, ready, VideoPlayerState#video_player{timer_start = erlang:now()}};
    _HdrError -> 
		  ?D(_HdrError),
		  {error, "Invalid header"}
	end.
	
stop(_, State) ->
  {stop, normal, State}.
  
ready({start}, #video_player{} = State) ->
	Timer = gen_fsm:start_timer(1, play),
	NextState = State#video_player{timer_ref  = Timer},
	?D({"Player starting with pid", self()}),
  {next_state, ready, NextState, ?TIMEOUT};
  
ready({pause}, #video_player{timer_ref = Timer} = State) ->
  gen_fsm:cancel_timer(Timer),
  {next_state, ready, State, ?TIMEOUT};

ready({timeout, _, play}, #video_player{device = IoDev, stream_id = StreamId, format = FileFormat, consumer = Consumer} = State) ->
	case FileFormat:read_frame(State) of
		{ok, done} ->
		  ?D("Video file finished"),
  		file:close(IoDev),
  		{stop, normal, State};
		{ok, #video_frame{type = Type} = Frame, Player} -> 
			TimeStamp = Frame#video_frame.timestamp_abs - State#video_player.ts_prev,
			send(Consumer, Frame#video_frame{timestamp=TimeStamp, streamid = StreamId}),
			Timeout = timeout(Frame#video_frame.timestamp_abs, 
				Player#video_player.timer_start, 
				Player#video_player.client_buffer),
			Timer = gen_fsm:start_timer(Timeout, play),
			NextState = Player#video_player{timer_ref  = Timer,
											  ts_prev = Frame#video_frame.timestamp_abs,
											  pos = Frame#video_frame.nextpos},
			{next_state, ready, NextState, ?TIMEOUT};
		{error, _Reason} ->
			?D(_Reason),
			file:close(IoDev),
			{stop, normal, State}
	end;

ready(timeout, State) ->
  Timer = gen_fsm:start_timer(1, play).
  % 'WAIT_FOR_DATA'({timeout, Timer, play}, #ems_fsm{video_timer_ref = Timer, video_device = IoDev, video_pos = Pos, video_stream_id = StreamId, video_format = Format} = State) ->
  %   case Format:read_frame(IoDev, Pos) of
  %     {ok, done} ->
  %       file:close(IoDev),
  %       {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};
  %     {ok, Tag} when is_record(Tag,video_frame) ->
  %       TimeStamp = Tag#video_frame.timestamp_abs - State#ems_fsm.video_ts_prev,      
  %       send(Tag#video_frame{timestamp=TimeStamp, streamid = StreamId}),
  %       Timeout = timeout(Tag#video_frame.timestamp_abs, 
  %                         State#ems_fsm.video_timer_start, 
  %                           State#ems_fsm.client_buffer),
  %       NewTimer = gen_fsm:start_timer(Timeout, play),
  %       NextState = State#ems_fsm{video_timer_ref  = NewTimer,
  %                     video_ts_prev = Tag#video_frame.timestamp_abs,
  %                     video_pos = Tag#video_frame.nextpos},
  %       {next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};
  %     {error,_Reason} -> 
  %       file:close(IoDev),
  %       {stop, normal, State}
  %   end;



%%-------------------------------------------------------------------------
%% @spec (FLV_TAG::tuple()) -> any()
%% @doc Convert FLV_Tag into Channel then transmit the Channel and Body
%% @end
%%-------------------------------------------------------------------------

send(Consumer, #video_frame{type = Type, streamid=StreamId,timestamp_abs = TimeStamp,body=Body, raw_body = false} = Frame) ->
	Channel = #channel{id=channel_id(Type, StreamId),timestamp=TimeStamp,length=size(Body),type=Type,stream=StreamId},
	gen_fsm:send_event(Consumer, {send, {Channel,Frame}});

send(Consumer, #video_frame{type = Type, streamid=StreamId,timestamp_abs = TimeStamp,body=Body}) ->
	Channel = #channel{id=channel_id(Type, StreamId),timestamp=TimeStamp,length=size(Body),type=Type,stream=StreamId},
	gen_fsm:send_event(Consumer, {send, {Channel,Body}}).


% rsaccon: TODO: streams per connections need to be stored and channelId retrieved from stream
% idea: a  process per stream, mnesia RAM table (with streamid as key) contains stream process PID
channel_id(?FLV_TAG_TYPE_META, _StreamId) -> 4;
channel_id(?FLV_TAG_TYPE_VIDEO, _StreamId) -> 5;
channel_id(?FLV_TAG_TYPE_AUDIO, _StreamId) -> 6.



%%-------------------------------------------------------------------------
%% @spec () -> FileName::string()
%% @doc retrieves FLV video file folder from application environment
%% @end
%%-------------------------------------------------------------------------	
file_dir() ->
  case application:get_env(ems, file_dir) of
    {ok, Val} ->
      Val;
    _ ->
      % exit(flv_dir_not_defined)
      "/tmp"
  end.



normalize_filename(Name) ->
    case filename:extension(Name) of
        ".flv" -> Name;
        ".FLV" -> Name;
        ".mp4" -> Name;
        ".MP4" -> Name;
        _      -> Name ++ ".flv"
    end.
 
file_format(Name) ->
  case filename:extension(Name) of
      ".flv" -> ems_flv;
      ".FLV" -> ems_flv;
      ".mp4" -> ems_mp4;
      ".MP4" -> ems_mp4
  end.
  
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.


handle_sync_event(Event, _From, StateName, StateData) ->
     io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, got_sync_request2]),
    {stop, {StateName, undefined_event, Event}, StateData}.
  
handle_info({tcp_closed, _Socket}, _StateName,
            #video_player{} = StateData) ->
    error_logger:info_msg("~p Video player lost connection.\n", [self()]),
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
  {noreply, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
  ok.
 
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%-------------------------------------------------------------------------
%% @spec (AbsTime::integer(), TimerStart::integer(), ClientBuffer::integer()) -> [TimeOut::integer() | 0]
%% @doc calculates timeout to playback of next FLV Tag 
%% @end
%%-------------------------------------------------------------------------	
timeout(AbsTime, TimerStart, ClientBuffer) ->
    Timeout = AbsTime - ClientBuffer - trunc(timer:now_diff(now(), TimerStart) / 1000),
    if 
		(Timeout > 0) -> 
            Timeout; 
        true -> 
            0 
    end.

 
