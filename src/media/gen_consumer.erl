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

-module(gen_consumer).
-author('Max Lapshin <max@maxidoors.ru>').

-include("../../include/ems.hrl").
-include_lib("erlmedia/include/video_frame.hrl").

-export([start_link/3, client/1, init/3, ready/1, tick/1, handle_info/2]).
-export([segment/2]).

-export([behaviour_info/1]).

-record(ems_stream, {
  media_info = undefined,
  module,
  state,
  
	sent_video_config = false,
	sent_audio_config = false,
	sent_metadata = false,
	video_config,
	audio_config,
	metadata,
	paused = false,
	pause_ts = undefined,
	send_audio = true,
	send_video = true,
	
	mode,
	real_mode,
	host,
	name,
	
	playlist_module,
	playlist,
	
	synced = false,
  client_buffer,
  prepush = 0,
  play_end = undefined,
  seek = undefined,
	buffer,
	timer_start,
	playing_from = 0,
	ts_prev = 0,
	pos = undefined,
	last_dts,
	ts_delta,
	stopped = false
}).

behaviour_info(callbacks) -> [{init,2}, {handle_frame,2}, {handle_control,2}, {handle_info,2}, {terminate,1}];
behaviour_info(_Other) -> undefined.


start_link(Module, Options, Args) ->
  {ok, spawn_link(?MODULE, init, [Module, Options, Args])}.
  
client(Player) ->
  Ref = erlang:make_ref(),
  Player ! {client, self(), Ref},
  receive 
    {Info, Ref} -> Info
  after 1000 ->
    {undefined, undefined, Player}
  end.


init(Module, Options, Args) ->
  Host = proplists:get_value(host, Options),
  case Module:init(Options, Args) of
    {ok, State} ->
      ?MODULE:ready(#ems_stream{host = Host, module = Module, state = State});
    Else ->
      Else
  end.

handle_play(Play, #ems_stream{media_info = MediaInfo} = Stream) when MediaInfo =/= undefined ->
  handle_play(Play, stop(Stream));

handle_play({play, Name, Options}, #ems_stream{host = Host, module = M, state = S} = Stream) ->
  ?D({"Playing new file", Name, Options}),
  case media_provider:find_or_open(Host, Name) of
    {notfound, Reason} ->
      {noreply, S1} = M:handle_control({notfound, Reason}, S),
      ?D({"Not found", Name, Options}),
      ?MODULE:ready(Stream#ems_stream{state = S1});
    {playlist, Name, PlaylistOptions} ->
      PlaylistModule = proplists:get_value(url, PlaylistOptions),
      Playlist = PlaylistModule:init(Host, PlaylistOptions),
      ?D({"Opened playlist", PlaylistModule, Playlist}),
      self() ! start,
      ?MODULE:ready(Stream#ems_stream{playlist_module = PlaylistModule, playlist = Playlist});
    MediaEntry when is_pid(MediaEntry) ->
      erlang:monitor(process, MediaEntry),
      {noreply, S1} = M:handle_control({start_play,Name}, S),
      {Seek, PlayingFrom, PlayEnd} = segment(MediaEntry, Options),
      
      ?D({"Seek:", PlayingFrom, PlayEnd, (catch PlayEnd - PlayingFrom)}),

      Stream1 = case (catch PlayEnd - PlayingFrom) of
        SegmentLength when is_number(SegmentLength) -> 
          Stream#ems_stream{mode = file, real_mode = file};
        _ ->
          {ok, MediaMode} = stream_media:subscribe(MediaEntry),
          Stream#ems_stream{mode = MediaMode, real_mode = MediaMode}
      end,  
      self() ! start,
      ?D({"Start", Host, Name, Stream1#ems_stream.mode}),
      ?MODULE:ready(Stream1#ems_stream{media_info = MediaEntry, name = Name,pos = Seek, state = S1,
                                playing_from = PlayingFrom,play_end = PlayEnd,
                                stopped = false, paused = false,
                                client_buffer = proplists:get_value(client_buffer, Options, 10000),
                                sent_audio_config = false, sent_video_config = false,
                                timer_start = element(1, erlang:statistics(wall_clock))})
  end.

stop(#ems_stream{media_info = undefined} = Stream) ->
  ?D({"Already stopped", self()}),
  Stream;


stop(#ems_stream{media_info = MediaEntry} = Stream) ->
  ?D({"Stopping", self(), MediaEntry}),
  stream_media:unsubscribe(MediaEntry),
  flush_tick(),
  flush_frames(),
  Stream#ems_stream{media_info = undefined, stopped = true}.
  
  
flush_frames() ->
  receive
    #video_frame{} -> flush_frames()
  after
    0 -> ok
  end.
  
flush_tick() ->
  receive
    tick -> flush_tick()
  after 
    0 -> ok
  end.



segment(MediaEntry, Options) ->
  {Seek, _BaseTS, PlayingFrom} = case proplists:get_value(seek, Options) of
    undefined -> {undefined, 0, 0};
    {BeforeAfterSeek, SeekTo} ->
      case file_media:seek(MediaEntry, BeforeAfterSeek, SeekTo) of
        {Pos, NewTimestamp} ->
          ?D({"Starting from", Pos,round(SeekTo), NewTimestamp}),
          {Pos, NewTimestamp, NewTimestamp};
        _ ->
          {undefined, 0, 0}
      end
  end,

  PlayEnd = case proplists:get_value(duration, Options) of
    undefined -> undefined;
    {BeforeAfterEnd, Duration} ->
      Length = proplists:get_value(length, media_provider:info(MediaEntry)),
      {_, DurationFrom} = proplists:get_value(seek, Options),
      TotalDuration = DurationFrom + Duration,
      case TotalDuration of
        TotalDuration when TotalDuration > Length -> TotalDuration;
        _ ->
          case file_media:seek(MediaEntry, BeforeAfterEnd, DurationFrom + Duration) of
            {_Pos, EndTimestamp} -> EndTimestamp;
            _ -> undefined
          end
      end
  end,
  {Seek, PlayingFrom, PlayEnd}.


ready(State) ->
  receive
    Message -> ?MODULE:handle_info(Message, State)
  end.
  


  
  
  
handle_eof(#ems_stream{module = M, state = S, playlist_module = undefined} = Stream) ->
  Length = proplists:get_value(length, media_provider:info(Stream#ems_stream.media_info), 0),
  ?D({"File is over", Stream#ems_stream.name, Length}),
  {noreply, S1} = M:handle_control({play_complete, Length}, S),
  ?MODULE:ready(stop(Stream#ems_stream{state = S1}));
  
handle_eof(#ems_stream{playlist_module = PlaylistModule, playlist = Playlist} = Stream) ->
  case PlaylistModule:next(Playlist) of
    eof ->
      PlaylistModule:close(Playlist),
      handle_eof(Stream#ems_stream{playlist_module = undefined, playlist = undefined});
    {Playlist1, Name, Options} ->
      ?D({"Playlist item", Name, Options}),
      handle_play({play,Name,Options}, Stream#ems_stream{playlist = Playlist1})
  end.

  
  
handle_info(Message, #ems_stream{module = M, state = S, mode = Mode, real_mode = RealMode, media_info = MediaEntry, client_buffer = ClientBuffer, playlist_module = PlaylistModule} = State) ->
  case Message of
    {client_buffer, NewClientBuffer} -> 
      ?MODULE:ready(State#ems_stream{client_buffer = NewClientBuffer});
      
    {play, _Name, _Options} = Play ->
      handle_play(Play, State);
    
    start when PlaylistModule =/= undefined andalso Mode == undefined ->
      handle_eof(State);

    {'DOWN', _Ref, process, MediaEntry, _Reason} ->
      ?D("Died media info"),
      handle_eof(State);

    {send_audio, Audio} ->
      ?D({"Send audio", Audio}),
      ?MODULE:ready(State#ems_stream{send_audio = Audio});

    {send_video, Video} ->
      ?D({"Send video", Video}),
      ?MODULE:ready(State#ems_stream{send_video = Video});

    {seek, _BeforeAfter, Timestamp} when RealMode == stream andalso Timestamp == 0 andalso MediaEntry =/= undefined ->
      ?D({"Return to live"}),
      flush_tick(),
      stream_media:subscribe(MediaEntry),
      ?MODULE:ready(State#ems_stream{mode = stream});
      
    {seek, BeforeAfter, Timestamp} when MediaEntry =/= undefined ->
      case file_media:seek(MediaEntry, BeforeAfter, Timestamp) of
        {_, undefined} ->
          ?D({"Seek beyong current borders", Timestamp}),
          {noreply, S1} = M:handle_control({seek_failed, Timestamp}, S),
          ?MODULE:ready(State#ems_stream{state = S1});
        {Pos, NewTimestamp} ->
          ?D({"Player real seek to", round(Timestamp), NewTimestamp, ClientBuffer}),
          stream_media:unsubscribe(MediaEntry),
          self() ! tick,
          flush_frames(),
          {noreply, S1} = M:handle_control({seek_notify, NewTimestamp}, S),
          ?MODULE:ready(State#ems_stream{pos = Pos, mode = file, state = S1,
                                         ts_prev = NewTimestamp, 
                                         playing_from = NewTimestamp,
                                         timer_start = element(1, erlang:statistics(wall_clock)),
                                         prepush = ClientBuffer});
        undefined ->
          ?D({"Seek beyong current borders", Timestamp}),
          {noreply, S1} = M:handle_control({seek_failed, Timestamp}, S),
          ?MODULE:ready(State#ems_stream{state = S1})
      end;

    pause ->
      ?D("Player paused"),
      flush_tick(),
      ?MODULE:ready(State#ems_stream{paused = true});

    {pause, NewTS} ->
      ?D("Player paused"),
      flush_tick(),
      ?MODULE:ready(State#ems_stream{paused = true, pause_ts = NewTS});

    exit ->
      M:terminate(S),
      ok;

    undefined ->
      ?MODULE:ready(State);

    Message ->
      case Mode of
        file -> handle_file(Message, State);
        stream -> handle_stream(Message, State);
        undefined ->
          ?D({"Unknown message", Message}),
          ?MODULE:ready(State)
      end

  end.


handle_stream(Message, #ems_stream{module = M, state = S} = State) ->
  case Message of
    start ->
      erlang:yield(),
      ?MODULE:ready(State);

    {resume, _NewTS} ->
      ?D("Player resumed"),
      ?MODULE:ready(State#ems_stream{mode=stream,paused = false});

    resume ->
      ?D("Player resumed"),
      ?MODULE:ready(State#ems_stream{mode=stream,paused = false});


    #video_frame{} = Frame ->
      handle_frame(State, Frame);

    eof ->
      ?D("MPEG TS finished"),
      handle_eof(State);

    stop -> 
      ?D({"stream play stop", self()}),
      {noreply, S1} = M:handle_control(stop, S),
      ?MODULE:ready(stop(State#ems_stream{state = S1}));

  	Message ->
  	  case M:handle_info(Message, S) of
  	    {noreply, S1} -> ?MODULE:ready(State#ems_stream{state = S1});
  	    stop -> ok;
  	    Else -> ?D(Else),erlang:error(Else)
  	  end
  end.


  
handle_file(Message, #ems_stream{media_info = MediaInfo, module = M, state = S, client_buffer = ClientBuffer, pause_ts = PauseTS} = State) ->
  case Message of
    start ->
      Meta = case file_media:metadata(MediaInfo) of
        undefined -> undefiend;
        MetaData -> #video_frame{type = metadata, body = [<<?AMF_COMMAND_ONMETADATA>>, MetaData], dts = 0, pts = 0}
      end,
    	self() ! tick,
      ?MODULE:ready(State#ems_stream{prepush = ClientBuffer, stopped = false, paused = false, metadata = Meta, sent_metadata = false});
      
    resume ->
      ?D("Player resumed"),
      self() ! tick,
      ?MODULE:ready(State#ems_stream{paused = false});


    {resume, NewTS} ->
      ?D({"Player resumed at", PauseTS, NewTS}),
      case PauseTS of
        NewTS -> self() ! tick;
        _ -> self() ! {seek, before, NewTS}
      end,
      ?MODULE:ready(State#ems_stream{paused = false});
      
    stop ->
      {noreply, S1} = M:handle_control(stop, S),
      ?MODULE:ready(stop(State#ems_stream{state = S1}));
  
    tick ->
      tick(State);
    	
  	Message ->
  	  case M:handle_info(Message, S) of
  	    {noreply, S1} -> ?MODULE:ready(State#ems_stream{state = S1});
  	    stop -> ok;
  	    Else -> ?D(Else),erlang:error(Else)
  	  end
  end.


  

tick(#ems_stream{stopped = true} = State) ->
  ?MODULE:ready(State);

tick(#ems_stream{paused = true} = State) ->
  ?MODULE:ready(State);


tick(#ems_stream{media_info = MediaInfo, pos = Key} = Player) ->
  % ?D({tick,self(),MediaInfo,Key,Player#ems_stream.play_end}),
  handle_frame(Player, file_media:read_frame(MediaInfo, Key)).


%% We must do the following algorithm, the same as in stream_media.
%% Wait for first non-decoder config frame and set it as a start time of new portion of stream
%% The whole ems_stream should start from first timestamp of first portion
%% After first content frame arrives, we calculate delta between last dts from previous portion of 
%% frames and new. This protects us from situation, when stream_media sends us frames with huge timestamps
handle_frame(#ems_stream{ts_delta = undefined} = Stream, #video_frame{decoder_config = true} = Frame) ->
  send_frame(Stream, Frame);

handle_frame(#ems_stream{last_dts = undefined} = State, #video_frame{dts = DTS} = Frame) ->
  handle_frame(State#ems_stream{last_dts = DTS}, Frame);

handle_frame(#ems_stream{ts_delta = undefined, last_dts = LastDTS} = Stream, #video_frame{decoder_config = false, dts = DTS} = Frame) ->
  ?D({"New instance of gen_consumer", LastDTS - DTS, Frame#video_frame.type}),
  handle_frame(Stream#ems_stream{ts_delta = LastDTS - DTS}, Frame); %% Lets glue new instance of stream to old one

handle_frame(#ems_stream{ts_delta = Delta} = Stream, #video_frame{dts = DTS, pts = PTS} = Frame) ->
  send_frame(Stream, Frame#video_frame{dts = DTS + Delta, pts = PTS + Delta});

handle_frame(#ems_stream{} = Stream, eof) ->
  send_frame(Stream, eof).



%% Here goes plenty, plenty of cases
  
send_frame(#ems_stream{play_end = PlayEnd} = State, #video_frame{dts = Timestamp}) when is_number(PlayEnd) andalso PlayEnd =< Timestamp ->
  % Consumer ! {ems_stream, StreamId, play_complete, Length},
  ?D({"Finished due to playend"}),
  handle_eof(State);


send_frame(#ems_stream{module = M, state = S, metadata = Meta, sent_metadata = false} = Player, 
           #video_frame{type = video, frame_type = keyframe, dts = DTS} = Frame) when Meta =/= undefined ->
  % ?D({"Sent metadata", Meta}),
  {noreply, S1} = M:handle_frame(Meta#video_frame{dts = DTS, pts = DTS}, S),
  send_frame(Player#ems_stream{sent_metadata = true, state = S1}, Frame);


send_frame(#ems_stream{mode = file, module = M, state = S} = Player, 
           #video_frame{next_id = Next} = Frame) ->
  {noreply, S1} = M:handle_frame(Frame, S),
  tick_timeout(Frame, Player#ems_stream{pos = Next, state = S1});


send_frame(#ems_stream{} = Player, #video_frame{decoder_config = true, type = video} = F) ->
  ?MODULE:ready(Player#ems_stream{video_config = F});

send_frame(#ems_stream{} = Player, #video_frame{decoder_config = true, type = audio} = F) ->
  ?MODULE:ready(Player#ems_stream{audio_config = F});

send_frame(#ems_stream{} = Player, #video_frame{type = metadata} = F) ->
  % ?D({"Replacing metadata", F}),
  ?MODULE:ready(Player#ems_stream{metadata = F, sent_metadata = false});



send_frame(#ems_stream{} = Player, eof) ->
  handle_eof(Player);

send_frame(#ems_stream{module = M, state = S, audio_config = A, sent_audio_config = false} = Player, 
           #video_frame{type = audio, dts = DTS} = Frame) when A =/= undefined ->
  {noreply, S1} = M:handle_frame(A#video_frame{dts = DTS, pts = DTS}, S),
  ?D({"Send audio config", DTS}),
  {noreply, S2} = M:handle_frame(Frame, S1),
  ?MODULE:ready(Player#ems_stream{sent_audio_config = true, state = S2});

send_frame(#ems_stream{module = M, state = S, video_config = V, sent_video_config = false} = Player, 
           #video_frame{type = video, frame_type = keyframe, dts = DTS} = Frame) when V =/= undefined ->
   {noreply, S1} = M:handle_frame(V#video_frame{dts = DTS, pts = DTS}, S),
   ?D({"Send audio config", DTS}),
   {noreply, S2} = M:handle_frame(Frame, S1),
   ?MODULE:ready(Player#ems_stream{sent_video_config = true, state = S2});

send_frame(#ems_stream{module = M, state = S, sent_audio_config = true} = Player, 
           #video_frame{type = audio, codec_id = aac} = Frame) ->
  {noreply, S1} = M:handle_frame(Frame, S),
  ?MODULE:ready(Player#ems_stream{state = S1});

send_frame(#ems_stream{module = M, state = S, sent_video_config = true} = Player, 
           #video_frame{type = video, codec_id = h264} = Frame) ->
  {noreply, S1} = M:handle_frame(Frame, S),
  ?MODULE:ready(Player#ems_stream{state = S1});

send_frame(#ems_stream{module = M, state = S} = Player, 
           #video_frame{codec_id = Codec} = Frame) when Codec =/= aac andalso Codec =/= h264 ->
  % ?D({Frame#video_frame.type, Frame#video_frame.dts, Frame#video_frame.codec_id}),
  {noreply, S1} = M:handle_frame(Frame, S),
  ?MODULE:ready(Player#ems_stream{state = S1});

send_frame(#ems_stream{} = Player, #video_frame{type = _Type, dts = _DTS} = _Frame) ->
  % ?D({"Refuse to sent unsynced frame", _Type, _DTS, _Frame#video_frame.frame_type}),
  ?MODULE:ready(Player).



  

%%-------------------------------------------------------------------------
%% @spec (AbsTime::integer(), TimerStart::integer(), ClientBuffer::integer()) -> [TimeOut::integer() | 0]
%% @doc calculates timeout to playback of next FLV Tag 
%% @end
%%-------------------------------------------------------------------------	

tick_timeout(#video_frame{dts = AbsTime}, #ems_stream{timer_start = TimerStart, client_buffer = ClientBuffer, playing_from = PlayingFrom, prepush = Prepush} = Player) ->
  SeekTime = AbsTime - PlayingFrom,
  % Timeout = SeekTime - ClientBuffer - trunc(timer:now_diff(now(), TimerStart) / 1000),
  
  Timeout = SeekTime - ClientBuffer - (element(1, erlang:statistics(wall_clock)) - TimerStart),

  % ?D({"Timeout", Timeout, AbsTime, PlayingFrom, ClientBuffer, Prepush - SeekTime, 
  %  (element(1, erlang:statistics(wall_clock)) - TimerStart)}),
  wait4tick(Player, Prepush - SeekTime, round(Timeout)).
  
wait4tick(Player, Prepush, _Timeout) when Prepush > 0 ->
  ?MODULE:tick(Player#ems_stream{prepush = Prepush});
  
wait4tick(Player, _Prepush, Timeout) when Timeout > 0 ->
  receive
    tick ->
      handle_info(tick, Player);
    Message ->
      self() ! tick,
      handle_info(Message, Player)
  after
    Timeout ->
      handle_info(tick, Player)
  end;

wait4tick(Player, _, _) ->
  ?MODULE:tick(Player).

 
