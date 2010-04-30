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

-module(ems_stream).
-author('Max Lapshin <max@maxidoors.ru>').

-include("../../include/ems.hrl").
-include_lib("erlmedia/include/video_frame.hrl").

-export([file_dir/1, file_format/1, start_link/1, client/1, init/1, ready/1, tick/1]).
-export([segment/2]).



-record(ems_stream, {
  consumer,
  media_info = undefined,
	stream_id,
	sent_video_config = false,
	sent_audio_config = false,
	video_config,
	audio_config,
	paused = false,
	pause_ts = undefined,
	send_audio = true,
	send_video = true,
	
	mode,
	real_mode,
	host,
	name,
	bytes_sent = 0,
	
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
	stopped = false
}).



start_link(Options) ->
  {ok, spawn_link(?MODULE, init, [Options])}.
  
client(Player) ->
  Ref = erlang:make_ref(),
  Player ! {client, self(), Ref},
  receive 
    {Info, Ref} -> Info
  after 1000 ->
    {undefined, undefined, Player}
  end.


init(Options) ->
  Consumer = proplists:get_value(consumer, Options),
  erlang:monitor(process, Consumer),
  link(Consumer),
  Stream = #ems_stream{consumer = Consumer,
                       host = proplists:get_value(host, Options),
                       stream_id = proplists:get_value(stream_id, Options)},
  ?MODULE:ready(Stream).

handle_play(Play, #ems_stream{media_info = MediaInfo} = Stream) when MediaInfo =/= undefined ->
  handle_play(Play, stop(Stream));

handle_play({play, Name, Options}, #ems_stream{host = Host, consumer = Consumer, stream_id = StreamId} = Stream) ->
  ?D({"Playing new file", Name, Options}),
  case media_provider:find_or_open(Host, Name) of
    {notfound, Reason} ->
      Consumer ! {ems_stream, StreamId, {notfound, Reason}},
      ?D({"Not found", Name, Options}),
      ?MODULE:ready(Stream);
    {playlist, Name, PlaylistOptions} ->
      PlaylistModule = proplists:get_value(url, PlaylistOptions),
      Playlist = PlaylistModule:init(Host, PlaylistOptions),
      ?D({"Opened playlist", PlaylistModule, Playlist}),
      self() ! start,
      ?MODULE:ready(Stream#ems_stream{playlist_module = PlaylistModule, playlist = Playlist});
    MediaEntry when is_pid(MediaEntry) ->
      erlang:monitor(process, MediaEntry),
      Consumer ! {ems_stream, StreamId, start_play},
      {Seek, PlayingFrom, PlayEnd} = segment(MediaEntry, Options),
      
      ?D({"Seek:", PlayingFrom, PlayEnd, (catch PlayEnd - PlayingFrom)}),

      Stream1 = case (catch PlayEnd - PlayingFrom) of
        SegmentLength when is_number(SegmentLength) -> 
          Stream#ems_stream{mode = file, real_mode = file};
        _ ->
          {ok, MediaMode} = gen_server:call(MediaEntry, {subscribe, self()}),
          Stream#ems_stream{mode = MediaMode,pos = Seek, real_mode = MediaMode,
                             playing_from = PlayingFrom,
                             play_end = PlayEnd}
      end,  
      self() ! start,
      ?D({"Start", Host, Name, Stream1#ems_stream.mode}),
      ems_event:user_play(Host, Consumer, Name, MediaEntry),
      ?MODULE:ready(Stream1#ems_stream{media_info = MediaEntry, name = Name,
                                stopped = false, paused = false,
                                client_buffer = proplists:get_value(client_buffer, Options, 10000),
                                sent_audio_config = false, sent_video_config = false, bytes_sent = 0,
                                timer_start = element(1, erlang:statistics(wall_clock))})
  end.


stop(#ems_stream{media_info = MediaEntry} = Stream) ->
  ?D({"Stopping", MediaEntry}),
  notify_stats(Stream),
  gen_server:call(MediaEntry, {unsubscribe, self()}),
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
    Message -> handle_info(Message, State)
  end.
  


  
  
  
handle_eof(#ems_stream{consumer = Consumer, stream_id = StreamId, playlist_module = undefined} = Stream) ->
  Length = proplists:get_value(length, media_provider:info(Stream#ems_stream.media_info), 0),
  ?D({"File is over", Stream#ems_stream.name, Length}),
  Consumer ! {ems_stream, StreamId, play_complete, Length},
  ?MODULE:ready(stop(Stream));
  
handle_eof(#ems_stream{playlist_module = PlaylistModule, playlist = Playlist} = Stream) ->
  case PlaylistModule:next(Playlist) of
    eof ->
      PlaylistModule:close(Playlist),
      handle_eof(Stream#ems_stream{playlist_module = undefined, playlist = undefined});
    {Playlist1, Name, Options} ->
      ?D({"Playlist item", Name, Options}),
      handle_play({play,Name,Options}, Stream#ems_stream{playlist = Playlist1})
  end.

  
  
handle_info(Message, #ems_stream{mode = Mode, real_mode = RealMode, stream_id = StreamId, media_info = MediaEntry, consumer = Consumer, client_buffer = ClientBuffer, playlist_module = PlaylistModule} = State) ->
  case Message of
    {client_buffer, NewClientBuffer} -> 
      ?MODULE:ready(State#ems_stream{client_buffer = NewClientBuffer});
      
    {play, _Name, _Options} = Play ->
      handle_play(Play, State);
    
    start when PlaylistModule =/= undefined andalso Mode == undefined ->
      handle_eof(State);

    {'DOWN', _Ref, process, Consumer, _Reason} ->
      notify_stats(State),
      ok;

    {'DOWN', _Ref, process, MediaEntry, _Reason} ->
      ?D("Died media info"),
      handle_eof(State);

    {send_audio, Audio} ->
      ?D({"Send audio", Audio}),
      ?MODULE:ready(State#ems_stream{send_audio = Audio});

    {send_video, Video} ->
      ?D({"Send video", Video}),
      ?MODULE:ready(State#ems_stream{send_video = Video});

    {client, Pid, Ref} ->
      Pid ! {gen_fsm:sync_send_event(Consumer, info), Ref},
      ?MODULE:ready(State);

    {seek, _BeforeAfter, Timestamp} when RealMode == stream andalso Timestamp == 0 andalso MediaEntry =/= undefined ->
      ?D({"Return to live"}),
      flush_tick(),
      gen_server:call(MediaEntry, {subscribe, self()}),
      ?MODULE:ready(State#ems_stream{mode = stream});
      
    {seek, BeforeAfter, Timestamp} when MediaEntry =/= undefined ->
      case file_media:seek(MediaEntry, BeforeAfter, Timestamp) of
        {_, undefined} ->
          ?D({"Seek beyong current borders", Timestamp}),
          Consumer ! {ems_stream, StreamId, seek_failed},
          ?MODULE:ready(State);
        {Pos, NewTimestamp} ->
          ?D({"Player real seek to", round(Timestamp), NewTimestamp, ClientBuffer}),
          gen_server:call(MediaEntry, {unsubscribe, self()}),
          self() ! tick,
          flush_frames(),
          Consumer ! {ems_stream, StreamId, seek_notify, NewTimestamp},
          ?MODULE:ready(State#ems_stream{pos = Pos, mode = file,
                                         ts_prev = NewTimestamp, 
                                         playing_from = NewTimestamp,
                                         timer_start = element(1, erlang:statistics(wall_clock)),
                                         prepush = ClientBuffer});
        undefined ->
          ?D({"Seek beyong current borders", Timestamp}),
          Consumer ! {ems_stream, StreamId, seek_failed},
          ?MODULE:ready(State)
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
      notify_stats(State),
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


handle_stream(Message, #ems_stream{} = State) ->
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
      send_frame(State, Frame);

    eof ->
      ?D("MPEG TS finished"),
      handle_eof(State);

    stop -> 
      ?D({"stream play stop", self()}),
      ?MODULE:ready(stop(State));

  	Else ->
  	  ?D({"Unknown message", self(), Else}),
  	  ?MODULE:ready(State)
  end.


  
handle_file(Message, #ems_stream{media_info = MediaInfo, consumer = Consumer, stream_id = StreamId, client_buffer = ClientBuffer, pause_ts = PauseTS} = State) ->
  case Message of
    start ->
      case file_media:metadata(MediaInfo) of
        undefined -> ok;
        MetaData -> Consumer ! #video_frame{type = metadata, stream_id = StreamId, body = [<<?AMF_COMMAND_ONMETADATA>>, MetaData]}
      end,
    	self() ! tick,
      ?MODULE:ready(State#ems_stream{prepush = ClientBuffer, stopped = false, paused = false});
      
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
      ?MODULE:ready(stop(State));
  
    tick ->
      tick(State);
    	
  	Else ->
  	  ?D({"Unknown message", Else}),
  	  ?MODULE:ready(State)
  end.


notify_stats(#ems_stream{host = Host, consumer = User, name = Name, bytes_sent = Sent}) ->
  ems_event:user_stop(Host, User, Name, Sent).
  

tick(#ems_stream{stopped = true} = State) ->
  ?MODULE:ready(State);

tick(#ems_stream{paused = true} = State) ->
  ?MODULE:ready(State);


tick(#ems_stream{media_info = MediaInfo, pos = Key} = Player) ->
  send_frame(Player, file_media:read_frame(MediaInfo, Key)).


%% Here goes plenty, plenty of cases
  
send_frame(#ems_stream{play_end = PlayEnd, stream_id = _StreamId} = State, #video_frame{dts = Timestamp}) when is_number(PlayEnd) andalso PlayEnd =< Timestamp ->
  % Consumer ! {ems_stream, StreamId, play_complete, Length},
  ?D({"Finished due to playend"}),
  handle_eof(State);


send_frame(#ems_stream{mode = file, consumer = Consumer, stream_id = StreamId, bytes_sent = Sent} = Player, 
           #video_frame{next_id = Next} = Frame) ->
  Consumer ! Frame#video_frame{stream_id = StreamId},
  tick_timeout(Frame, Player#ems_stream{bytes_sent = Sent + bin_size(Frame), pos = Next});


send_frame(#ems_stream{} = Player, #video_frame{decoder_config = true, type = video} = F) ->
  ?MODULE:ready(Player#ems_stream{video_config = F});

send_frame(#ems_stream{} = Player, #video_frame{decoder_config = true, type = audio} = F) ->
  ?MODULE:ready(Player#ems_stream{audio_config = F});


send_frame(#ems_stream{consumer = Consumer, stream_id = StreamId} = Player, #video_frame{type = metadata} = F) ->
  Consumer ! F#video_frame{stream_id = StreamId},
  ?MODULE:ready(Player);



send_frame(#ems_stream{} = Player, eof) ->
  handle_eof(Player);

send_frame(#ems_stream{consumer = Consumer, stream_id = StreamId, audio_config = A, sent_audio_config = false, bytes_sent = Sent} = Player, 
           #video_frame{type = audio, dts = DTS} = Frame) when A =/= undefined ->
  Consumer ! A#video_frame{stream_id = StreamId, dts = DTS},
  ?D({"Send audio config", DTS}),
  Consumer ! Frame#video_frame{stream_id = StreamId},
  ?MODULE:ready(Player#ems_stream{sent_audio_config = true, bytes_sent = Sent + bin_size(Frame)});

send_frame(#ems_stream{consumer = Consumer, stream_id = StreamId, video_config = V, sent_video_config = false, bytes_sent = Sent} = Player, 
           #video_frame{type = video, frame_type = keyframe, dts = DTS} = Frame) when V =/= undefined ->
  Consumer ! V#video_frame{stream_id = StreamId, dts = DTS},
  ?D({"Send video config", self(), DTS}),
  Consumer ! Frame#video_frame{stream_id = StreamId},
  ?MODULE:ready(Player#ems_stream{sent_video_config = true, bytes_sent = Sent + bin_size(Frame)});

send_frame(#ems_stream{consumer = Consumer, stream_id = StreamId, sent_audio_config = true, bytes_sent = Sent} = Player, 
           #video_frame{type = audio, codec_id = aac} = Frame) ->
  Consumer ! Frame#video_frame{stream_id = StreamId},
  ?MODULE:ready(Player#ems_stream{bytes_sent = Sent + bin_size(Frame)});

send_frame(#ems_stream{consumer = Consumer, stream_id = StreamId, sent_video_config = true, bytes_sent = Sent} = Player, 
           #video_frame{type = video, codec_id = h264} = Frame) ->
  Consumer ! Frame#video_frame{stream_id = StreamId},
  ?MODULE:ready(Player#ems_stream{bytes_sent = Sent + bin_size(Frame)});

send_frame(#ems_stream{consumer = Consumer, stream_id = StreamId, bytes_sent = Sent} = Player, 
           #video_frame{codec_id = Codec} = Frame) when Codec =/= aac andalso Codec =/= h264 ->
  % ?D({Frame#video_frame.type, Frame#video_frame.dts, Frame#video_frame.codec_id}),
  Consumer ! Frame#video_frame{stream_id = StreamId},
  ?MODULE:ready(Player#ems_stream{bytes_sent = Sent + bin_size(Frame)});

send_frame(#ems_stream{} = Player, #video_frame{type = _Type, dts = _DTS} = _Frame) ->
  % ?D({"Refuse to sent unsynced frame", _Type, _DTS, _Frame#video_frame.frame_type}),
  ?MODULE:ready(Player).


bin_size(#video_frame{body = Body} = _Frame) ->
  try iolist_size(Body) of
    BinSize -> BinSize
  catch
    _:_ -> 0
  end.



%%-------------------------------------------------------------------------
%% @spec () -> FileName::string()
%% @doc retrieves FLV video file folder from application environment
%% @end
%%-------------------------------------------------------------------------	
file_dir(Host) ->
  ems:get_var(file_dir, Host, undefined).



file_format(Name) ->
  case filename:extension(Name) of
      ".flv" -> flv_reader;
      ".FLV" -> flv_reader;
      ".3gp" -> mp4_reader;
      ".mp4" -> mp4_reader;
      ".MP4" -> mp4_reader;
      ".mov" -> mp4_reader;
      ".m4v" -> mp4_reader;
      ".mkv" -> mkv;
      ".MKV" -> mkv;
      _ -> flv_reader
  end.
  

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

 
