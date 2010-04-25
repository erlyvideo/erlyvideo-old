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

-export([file_dir/1, file_format/1, start_link/1, client/1, init/1, ready/1, play/1]).



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
	
	synced = false,
  client_buffer,
  prepush = 0,
  play_end = undefined,
  seek = undefined,
	buffer,
	timer_start,
	playing_from = 0,
	ts_prev = 0,
	pos = 0,
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
    {'DOWN', _, process, Consumer, _} ->
      ok;
    MediaEntry ->
      erlang:monitor(process, MediaEntry),
      Consumer ! {ems_stream, StreamId, start_play},
      {ok, Mode} = gen_server:call(MediaEntry, {subscribe, self()}),
      self() ! start,
      ems_event:user_play(Host, Consumer, Name, MediaEntry),
      prepare(Stream#ems_stream{media_info = MediaEntry, mode = Mode, real_mode = Mode, name = Name,
                                sent_audio_config = false, sent_video_config = false, bytes_sent = 0,
                                timer_start = element(1, erlang:statistics(wall_clock))}, Options)
  end.


stop(#ems_stream{media_info = MediaEntry} = Stream) ->
  ?D({"Stopping", MediaEntry}),
  gen_server:call(MediaEntry, {unsubscribe, self()}),
  flush_play(),
  Stream#ems_stream{media_info = undefined}.
  
  
flush_frames() ->
  receive
    #video_frame{} -> flush_frames()
  after
    0 -> ok
  end.
  
flush_play() ->
  receive
    play -> flush_play()
  after 
    0 -> ok
  end.
        

% prepare(#ems_stream{mode = stream} = Stream, _Options) ->
%   ready(Stream);
% 
prepare(#ems_stream{media_info = MediaEntry, mode = CurrentMode} = Stream, Options) ->
  {Seek, _BaseTS, PlayingFrom} = case proplists:get_value(seek, Options) of
    undefined -> {undefined, 0, 0};
    {BeforeAfterSeek, SeekTo} ->
      case file_media:seek(MediaEntry, BeforeAfterSeek, SeekTo) of
        {Pos, NewTimestamp} ->
          ?D({"Starting from", round(SeekTo), NewTimestamp}),
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
  ?D({"Seek:", PlayingFrom, PlayEnd, (catch PlayEnd - PlayingFrom)}),
  Mode = case Seek of
    undefined -> CurrentMode;
    _ -> 
      gen_server:call(MediaEntry, {unsubscribe, self()}),
      flush_play(),
      file
  end,  
  ready(Stream#ems_stream{pos = Seek, mode = Mode,
                     playing_from = PlayingFrom,
                     play_end = PlayEnd,
                     client_buffer = proplists:get_value(client_buffer, Options, 10000)}).
  


ready(State) ->
  receive
    Message -> handle_info(Message, State)
  end.
  
handle_info(Message, #ems_stream{mode = Mode, real_mode = RealMode, stream_id = StreamId, media_info = MediaEntry, consumer = Consumer, client_buffer = ClientBuffer} = State) ->
  case Message of
    {client_buffer, NewClientBuffer} -> 
      ?MODULE:ready(State#ems_stream{client_buffer = NewClientBuffer});
      
    {play, _Name, _Options} = Play ->
      handle_play(Play, State);

    {'DOWN', _Ref, process, Consumer, _Reason} ->
      notify_stats(State),
      ok;

    {'DOWN', _Ref, process, MediaEntry, _Reason} ->
      ?D("Died media info"),
      Consumer ! {ems_stream, StreamId, play_complete, 0},
      notify_stats(State),
      ?MODULE:ready(State#ems_stream{media_info = undefined});


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
      flush_play(),
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
          self() ! play,
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
      flush_play(),
      ?MODULE:ready(State#ems_stream{paused = true});

    {pause, NewTS} ->
      ?D("Player paused"),
      flush_play(),
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


handle_stream(Message, #ems_stream{media_info = MediaEntry} = State) ->
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
      ok;

    stop -> 
      ?D({"stream play stop", self()}),
      gen_server:call(MediaEntry, {unsubscribe, self()}),
      notify_stats(State),
      ?MODULE:ready(State#ems_stream{media_info = undefined});


  	{tcp_closed, _Socket} ->
      error_logger:info_msg("~p Video player lost connection.\n", [self()]),
      ok;

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
    	self() ! play,
      ?MODULE:ready(State#ems_stream{prepush = ClientBuffer, stopped = false, paused = false});
      
      
    resume ->
      ?D("Player resumed"),
      self() ! play,
      ?MODULE:ready(State#ems_stream{paused = false});


    {resume, NewTS} ->
      ?D({"Player resumed at", PauseTS, NewTS}),
      case PauseTS of
        NewTS -> self() ! play;
        _ -> self() ! {seek, before, NewTS}
      end,
      ?MODULE:ready(State#ems_stream{paused = false});
      
    stop ->
      flush_play(),
      notify_stats(State),
      ?MODULE:ready(State);
  
    play ->
      play(State);
    	
  	{tcp_closed, _Socket} ->
      error_logger:info_msg("~p Video player lost connection.\n", [self()]),
      ok;
  	Else ->
  	  ?D({"Unknown message", Else}),
  	  ?MODULE:ready(State)
  end.


notify_stats(#ems_stream{host = Host, consumer = User, name = Name, bytes_sent = Sent}) ->
  ems_event:user_stop(Host, User, Name, Sent).
  

play(#ems_stream{stopped = true} = State) ->
  ?MODULE:ready(State);

play(#ems_stream{paused = true} = State) ->
  ?MODULE:ready(State);


play(#ems_stream{sent_audio_config = false, media_info = MediaInfo, pos = Pos} = Player) ->
  % ?D({"Sent audio config"}),
  send_frame(Player#ems_stream{sent_audio_config = true}, {file_media:codec_config(MediaInfo, audio), Pos});

play(#ems_stream{sent_video_config = false, media_info = MediaInfo, pos = Pos} = Player) ->
  % ?D({"Sent video config", file_media:codec_config(MediaInfo, video)}),
  send_frame(Player#ems_stream{sent_video_config = true}, {file_media:codec_config(MediaInfo, video), Pos});
    

play(#ems_stream{media_info = MediaInfo, pos = Key} = Player) ->
  Reply = file_media:read_frame(MediaInfo, Key),
  send_frame(Player, Reply).
  
send_frame(#ems_stream{play_end = PlayEnd, stream_id = _StreamId} = State, {#video_frame{dts = Timestamp}, _}) when is_number(PlayEnd) andalso PlayEnd =< Timestamp ->
  % Consumer ! {ems_stream, StreamId, play_complete, Length},
  ?D({"Finished due to playend"}),
  notify_stats(State),
  ok;

send_frame(#ems_stream{mode=stream} = Player, #video_frame{decoder_config = true, type = video} = F) ->
  ?MODULE:ready(Player#ems_stream{video_config = F});

send_frame(#ems_stream{mode=stream} = Player, #video_frame{decoder_config = true, type = audio} = F) ->
  ?MODULE:ready(Player#ems_stream{audio_config = F});


send_frame(#ems_stream{mode = stream, consumer = Consumer, stream_id = StreamId, bytes_sent = Sent} = Player, #video_frame{type = metadata} = F) ->
  Consumer ! F#video_frame{stream_id = StreamId},
  ?MODULE:ready(Player#ems_stream{bytes_sent = Sent + iolist_size(F#video_frame.body)});

send_frame(#ems_stream{mode=stream, consumer = Consumer, stream_id = StreamId, audio_config = A, sent_audio_config = false, bytes_sent = Sent} = Player, 
           #video_frame{type = audio, dts = DTS} = Frame) when A =/= undefined ->
  Consumer ! A#video_frame{stream_id = StreamId, dts = DTS},
  ?D({"Send audio config", DTS}),
  Consumer ! Frame#video_frame{stream_id = StreamId},
  ?MODULE:ready(Player#ems_stream{sent_audio_config = true, bytes_sent = Sent + iolist_size(Frame#video_frame.body)});

send_frame(#ems_stream{mode=stream, consumer = Consumer, stream_id = StreamId, video_config = V, sent_video_config = false, bytes_sent = Sent} = Player, 
           #video_frame{type = video, frame_type = keyframe, dts = DTS} = Frame) when V =/= undefined ->
  Consumer ! V#video_frame{stream_id = StreamId, dts = DTS},
  ?D({"Send video config", DTS}),
  Consumer ! Frame#video_frame{stream_id = StreamId},
  ?MODULE:ready(Player#ems_stream{sent_video_config = true, bytes_sent = Sent + iolist_size(Frame#video_frame.body)});

send_frame(#ems_stream{mode=stream,consumer = Consumer, stream_id = StreamId, sent_audio_config = true, bytes_sent = Sent} = Player, 
           #video_frame{type = audio, codec_id = aac} = Frame) ->
  Consumer ! Frame#video_frame{stream_id = StreamId},
  ?MODULE:ready(Player#ems_stream{bytes_sent = Sent + iolist_size(Frame#video_frame.body)});

send_frame(#ems_stream{mode=stream,consumer = Consumer, stream_id = StreamId, sent_video_config = true, bytes_sent = Sent} = Player, 
           #video_frame{type = video, codec_id = h264} = Frame) ->
  Consumer ! Frame#video_frame{stream_id = StreamId},
  ?MODULE:ready(Player#ems_stream{bytes_sent = Sent + iolist_size(Frame#video_frame.body)});

send_frame(#ems_stream{mode=stream,consumer = Consumer, stream_id = StreamId, bytes_sent = Sent} = Player, 
           #video_frame{codec_id = Codec} = Frame) when Codec =/= aac andalso Codec =/= h264 ->
  % ?D({Frame#video_frame.type, Frame#video_frame.dts, Frame#video_frame.codec_id}),
  Consumer ! Frame#video_frame{stream_id = StreamId},
  ?MODULE:ready(Player#ems_stream{bytes_sent = Sent + iolist_size(Frame#video_frame.body)});

send_frame(#ems_stream{mode = stream} = Player, #video_frame{type = _Type, dts = _DTS} = _Frame) ->
  % ?D({"Refuse to sent unsynced frame", _Type, _DTS, _Frame#video_frame.frame_type}),
  ?MODULE:ready(Player);

send_frame(#ems_stream{mode=file} = Player, undefined) ->
  self() ! play,
  ?MODULE:ready(Player);

send_frame(#ems_stream{mode=file} = Player, {undefined, undefined}) ->
  self() ! play,
  ?MODULE:ready(Player);

send_frame(#ems_stream{mode=file} = Player, {#video_frame{body = undefined}, Next}) ->
  self() ! play,
  ?MODULE:ready(Player#ems_stream{pos = Next});
  
send_frame(#ems_stream{mode=file, name = Name, consumer = Consumer, stream_id = StreamId} = Player, done) ->
  Length = proplists:get_value(length, media_provider:info(Player#ems_stream.media_info)),
  ?D({"File is over", Name, Length}),
  % timer:send_after(round(Timeout), Consumer, {ems_stream, StreamId, play_complete, Length}),
  Consumer ! {ems_stream, StreamId, play_complete, Length},
  flush_play(),
  case Player#ems_stream.play_end of
    undefined -> ?MODULE:ready(Player#ems_stream{});
    _ -> ok
  end;

send_frame(#ems_stream{mode=file,consumer = Consumer, stream_id = StreamId, bytes_sent = Sent} = Player, {#video_frame{} = Frame, Next}) ->
  % ?D({Frame#video_frame.type, Frame1#video_frame.dts}),
  Consumer ! Frame#video_frame{stream_id = StreamId},    
  timeout_play(Frame, Player#ems_stream{pos = Next, bytes_sent = Sent + iolist_size(Frame#video_frame.body)}).
  



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

timeout_play(#video_frame{dts = AbsTime}, #ems_stream{timer_start = TimerStart, client_buffer = ClientBuffer, playing_from = PlayingFrom, prepush = Prepush} = Player) ->
  SeekTime = AbsTime - PlayingFrom,
  % Timeout = SeekTime - ClientBuffer - trunc(timer:now_diff(now(), TimerStart) / 1000),
  
  Timeout = SeekTime - ClientBuffer - (element(1, erlang:statistics(wall_clock)) - TimerStart),

  % ?D({"Timeout", Timeout, AbsTime, PlayingFrom, ClientBuffer, Prepush - SeekTime, 
  %  (element(1, erlang:statistics(wall_clock)) - TimerStart)}),
  make_play(Player, Prepush - SeekTime, round(Timeout)).
  
make_play(Player, Prepush, _Timeout) when Prepush > 0 ->
  ?MODULE:play(Player#ems_stream{prepush = Prepush});
  
make_play(Player, _Prepush, Timeout) when Timeout > 0 ->
  receive
    play ->
      handle_info(play, Player);
    Message ->
      self() ! play,
      handle_info(Message, Player)
  after
    Timeout ->
      handle_info(play, Player)
  end;

make_play(Player, _, _) ->
  ?MODULE:play(Player).

 
