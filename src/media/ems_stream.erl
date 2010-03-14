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
-include_lib("erlyvideo/include/video_frame.hrl").

-export([file_dir/1, file_format/1, start_link/1, client/1, init/1, ready/1, play/1, wait/1]).



-record(ems_stream, {
  consumer,
  media_info,
	stream_id,
	sent_video_config = false,
	sent_audio_config = false,
	base_dts = undefined,
	paused = false,
	send_audio = true,
	send_video = true,
	
	mode,
	host,
	
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
  ?MODULE:wait(Stream).
  

wait(Stream) ->
  receive
    Message ->
      handle_wait(Message, Stream)
  end.

handle_wait({client_buffer, Buffer}, Stream) ->
  wait(Stream#ems_stream{client_buffer = Buffer});

handle_wait({play, Name, Options}, #ems_stream{host = Host, consumer = Consumer, stream_id = StreamId} = Stream) ->
  case media_provider:find_or_open(Host, Name) of
    {notfound, Reason} ->
      Consumer ! {ems_stream, StreamId, {notfound, Reason}},
      wait(Stream);
    {'DOWN', _, process, Consumer, _} ->
      ok;
    MediaEntry ->
      erlang:monitor(process, MediaEntry),
      Consumer ! {ems_stream, StreamId, start_play},
      {ok, Mode} = gen_server:call(MediaEntry, {subscribe, self()}),
      self() ! start,
      prepare(Stream#ems_stream{media_info = MediaEntry, mode = Mode,
       timer_start = element(1, erlang:statistics(wall_clock))}, Options)
  end.
      

prepare(#ems_stream{mode = stream} = Stream, _Options) ->
  ready(Stream);

prepare(#ems_stream{media_info = MediaEntry, mode = file} = Stream, Options) ->
  {Seek, BaseTS, PlayingFrom} = case proplists:get_value(seek, Options) of
    undefined -> {undefined, 0, 0};
    SeekTo ->
      case file_media:seek(MediaEntry, SeekTo) of
        {Pos, NewTimestamp} ->
          ?D({"Starting from", round(SeekTo), NewTimestamp}),
          {Pos, NewTimestamp, NewTimestamp};
        _ ->
          {undefined, 0, 0}
      end
  end,
  
  
  PlayEnd = case proplists:get_value(duration_before, Options) of
    undefined -> undefined;
    Duration -> 
      case file_media:seek(MediaEntry, PlayingFrom + Duration) of
        {_Pos, EndTimestamp} -> EndTimestamp;
        _ -> undefined
      end
  end,
  % ?D({"Seek:", Seek, BaseTS, PlayingFrom, PlayEnd}),
  ready(Stream#ems_stream{pos = Seek,
                     base_dts = BaseTS,
                     playing_from = PlayingFrom,
                     play_end = PlayEnd,
                     client_buffer = proplists:get_value(client_buffer, Options, 10000)}).
  


ready(#ems_stream{mode = file} = State) ->
  receive
    Message ->
      handle_file(Message, State)
  end;

ready(#ems_stream{mode = stream} = State) ->
  receive
    Message ->
      handle_stream(Message, State)
  end.
  
handle_file(Message, #ems_stream{media_info = MediaInfo, 
                    consumer = Consumer, 
                    client_buffer = ClientBuffer,
                    stream_id = StreamId} = State) ->
  case Message of
    {client_buffer, NewClientBuffer} -> 
      ?MODULE:ready(State#ems_stream{client_buffer = NewClientBuffer});
      
    start ->
      case file_media:metadata(MediaInfo) of
        undefined -> ok;
        MetaData -> Consumer ! #video_frame{type = metadata, stream_id = StreamId, body = [<<?AMF_COMMAND_ONMETADATA>>, MetaData]}
      end,
    	self() ! play,
      ?MODULE:ready(State#ems_stream{prepush = ClientBuffer, stopped = false, paused = false});
      
    {client, Pid, Ref} ->
      Pid ! {gen_fsm:sync_send_event(Consumer, info), Ref},
      ?MODULE:ready(State);
      
    pause ->
      ?D("Player paused"),
      ?MODULE:ready(State#ems_stream{paused = true});

    resume ->
      ?D("Player resumed"),
      self() ! play,
      ?MODULE:ready(State#ems_stream{paused = false});
      
    {send_audio, Audio} ->
      ?D({"Send audio", Audio}),
      ?MODULE:ready(State#ems_stream{send_audio = Audio});

    {send_video, Video} ->
      ?D({"Send video", Video}),
      ?MODULE:ready(State#ems_stream{send_video = Video});

    {seek, Timestamp} ->
      case file_media:seek(MediaInfo, Timestamp) of
        {Pos, NewTimestamp} ->
          ?D({"Player real seek to", round(Timestamp), NewTimestamp}),
          self() ! play,
          ?MODULE:ready(State#ems_stream{pos = Pos, 
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
      
    {'DOWN', _Ref, process, _Pid, _Reason} ->
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
  send_frame(Player, Reply);
  
play(Else) ->
  ?D(Else),
  ok.

send_frame(#ems_stream{mode=stream,sent_video_config = true} = Player, #video_frame{decoder_config = true, type = video}) ->
  ?MODULE:ready(Player);

send_frame(#ems_stream{mode=stream,sent_audio_config = true} = Player, #video_frame{decoder_config = true, type = audio}) ->
  ?MODULE:ready(Player);

send_frame(#ems_stream{mode=stream,synced = false} = Player, #video_frame{decoder_config = false, frame_type = frame}) ->
  ?MODULE:ready(Player);

send_frame(#ems_stream{mode=stream,synced = false} = Player, #video_frame{decoder_config = false, frame_type = keyframe} = VideoFrame) ->
  send_frame(Player#ems_stream{mode=stream,synced = true}, VideoFrame);



send_frame(#ems_stream{mode=stream,base_dts = undefined} = Player, #video_frame{dts = Ts} = Frame) when is_number(Ts) andalso Ts > 0 ->
  send_frame(Player#ems_stream{mode=stream,base_dts = Ts}, Frame);

send_frame(#ems_stream{mode=stream,consumer = Consumer, stream_id = StreamId, base_dts = BaseTs} = Player, 
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
    {true, audio} -> Player#ems_stream{mode=stream,sent_audio_config = true};
    {true, video} -> Player#ems_stream{mode=stream,sent_video_config = true};
    _ -> Player
  end,
  ?MODULE:ready(Player1);



send_frame(#ems_stream{play_end = PlayEnd}, {#video_frame{dts = Timestamp}, _}) when PlayEnd =< Timestamp ->
  ok;

send_frame(Player, undefined) ->
  self() ! play,
  ?MODULE:ready(Player);

send_frame(Player, {undefined, undefined}) ->
  self() ! play,
  ?MODULE:ready(Player);

send_frame(Player, {#video_frame{body = undefined}, Next}) ->
  self() ! play,
  ?MODULE:ready(Player#ems_stream{pos = Next});
  
send_frame(#ems_stream{} = _Player, {done, undefined}) ->
  ok;

send_frame(#ems_stream{} = _Player, done) ->
  ok;

send_frame(#ems_stream{consumer = Consumer, stream_id = StreamId, base_dts = BaseDTS} = Player, {#video_frame{dts = DTS, pts = PTS} = Frame, Next}) ->
  Frame1 = case DTS of
    0 ->
      Frame#video_frame{stream_id = StreamId, dts = DTS + BaseDTS, pts = PTS + BaseDTS};
    _ ->
      Frame#video_frame{stream_id = StreamId}
  end,
  Consumer ! Frame1,    
  timeout_play(Frame1, Player#ems_stream{pos = Next}).
  

handle_stream(Message, #ems_stream{consumer = Consumer} = State) ->
  case Message of
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
      ?MODULE:ready(State#ems_stream{mode=stream,paused = true});

    resume ->
      ?D("Player resumed"),
      ?MODULE:ready(State#ems_stream{mode=stream,paused = false});

    {send_audio, Audio} ->
      ?D({"Send audio", Audio}),
      ?MODULE:ready(State#ems_stream{mode=stream,send_audio = Audio});

    {send_video, Video} ->
      ?D({"Send video", Video}),
      ?MODULE:ready(State#ems_stream{mode=stream,send_video = Video});

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
      ".3gp" -> mp4;
      ".mp4" -> mp4;
      ".MP4" -> mp4;
      ".mov" -> mp4;
      ".m4v" -> mp4;
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

  % ?D({"Timeout", Timeout, AbsTime, PlayingFrom, ClientBuffer, (element(1, erlang:statistics(wall_clock)) - TimerStart)}),
  make_play(Player, Prepush - SeekTime, round(Timeout)).
  
make_play(Player, Prepush, _Timeout) when Prepush > 0 ->
  ?MODULE:play(Player#ems_stream{prepush = Prepush});
  
make_play(Player, _Prepush, Timeout) when Timeout > 0 ->
  receive
    play ->
      handle_file(play, Player);
    Message ->
      self() ! play,
      handle_file(Message, Player)
  after
    Timeout ->
      handle_file(play, Player)
  end;

make_play(Player, _, _) ->
  ?MODULE:play(Player).

 
