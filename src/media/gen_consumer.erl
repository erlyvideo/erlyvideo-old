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
      {noreply, S1} = M:handle_control({notfound, Name, Reason}, S),
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
      case M:handle_control({start_play,Name}, S) of
        {noreply, S1} ->
          {ok, MediaMode} = stream_media:subscribe(MediaEntry),
          Stream1 = Stream#ems_stream{mode = MediaMode, real_mode = MediaMode},
          self() ! start,
          ?D({"Start", Host, Name, Stream1#ems_stream.mode}),
          Stopped = false;
        {nostart, S1} ->
          Stream1 = Stream#ems_stream{mode = file, real_mode = file},
          Stopped = true,
          ?D({"Delayed start"})
      end,    
      ?MODULE:ready(Stream1#ems_stream{media_info = MediaEntry,name = Name,state = S1,
                                stopped = Stopped, paused = Stopped,
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
          case M:handle_control({seek_failed, Timestamp}, S) of
            {noreply, S1} -> ?MODULE:ready(State#ems_stream{state = S1});
            {stop, normal} -> ok;
            {stop, Reason} -> {Reason, State}
          end
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
      ?MODULE:ready(State#ems_stream{stopped = false, paused = false});

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
        undefined -> undefined;
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
  prepare_frame(Stream, Frame);

handle_frame(#ems_stream{ts_delta = undefined} = Stream, #video_frame{type = metadata} = Frame) ->
  ?D(Frame),
  prepare_frame(Stream, Frame);

handle_frame(#ems_stream{last_dts = undefined} = State, #video_frame{dts = DTS} = Frame) ->
  ?D({"Initializing gen_consumer with last_dts", DTS, Frame#video_frame.type}),
  handle_frame(State#ems_stream{last_dts = DTS}, Frame);

handle_frame(#ems_stream{ts_delta = undefined, last_dts = LastDTS} = Stream, #video_frame{decoder_config = false, dts = DTS, body = Body} = Frame) when is_binary(Body) andalso size(Body) > 0 andalso is_number(DTS) ->
  ?D({"Syncronizing gen_consumer on new base dts", DTS, Frame#video_frame.type}),
  handle_frame(Stream#ems_stream{ts_delta = LastDTS - DTS}, Frame); %% Lets glue new instance of stream to old one

handle_frame(#ems_stream{ts_delta = Delta} = Stream, #video_frame{dts = DTS, pts = PTS} = Frame) when is_number(Delta) ->
  prepare_frame(Stream, Frame#video_frame{dts = DTS + Delta, pts = PTS + Delta});

handle_frame(#ems_stream{} = Stream, eof) ->
  handle_eof(Stream);
  
handle_frame(ElseStream, ElseMsg) ->
  ?D("Function clause"),
  ?D(ElseStream),
  ?D(ElseMsg),
  erlang:exit(function_clause).



prepare_frame(#ems_stream{last_dts = Last} = Stream, #video_frame{dts = DTS} = Frame) ->
  save_config(Stream, Frame).


%% Clauses for old config, that just repeats
save_config(#ems_stream{video_config = #video_frame{body = Config}} = Player, 
            #video_frame{decoder_config = true, type = video, body = Config} = F) ->
  config_saved(Player, F);

save_config(#ems_stream{audio_config = #video_frame{body = Config}} = Player, 
           #video_frame{decoder_config = true, type = audio, body = Config} = F) ->
  config_saved(Player, F);

%% Now replacing config with new one
save_config(#ems_stream{} = Player, #video_frame{decoder_config = true, type = video} = F) ->
  ?D(save_video),
  config_saved(Player#ems_stream{video_config = F, sent_video_config = false}, F);

save_config(#ems_stream{} = Player, #video_frame{decoder_config = true, type = audio} = F) ->
  ?D(save_audio),
  config_saved(Player#ems_stream{audio_config = F, sent_audio_config = false}, F);

% save_config(#ems_stream{} = Player, #video_frame{type = metadata} = F) ->
%   config_saved(Player#ems_stream{metadata = F, sent_metadata = false}, F);

%% And just case for all other frames
save_config(#ems_stream{} = Player, #video_frame{} = Frame) ->
  send_config(Player, Frame).


config_saved(#ems_stream{mode = file} = Player, #video_frame{next_id = Next}) ->
  self() ! tick,
  ?D(config_saved),
  ?MODULE:ready(Player#ems_stream{pos = Next});
  
config_saved(Player, _) ->
  ?MODULE:ready(Player).
  
%% Here goes plenty, plenty of cases


%% This function tries to send decoder config if it is time to

% send_config(#ems_stream{module = M, state = S, metadata = Meta, sent_metadata = false} = Player, 
%             #video_frame{type = video, frame_type = keyframe, dts = DTS} = Frame) when is_record(Meta, video_frame) ->
%   % ?D({"Sent metadata", Meta}),
%   {noreply, S1} = M:handle_frame(Meta#video_frame{dts = DTS, pts = DTS}, S),
%   send_config(Player#ems_stream{sent_metadata = true, state = S1}, Frame);

send_config(#ems_stream{module = M, state = S, audio_config = A, sent_audio_config = false} = Player, 
           #video_frame{type = audio, dts = DTS} = Frame) when A =/= undefined ->
  {noreply, S1} = M:handle_frame(A#video_frame{dts = DTS, pts = DTS}, S),
  ?D({"Send audio config", DTS}),
  send_config(Player#ems_stream{sent_audio_config = true, state = S1}, Frame);

send_config(#ems_stream{module = M, state = S, video_config = V, sent_video_config = false} = Player, 
           #video_frame{type = video, frame_type = keyframe, dts = DTS} = Frame) when V =/= undefined ->
  {noreply, S1} = M:handle_frame(V#video_frame{dts = DTS, pts = DTS}, S),
  ?D({"Send video config", DTS}),
  send_config(Player#ems_stream{sent_video_config = true, state = S1}, Frame);
   
send_config(#ems_stream{} = Player, #video_frame{} = Frame) ->
  % ?D({Frame#video_frame.type, Player#ems_stream.audio_config, Player#ems_stream.sent_audio_config, Player#ems_stream.video_config, Player#ems_stream.sent_video_config}),
  try_send_frame(Player, Frame).



%% Now just sending frame at least

try_send_frame(#ems_stream{module = M, state = S, sent_audio_config = true} = Player, 
           #video_frame{type = audio, codec_id = aac} = Frame) ->
  send_frame(Player, Frame);

try_send_frame(#ems_stream{module = M, state = S, sent_video_config = true} = Player, 
           #video_frame{type = video, codec_id = h264} = Frame) ->
  send_frame(Player, Frame);

try_send_frame(#ems_stream{module = M, state = S} = Player, 
           #video_frame{codec_id = Codec} = Frame) when Codec =/= aac andalso Codec =/= h264 ->
  % ?D({Frame#video_frame.type, Frame#video_frame.dts, Frame#video_frame.codec_id}),
  send_frame(Player, Frame);

try_send_frame(#ems_stream{} = Player, #video_frame{type = _Type, dts = _DTS} = Frame) ->
  % ?D({"Refuse to sent unsynced frame", _Type, _DTS, _Frame#video_frame.frame_type}),
  frame_sent(Player, Frame).


send_frame(#ems_stream{module = M, state = S} = Player, #video_frame{next_id = Next} = Frame) ->
  % ?D({send, Frame#video_frame.type, Player#ems_stream.sent_video_config, Player#ems_stream.video_config}),
  case Frame#video_frame.type of
    metadata -> ?D(Frame);
    _ -> ok
  end,
  case M:handle_frame(Frame, S) of
    {noreply, S1} -> frame_sent(Player#ems_stream{pos = Next, state = S1}, Frame);
    stop -> handle_eof(Player)
  end.

frame_sent(#ems_stream{mode = file} = Player, Frame) ->
  tick_timeout(Frame, Player);
  
frame_sent(Player, _) ->
  ?MODULE:ready(Player).
  

%% @hidden
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
      ?MODULE:handle_info(tick, Player)
  end;

wait4tick(Player, _, _) ->
  ?MODULE:tick(Player).

 
