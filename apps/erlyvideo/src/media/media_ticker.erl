%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Media ticker
% 
% 1. Стартуем
% 2. Запоминаем реальное время старта
% 3. Считываем N кадров
% 4. Запоминаем время первого кадра
% 5. Запоминаем время и next_id последнего кадра
% 6. Посылаем все кадры
% 7. Спим сколько надо
% 8. Идем на 3
% 
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(media_ticker).
-include_lib("erlmedia/include/video_frame.hrl").
-include("../log.hrl").

-export([start_link/3, init/3, loop/1, handle_message/2]).
-export([start/1, pause/1, resume/1, seek/2, stop/1, play_setup/2]).

-define(BURST_SIZE, 40).

-record(ticker, {
  media,
  consumer,
  stream_id,
  pos,
  dts,
  client_buffer,
  timer_start,
  playing_from,
  playing_till,
  paused = false,
  options,
  burst_size = ?BURST_SIZE,
  no_timeouts
}).

start(Ticker) ->
  Ticker ! start.

seek(Ticker, DTS) ->
  Ticker ! {seek, DTS}.

stop(Ticker) ->
  Ticker ! stop.

pause(Ticker) ->
  Ticker ! pause.

resume(Ticker) ->
  Ticker ! resume.

play_setup(Ticker, Options) ->
  Ticker ! {play_setup, Options}.

start_link(Media, Consumer, Options) ->
  proc_lib:start_link(?MODULE, init, [Media, Consumer, Options]).
  
init(Media, Consumer, Options) ->
  erlang:monitor(process, Media),
  erlang:monitor(process, Consumer),
  proc_lib:init_ack({ok, self()}),
  NoTimeouts = case application:get_env(erlyvideo, no_timeouts) of
    {ok, Val} -> Val;
    undefined -> false
  end,
  StreamId = proplists:get_value(stream_id, Options),
  BurstSize = proplists:get_value(burst_size, Options, ?BURST_SIZE),
  ClientBuffer = proplists:get_value(client_buffer, Options, 5000),
  Start = proplists:get_value(start, Options),
  SeekInfo = ems_media:seek_info(Media, Start, Options),
  case SeekInfo of
    {_Pos, _DTS} ->
      initialize_ticker(#ticker{media = Media, consumer = Consumer, stream_id = StreamId, client_buffer = ClientBuffer,
      burst_size = BurstSize, no_timeouts = NoTimeouts, options = Options}, SeekInfo);
    undefined ->
      ?D({failed_to_play_from, Start}),
      Consumer ! {ems_stream, StreamId, play_failed}
  end.
  

initialize_ticker(#ticker{options = Options, media = Media} = Ticker, {Pos, DTS}) ->
  Start = case proplists:get_value(start, Options, 0) of
    {_, S} -> S; % some time ago it was {before, Time}
    S -> S
  end,
  
  PlayingTill = case proplists:get_value(duration, Options) of
    undefined -> undefined;
    {before, Duration} ->
      Length = proplists:get_value(length, ems_media:info(Media)),
      if
        Duration > Length ->
          Start + Duration;
        true ->
          case ems_media:seek_info(Media, Start + Duration, Options) of
            {_Pos, EndTimestamp} -> EndTimestamp;
            _ -> undefined
          end
      end;
    Duration -> Start + Duration
  end,
  case PlayingTill of
    Start -> ?D({warning, "File duration is set to 0. Perhaps raise iphone segment size in streaming"});
    _ -> ok
  end,
  ?MODULE:loop(Ticker#ticker{pos = Pos, dts = DTS, playing_till = PlayingTill}).
  
loop(Ticker) ->
  receive
    Message -> safe_handle_message(Message, Ticker)
  end.

safe_handle_message(Message, Ticker) ->
  try ?MODULE:handle_message(Message, Ticker) of
    {stop, _Reason, _Ticker1} -> ok;
    {noreply, Ticker1} -> ?MODULE:loop(Ticker1);
    {restart, Message1, Ticker1} -> safe_handle_message(Message1, Ticker1)
  catch
    Class:Error ->
  	  Reason = io_lib_pretty_limited:print({Class,Error}, 5000),
  	  State = io_lib_pretty_limited:print(Ticker, 5000),
  	  Msg1 = io_lib_pretty_limited:print(Message, 5000),
      error_logger:error_report("** Media ticker ~p terminating \n"
             "** Last message in was ~s~n"
             "** When Server state == ~s~n"
             "** Reason for termination == ~n** ~s~n~p~n",
  	   [self(), Msg1, State, Reason, erlang:get_stacktrace()]),
  	  {Class,Error}
  end.

flush_tick() ->
  receive
    tick -> flush_tick()
  after 
    0 -> ok
  end.

notify_about_stop(#ticker{media = Media, dts = DTS, pos = Pos}) ->
  Media ! {ticker_stop, self(), DTS, Pos}.
  

handle_message({'DOWN', _Ref, process, _Pid, _Reason}, Ticker) ->
  notify_about_stop(Ticker),
  {stop, normal, Ticker};

handle_message(stop, Ticker) ->
  notify_about_stop(Ticker),
  {stop, normal, Ticker};

handle_message(resume, Ticker) ->
  self() ! tick,
  {noreply, Ticker#ticker{paused = false}};

handle_message(start, Ticker) ->
  self() ! tick,
  {noreply, Ticker};
  
handle_message(pause, Ticker) ->
  flush_tick(),
  {noreply, Ticker#ticker{paused = true, timer_start = undefined}};
  
handle_message({seek, DTS}, #ticker{media = Media, paused = Paused, stream_id = StreamId, consumer = Consumer, options = Options} = Ticker) ->
  {Pos,NewDTS} = ems_media:seek_info(Media, DTS, Options),
  % ?D({"Seek", DTS, Pos,NewDTS}),
  case Paused of
    true -> ok;
    false -> self() ! tick
  end,
  Consumer ! {ems_stream, StreamId, seek_success, DTS},
  {noreply, Ticker#ticker{pos = Pos, dts = NewDTS, timer_start = undefined}};

handle_message({play_setup, Options}, #ticker{client_buffer = OldCB, media = _Media, paused = Paused} = Ticker) ->
  % ?D({play_setup, self(), Options}),
  ClientBuffer = proplists:get_value(client_buffer, Options, OldCB),
  case Paused of
    true -> ok;
    false -> self() ! tick
  end,
  {noreply, Ticker#ticker{client_buffer = ClientBuffer}};


handle_message(tick, #ticker{media = Media, dts = DTS, pos = Pos, consumer = Consumer, stream_id = StreamId,
                             playing_till = PlayingTill, burst_size = BurstSize} = Ticker) ->
  Consumer ! {ems_stream, StreamId, burst_start},
  case load_frames(Media, Consumer, Pos, PlayingTill, BurstSize, []) of
    {eof, Frames} ->
      send_frames(Frames, Consumer, StreamId),
      Consumer ! {ems_stream, StreamId, burst_stop},
      Consumer ! {ems_stream, StreamId, play_complete, DTS},
      notify_about_stop(Ticker),
      {noreply, Ticker};
  
    {ok, [#video_frame{dts = NewDTS, next_id = NewPos} = Frame|_] = Frames} ->
      send_frames(Frames, Consumer, StreamId),
      Consumer ! {ems_stream, StreamId, burst_stop},
      Ticker1 = save_start_time(Ticker, Frames),
      Timeout = tick_timeout(Ticker1, Frame), 
      % ?D({burst, length(Frames), NewPos, round(NewDTS)}),
      Ticker2 = Ticker1#ticker{pos = NewPos, dts = NewDTS},
      receive
        Message ->
          {restart, Message, Ticker2}
      after
        Timeout -> 
          self() ! tick,
          {noreply, Ticker2}
      end
  end.

save_start_time(#ticker{timer_start = undefined, client_buffer = ClientBuffer, paused = Paused} = Ticker, Frames) ->
  [#video_frame{dts = NewDTS}|_] = lists:reverse(Frames),
  TimerStart = os:timestamp(),
  
  PlayingFrom = case Paused of
    true -> NewDTS - ClientBuffer;
    false -> NewDTS
  end,
  Ticker#ticker{timer_start = TimerStart, playing_from = PlayingFrom};
  
save_start_time(Ticker, _) ->
  Ticker.

load_frames(Media, Consumer, Pos, PlayingTill, Count, Frames) when Count > 0 ->
  case ems_media:read_frame(Media, Consumer, Pos) of
    eof ->
      {eof, Frames};
    
    #video_frame{dts = NewDTS} when NewDTS > PlayingTill ->
      {eof, Frames};
      
    #video_frame{next_id = NewPos} = Frame ->
      % ?D({read, NewPos, round(Frame#video_frame.dts)}),
      load_frames(Media, Consumer, NewPos, PlayingTill, Count - 1, [Frame|Frames])
  end;
  
load_frames(_Media, _Consumer, _Pos, _PlayingTill, 0, Frames) ->
  {ok, Frames}.


send_frames(Frames, Consumer, StreamId) ->
  [Consumer ! Frame#video_frame{stream_id = StreamId} || Frame <- lists:reverse(Frames)].


tick_timeout(#ticker{no_timeouts = true}, _Frame) ->
  0;

tick_timeout(#ticker{} = Ticker, Frame) ->
  Now = os:timestamp(),
  tick_timeout(Ticker, Frame, Now).

tick_timeout(#ticker{playing_from = PlayingFrom, timer_start = TimerStart, client_buffer = ClientBuffer},
             #video_frame{dts = DTS}, Now) ->
  NextTime = DTS - PlayingFrom,   %% Time from PlayingFrom in video timeline in which next frame should be seen
  RealTime = timer:now_diff(Now, TimerStart) div 1000,    %% Wall clock from PlayingFrom
  Sleep = NextTime - RealTime - ClientBuffer,    %% Delta between next show time and current wall clock delta
  T = if
    Sleep < 0 -> 0;                %% This case means, that frame was too late. show it immediately
    ClientBuffer >= NextTime -> 0; %% We have seen less than buffer size from stream begin
    true -> round(Sleep)           %% Regular situation: we are far from stream begin, feed with frames
  end,
  % ?D({tick,round(DTS),round(PlayingFrom),RealTime,T}),
  T.


-include_lib("eunit/include/eunit.hrl").


% timeout_in_buffer_from_start_test() ->
%   ?assertEqual(0, tick_timeout(232, 0, {0,0,8000}, {0,0,10000}, 3000)).
% 
% timeout_in_buffer_after_seek_test() ->
%   ?assertEqual(0, tick_timeout(10232, 10000, {0,0,8000}, {0,0,10000}, 3000)).
% 
% timeout_right_after_buffer_from_start_test() ->
%   ?assertEqual(40, tick_timeout(3042, 0, {0,0,8000}, {0,0,10000}, 3000)).
% 
% timeout_right_after_buffer_after_seek_test() ->
%   ?assertEqual(40, tick_timeout(13042, 10000, {0,0,8000}, {0,0,10000}, 3000)).
% 









