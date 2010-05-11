%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        One iphone stream. It is statefull and has life timeout
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
-module(iphone_stream).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_consumer).
-include("../../include/ems.hrl").
-include_lib("erlmedia/include/video_frame.hrl").


%% External API
-export([start_link/1, start_link/3, segment/3]).

% gen_consumer behaviour
-export([handle_frame/2, handle_control/2, handle_info/2, init/2, terminate/1]).


-record(iphone_stream, {
  play_end = undefined,
  mpegts,
  name,
  consumer,
  host,
  seek,
  duration,
  buffer = []
}).


start_link(Options) ->
  gen_consumer:start_link(?MODULE, Options, proplists:get_value(name,Options)).

start_link(Host, Name, Options) ->
  gen_consumer:start_link(?MODULE, [{host,Host}|Options], Name).


segment(MediaEntry, SeekTarget, DurationTarget) ->
  {Seek, _BaseTS, PlayingFrom} = case SeekTarget of
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

  PlayEnd = case DurationTarget of
    undefined -> undefined;
    {BeforeAfterEnd, Duration} ->
      Length = proplists:get_value(length, media_provider:info(MediaEntry)),
      {_, DurationFrom} = SeekTarget,
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


%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Options,Name) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------


init(Options, Name) ->
  Host = proplists:get_value(host, Options),
  Seek = proplists:get_value(seek, Options),
  Duration = proplists:get_value(duration, Options),
  Consumer = proplists:get_value(consumer, Options),
  self() ! {play, Name, Options},
  {ok, #iphone_stream{mpegts = mpegts:init(), host = Host, seek = Seek, duration = Duration, consumer = Consumer}}.


handle_frame(#video_frame{dts = DTS} = _Frame, #iphone_stream{play_end = PlayEnd} = _Stream) when DTS > PlayEnd andalso is_number(PlayEnd) ->
  ?D({"Stop due to playend", DTS, PlayEnd}),
  stop;

handle_frame(#video_frame{} = Frame, #iphone_stream{consumer = Consumer} = Stream) ->
  % ?D({Frame#video_frame.type, round(Frame#video_frame.dts)}),
  Consumer ! Frame,
  {noreply, Stream}.


%% Handles specific control messages from gen_consumer
handle_control({notfound, _Name, _Reason}, Stream) ->
  {noreply, Stream};

handle_control({start_play, Name}, #iphone_stream{host = Host, seek = Seek, duration = Duration} = Stream) ->
  Media = media_provider:find(Host, Name),
  {_SeekPos, PlayingFrom, PlayEnd} = iphone_stream:segment(Media, Seek, Duration),
  
  ?D({"Seek:", PlayingFrom, PlayEnd, (catch PlayEnd - PlayingFrom)}),

  case Seek of
    {BeforeAfterSeek, SeekTime} -> self() ! {seek, BeforeAfterSeek, SeekTime};
    _ -> ok
  end,
  {nostart, Stream#iphone_stream{name = Name, play_end = PlayEnd}};

handle_control({play_complete, _Length}, #iphone_stream{consumer = Consumer} = Stream) ->
  Consumer ! {ems_stream, 0,play_complete,0},
  {noreply, Stream};

handle_control(stop, Stream) ->
  {noreply, Stream};

handle_control({seek_failed, _Timestamp}, Stream) ->
  {stop, normal};

handle_control({seek_notify, _Timestamp}, Stream) ->
  {noreply, Stream}.


%% Handles inbox messages
handle_info(stop, _Stream) ->
  stop;

handle_info(_Message, Stream) ->
  {noreply, Stream}.


terminate(_Stream) ->
  ok.
