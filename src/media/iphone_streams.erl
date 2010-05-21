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
-module(iphone_streams).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/video_frame.hrl").

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-define(STREAM_TIME, 10000).
-define(TIMEOUT, 10000).

%% External API
-export([start_link/1, find/3, segments/2, segment_info/3, play/4]).

-export([save_counters/4, get_counters/3]).





start_link(Options) ->
  gen_cache:start_link([{local, ?MODULE}|Options]).


%%--------------------------------------------------------------------
%% @spec (Host, Name, Number) -> {ok,Pid}
%%
%% @doc Starting new iphone segment
%% @end
%%----------------------------------------------------------------------
find(Host, Name, Number) ->
  {_, Count, _, Type} = segments(Host, Name),
  Options = case {Count,Type} of
    {Number,stream} -> [{client_buffer,0}]; % Only for last segment of stream timeshift we disable length
    _ -> [{client_buffer,?STREAM_TIME*2},{duration, {'before', ?STREAM_TIME}}]
  end,
  {ok, _Pid} = media_provider:play(Host, Name, [{consumer,self()},{start, {'before', Number * ?STREAM_TIME}}|Options]).


segment_info(_MediaEntry, Name, Number) ->
  % {_Seek, PlayingFrom, PlayEnd} = ems_stream:segment(MediaEntry, [{seek, {'before', Number * ?STREAM_TIME}}, {duration, {'before', ?STREAM_TIME}}]),
  % case {PlayingFrom, PlayEnd} of
  %   {PlayingFrom, PlayEnd} when is_number(PlayingFrom) andalso is_number(PlayEnd) -> 
  %     SegmentLength = round((PlayEnd - PlayingFrom)/1000),
  %     io_lib:format("#EXTINF:~p,~n/iphone/segments/~s/~p.ts~n", [SegmentLength, Name, Number]);
  %   _Else -> 
  %     undefined
  % end.
  io_lib:format("#EXTINF:~p,~n/iphone/segments/~s/~p.ts~n", [?STREAM_TIME div 1000, Name, Number]).
  

segments(Host, Name) ->
  Info = media_provider:info(Host, Name),
  Duration = proplists:get_value(length, Info, 0),
  Type = proplists:get_value(type, Info),
  Start = trunc(proplists:get_value(start, Info, 0) / ?STREAM_TIME),
  ?D({"Segments: ~p, ~p, ~p~n", [Duration, Type, Start]}),
  SegmentLength = ?STREAM_TIME div 1000,
  Count = if 
    Duration > 2*?STREAM_TIME -> round(Duration/?STREAM_TIME);
    true -> 1
  end,
  {Start,Count,SegmentLength,Type}.
  

play(Host, Name, Number, Req) ->
  case iphone_streams:find(Host, Name, Number) of
    {ok, PlayerPid} ->
      Counters = iphone_streams:get_counters(Host, Name, Number),
      io:format("Get counters for ~p:~p#~p: ~p~n", [Host, Name, Number, Counters]),
      NextCounters = mpegts_play:play(Name, PlayerPid, Req, Counters),
      iphone_streams:save_counters(Host, Name, Number+1, NextCounters),
      ok;
    {notfound, Reason} ->
      Req:stream(io_lib:format("404 Page not found.\n ~p: ~s ~s\n", [Name, Host, Reason])),
      Req:stream(close);
    Reason -> 
      Req:stream(io_lib:format("500 Internal Server Error.~n Failed to start video player: ~p~n ~p: ~p", [Reason, Name, Req])),
      Req:stream(close)
  end.
  
  
  
save_counters(Host, Name, Number, Counters) ->
  gen_cache:set(?MODULE, {Host, Name, Number}, Counters).

get_counters(Host, Name, Number) ->
  gen_cache:get(?MODULE, {Host, Name, Number}, {0,0,0,0}).

