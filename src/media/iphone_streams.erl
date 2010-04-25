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
-behaviour(gen_server).
-include_lib("erlmedia/include/video_frame.hrl").

-define(STREAM_TIME, 10000).
-define(TIMEOUT, 10000).

%% External API
-export([start_link/1, find/3, segments/2, segment_info/3, play/4]).

-export([save_counters/4, get_counters/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(iphone_streams, {
  cache = []
}).



start_link(Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).


%%--------------------------------------------------------------------
%% @spec (Channel::integer(), Message::text) -> {ok}
%%
%% @doc Called by client, that wants to receive {mpegts, Bin} packets for Time
%% @end
%%----------------------------------------------------------------------
find(Host, Name, Number) ->
  {_, Count, _, _} = segments(Host, Name),
  Buffer = case Count of
    Number -> 0;
    _ -> ?STREAM_TIME*2
  end,
  media_provider:play(Host, Name, [{stream_id, 1}, {seek, {'before', Number * ?STREAM_TIME}}, {duration, {'before', ?STREAM_TIME}}, {client_buffer, Buffer}]).

find_(Host, Name, Number) ->
  gen_server:call(?MODULE, {find, Host, Name, Number}).


segment_info(MediaEntry, Name, Number) ->
  {_Seek, PlayingFrom, PlayEnd} = ems_stream:segment(MediaEntry, [{seek, {'before', Number * ?STREAM_TIME}}, {duration, {'before', ?STREAM_TIME}}]),
  case {PlayingFrom, PlayEnd} of
    {PlayingFrom, PlayEnd} when is_number(PlayingFrom) andalso is_number(PlayEnd) -> 
      SegmentLength = round((PlayEnd - PlayingFrom)/1000),
      io_lib:format("#EXTINF:~p,~n/iphone/segments/~s/~p.ts~n", [SegmentLength, Name, Number]);
    _Else -> 
      undefined
  end.
  

segments(Host, Name) ->
  Info = media_provider:info(Host, Name),
  Duration = proplists:get_value(length, Info),
  Type = proplists:get_value(type, Info),
  Start = trunc(proplists:get_value(start, Info) / ?STREAM_TIME),
  io:format("Segments: ~p, ~p, ~p~n", [Duration, Type, Start]),
  SegmentLength = ?STREAM_TIME div 1000,
  Count = round(Duration/?STREAM_TIME),
  case Type of
    file -> {Start,Count,SegmentLength,Type};
    stream -> {Start+1,Count-1,SegmentLength,Type}
  end.
  

play(Host, Name, Number, Req) ->
  case iphone_streams:find(Host, Name, Number) of
    {ok, PlayerPid} ->
      Counters = iphone_streams:get_counters(Host, Name, Number),
      io:format("Get counters for ~p:~p#~p: ~p~n", [Host, Name, Number, Counters]),
      NextCounters = mpegts_lib:play(Name, PlayerPid, Req, Counters),
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
  gen_server:call(?MODULE, {save, Host, Name, Number, Counters}).

get_counters(Host, Name, Number) ->
  {ok, Counters} = gen_server:call(?MODULE, {get, Host, Name, Number}),
  Counters.


%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------


init([_Options]) ->
  {ok, #iphone_streams{}}.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
% handle_call({play, Host, Name, Number}, _From, #iphone_streams{streams = Streams} = State) ->
%   case lists:keytake({Host,Name,Number}, 1, Streams) of
%     {value, {Key,Player}, Streams1} ->
%       Key1 = {Host,Name,Number+1},
%       {reply, {ok, Player}, State#iphone_streams{streams = lists:keystore(Key,1,Streams,{Key1,Player})}};
%     false ->
%       {reply, {ok, Player}, }
%   {reply, ok, State#iphone_stream{consumer = Consumer, buffer = []}, 2*Time};

handle_call({save, Host, Name, Number, Counters}, _From, #iphone_streams{cache = Cache} = State) ->
  {Now, _} = erlang:statistics(wall_clock),
  Key = {Host, Name, Number},
  Cache1 = lists:keystore(Key, 1, Cache, {Key, Counters, Now}),
  {reply, ok, State#iphone_streams{cache = Cache1}, ?TIMEOUT};

handle_call({get, Host, Name, Number}, _From, #iphone_streams{cache = Cache} = State) ->
  Key = {Host, Name, Number},
  Counters = case lists:keyfind(Key, 1, Cache) of
    false -> {0,0,0,0};
    {_, Count, _} -> Count
  end,
  {reply, {ok, Counters}, State, ?TIMEOUT};
  
handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info(timeout, #iphone_streams{cache = Cache} = Server) ->
  {Now, _} = erlang:statistics(wall_clock),
  Limit = Now - ?TIMEOUT,
  Cache1 = lists:filter(fun({_,_,Time}) when Time < Limit -> false;
                           (_) -> true end, Cache),
  {noreply, Server#iphone_streams{cache = Cache1}};


handle_info(_Info, #iphone_streams{} = State) ->
  io:format("Unknown message: ~p~n", [_Info]),
  {noreply, State}.



%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
