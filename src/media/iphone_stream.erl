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
-behaviour(gen_server).
-include_lib("erlmedia/include/video_frame.hrl").


%% External API
-export([start_link/3, consume/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(iphone_stream, {
  time = 10000,
  player,
  paused = false,
  mpegts,
  next_dts,
  consumer,
  buffer = []
}).


start_link(Host, Name, Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Name, Options], []).


%%--------------------------------------------------------------------
%% @spec (Channel::integer(), Message::text) -> {ok}
%%
%% @doc Called by client, that wants to receive {mpegts, Bin} packets for Time
%% @end
%%----------------------------------------------------------------------
consume(Consumer) ->
  gen_server:call(?MODULE, {consume, Consumer}).



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


init([Host, Name, Options]) ->
  Time = proplists:get_value(time, Options, 10000),
  case media_provider:play(Host, Name, [{stream_id, 1}]) of
    {ok, PlayerPid} ->
      link(PlayerPid),
      erlang:monitor(PlayerPid),
      {ok, #iphone_stream{mpegts = mpegts:init(), time = Time, player = PlayerPid, next_dts = Time}, Time*2};
    Reason ->
      {stop, Reason}
  end.

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
handle_call({consume, Consumer}, _From, #iphone_stream{time = Time, buffer = Buffer, player = Player} = State) ->
  erlang:monitor(process, Consumer),
  lists:foreach(fun(Frame) ->
    Consumer ! Frame
  end, Buffer),
  Player ! resume,
  {reply, ok, State#iphone_stream{consumer = Consumer, buffer = []}, 2*Time};
  
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
handle_info({'DOWN', process, _Client, _Reason}, #iphone_stream{time = Time} = Server) ->
  {noreply, Server, 2*Time};

handle_info(#video_frame{} = Frame, #iphone_stream{paused = true, buffer = Buffer, time = Time} = Stream) ->
  {noreply, Stream#iphone_stream{buffer = Buffer ++ [Frame]}, 2*Time};

handle_info(#video_frame{} = Frame, #iphone_stream{consumer = undefined, buffer = Buffer, time = Time} = Stream) ->
  {noreply, Stream#iphone_stream{buffer = Buffer ++ [Frame]}, 2*Time};

handle_info(#video_frame{} = Frame, #iphone_stream{paused = false, time = Time} = Stream) ->
  Stream1 = handle_frame(Frame, Stream),
  {noreply, Stream1, 2*Time};

handle_info(_Info, #iphone_stream{time = Time} = State) ->
  io:format("Unknown message: ~p~n", [_Info]),
  {noreply, State, 2*Time}.


handle_frame(#video_frame{dts = DTS, type = video, frame_type = keyframe} = Frame, 
             #iphone_stream{player = Player, next_dts = NextDTS, time = Time} = Stream) when DTS >= NextDTS ->
  Player ! pause,
  Stream#iphone_stream{buffer = [Frame], next_dts = DTS + Time};
  
handle_frame(#video_frame{} = Frame, #iphone_stream{consumer = Consumer} = Stream) ->
  Consumer ! Frame,
  Stream.
  


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
