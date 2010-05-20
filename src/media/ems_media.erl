%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Erlyvideo media
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
-module(ems_media).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% External API
-export([start_link/2]).
-export([play/2, stop/1, resume/1, pause/1, seek/3]).
-export([metadata/1, setopts/2]).
-export([subscribe/2, unsubscribe/1, set_source/2, read_frame/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, print_state/1]).

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).


start_link(Module, Options) ->
  gen_server2:start_link(?MODULE, [Module, Options], []).


-record(ems_media, {
  module,
  state,
  type,
  options,
  video_config,
  audio_config,
  metadata,
  clients,
  source,
  source_ref,
  storage,
  format
}).


-record(client, {
  consumer,
  ref,
  stream_id,
  ticker,
  ticker_ref,
  state = paused
}).

%%--------------------------------------------------------------------
%% @spec (Channel::integer(), Message::text) -> {ok}
%%
%% @doc Call some function
%% @end
%%----------------------------------------------------------------------

play(Media, Options) ->
  ok = subscribe(Media, Options),
  ?D({play,Media,Options}),
  gen_server:call(Media, {start, self()}),
  ok.

stop(Media) ->
  unsubscribe(Media).

subscribe(Media, Options) ->
  gen_server:call(Media, {subscribe, self(), Options}).

unsubscribe(Media) ->
  gen_server:call(Media, {unsubscribe, self()}).
  
resume(Media) ->
  gen_server:call(Media, {resume, self()}).

pause(Media) ->
  gen_server:call(Media, {pause, self()}).

set_source(Media, Source) ->
  gen_server:call(Media, {set_source, Source}).
  
read_frame(Media, Key) ->
  gen_server2:call(Media, {read_frame, Key}).

seek(Media, BeforeAfter, DTS) ->
  ?D({"Call seek on", Media}),
  gen_server2:call(Media, {seek, self(), BeforeAfter, DTS}).
  
  
metadata(Media) ->
  gen_server2:call(Media, metadata).  

setopts(Media, Options) ->
  %TODO add options
  ok.
  
%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

print_state(#ems_media{} = Media) ->
  Media#ems_media{storage = storage}.


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


init([Module, Options]) ->
  ?D({init,Module,Options}),
  Media = #ems_media{options = Options, module = Module, clients = ets:new(clients, [set,  {keypos,#client.consumer}])},
  case Module:init(Options) of
    {ok, State} ->
      {ok, Media#ems_media{state = State}};
    {ok, State, {Format, Storage}} ->
      ?D({inited_with_storage,Format}),
      {ok, Media#ems_media{state = State, format = Format, storage = Storage}};
    {stop, Reason} ->
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
handle_call({subscribe, Client, Options}, _From, #ems_media{module = M, state = S, clients = Clients} = Media) ->
  StreamId = proplists:get_value(stream_id, Options),
  
  case M:handle_control({subscribe, Client, Options}, S) of
    {ok, S1} ->
      Ref = erlang:monitor(process,Client),
      ets:insert(Clients, #client{consumer = Client, stream_id = StreamId, ref = Ref, state = starting}),
      {reply, ok, Media#ems_media{state = S1}};
    {ok, S1, tick} ->
      Ref = erlang:monitor(process,Client),
      {ok, Ticker} = ems_sup:start_ticker(self(), Client, Options),
      TickerRef = erlang:monitor(process,Ticker),
      Entry = #client{consumer = Client, stream_id = StreamId, ref = Ref, ticker = Ticker, ticker_ref = TickerRef, state = passive},
      ets:insert(Clients, Entry),
      {reply, ok, Media#ems_media{state = S1}};
    {error, Reason} ->
      {reply, {error, Reason}, Media}
  end;
  
handle_call({unsubscribe,Client}, _From, #ems_media{clients = Clients} = Media) ->
  case ets:lookup(Clients, Client) of
    [#client{ref = Ref, ticker = Ticker, ticker_ref = TickerRef}] when TickerRef =/= undefined ->
      erlang:demonitor(TickerRef, [flush]),
      (catch media_ticker:stop(Ticker)),
      erlang:demonitor(Ref, [flush]),
      ets:delete(Clients, Client),
      {reply, ok, Media};
      
    [#client{ref = Ref}] ->
      erlang:demonitor(Ref, [flush]),
      ets:delete(Clients, Client),
      {reply, ok, Media};

    [] ->
      {reply, {error, no_client}, Media}
  end;


handle_call({start, Client}, From, Media) ->
  handle_call({resume, Client}, From, Media);
  
handle_call({resume, Client}, _From, #ems_media{clients = Clients} = Media) ->
  case ets:lookup(Clients, Client) of
    [#client{state = passive, ticker = Ticker}] ->
      media_ticker:start(Ticker),
      {reply, ok, Media};

    [#client{state = paused}] ->
      ets:update_element(Clients, Client, {#client.state, starting}),
      {reply, ok, Media};

    [] ->
      {reply, {error, no_client}, Media};
      
    _ ->
      {reply, ok, Media}
  end;
      
handle_call({pause, Client}, _From, #ems_media{clients = Clients} = Media) ->
  case ets:lookup(Clients, Client) of
    [#client{state = passive, ticker = Ticker}] ->
      media_ticker:pause(Ticker),
      {reply, ok, Media};
    
    [#client{}] ->
      ets:update_element(Clients, Client, {#client.state, paused}),
      {reply, ok, Media};
      
    [] ->
      {reply, {error, no_client}, Media}
  end;
      
handle_call({seek, Client, BeforeAfter, DTS}, _From, #ems_media{clients = Clients, format = Format, storage = Storage} = Media) when Format =/= undefined ->
  ?D({seek,Client,BeforeAfter,DTS}),
  case Format:seek(Storage, BeforeAfter, DTS) of
    {NewPos, NewDTS} ->
      ?D({seek_ticker,NewPos,NewDTS}),
      case ets:lookup(Clients, Client) of
        [#client{ticker = Ticker, state = passive}] ->
          media_ticker:seek(Ticker, NewPos, NewDTS),
          {reply, {seek_success, NewDTS}, Media};
        
        [#client{stream_id = StreamId} = Entry] ->
          {ok, Ticker} = ems_sup:start_ticker(self(), Client, [{stream_id, StreamId}]),
          TickerRef = erlang:monitor(process,Ticker),
          media_ticker:seek(Ticker, NewPos, NewDTS),
          ets:insert(Clients, Entry#client{ticker = Ticker, ticker_ref = TickerRef, state = passive}),
          {reply, {seek_success, NewDTS}, Media}
      end;    
    undefined ->
      {reply, seek_failed, Media}
  end;

handle_call({seek, _Client, _BeforeAfter, _DTS}, _From, #ems_media{format = undefined} = Media) ->
  {reply, seek_failed, Media};

handle_call({set_source, Source}, _From, #ems_media{source_ref = OldRef} = Media) ->
  case OldRef of
    undefined -> ok;
    _ -> erlang:demonitor(OldRef, [flush])
  end,
  Ref = erlang:monitor(process,Source),
  {reply, ok, Media#ems_media{source = Source, source_ref = Ref}};

handle_call({read_frame, Key}, _From, #ems_media{format = Format, storage = Storage} = Media) ->
  {reply, Format:read_frame(Storage, Key), Media};

handle_call({seek, BeforeAfter, DTS}, _From, #ems_media{format = Format, storage = Storage} = Media) ->
  {reply, Format:seek(Storage, BeforeAfter, DTS), Media};

handle_call(metadata, _From, #ems_media{format = Format, storage = Storage} = Media) ->
  {reply, {object, Format:properties(Storage)}, Media};

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

handle_info({'DOWN', _Ref, process, Source, _Reason}, #ems_media{source = Source} = Media) ->
  % FIXME: should wait for timeout
  % ems_event:stream_source_lost(Media#ems_stream.host, MediaInfo#media_info.name, self()),
  {stop, normal, Media};
  
handle_info({'DOWN', _Ref, process, Pid, _Reason}, #ems_media{clients = Clients} = Media) ->
  case ets:lookup(Clients, Pid) of
    [#client{ticker = Ticker, ticker_ref = TickerRef}] when Ticker =/= undefined -> 
      erlang:demonitor(TickerRef, [flush]),
      (catch ems_media:stop(Ticker)),
      ets:delete(Clients, Pid);
    [#client{}] -> 
      ets:delete(Clients, Pid);
    [] -> 
      case ets:select(Clients, ets:fun2ms(fun(#client{ticker = Ticker} = Entry) when Ticker == Pid -> Entry end)) of
        [] -> ?D({"Unknown pid died!", Pid});
        [#client{consumer = Client, stream_id = StreamId, ref = Ref}] ->
          erlang:demonitor(Ref, [flush]),
          Client ! {ems_stream, StreamId, play_failed},
          ets:delete(Clients, Client)
      end
  end,
  case client_count(Media) of
    0 -> timer:send_after(10000, graceful);
    _ -> ok
  end,
  {noreply, Media};
  

handle_info(#video_frame{type = Type} = Frame, #ems_media{module = M, state = S, clients = Clients} = Media) ->
  start_on_keyframe(Frame, Media),
  case M:handle_frame(Frame, S) of
    {ok, F, S1} ->
      case Type of
        audio -> send_frame(F, Clients, starting);
        _ -> ok
      end,
      send_frame(F, Clients, active),
      {noreply, Media#ems_media{state = S1}};
    {noreply, S1} ->
      {noreply, Media#ems_media{state = S1}};
    {stop, Reason, S1} ->
      {stop, Reason, Media#ems_media{state = S1}}
  end;

handle_info(graceful, #ems_media{source = undefined} = Media) ->
  case client_count(Media) of
    0 -> ?D("No clients and source"), {stop, normal, Media};
    _ -> {noreply, Media}
  end;
  
handle_info(graceful, #ems_media{} = Media) ->
  {noreply, Media};

handle_info(Info, _State) ->
  {stop, {unhandled, Info}}.


client_count(#ems_media{clients = Clients}) ->
  ets:info(Clients, size).

start_on_keyframe(#video_frame{type = video, frame_type = keyframe}, #ems_media{clients = Clients} = M) ->
  MS = ets:fun2ms(fun(#client{state = starting, consumer = Client}) -> Client end),
  Starting = ets:select(Clients, MS),
  [ets:update_element(Clients, Client, {#client.state, active}) || Client <- Starting],
  M;

start_on_keyframe(_, Media) ->
  Media.

send_frame(Frame, Clients, State) ->
  MS = ets:fun2ms(fun(#client{state = S, consumer = Client, stream_id = StreamId}) when S == State -> {Client, StreamId} end),
  List = ets:select(Clients, MS),
  [Pid ! Frame#video_frame{stream_id = StreamId} || {Pid,StreamId} <- List].

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
