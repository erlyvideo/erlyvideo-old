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
-export([metadata/1, setopts/2, seek_info/3]).
-export([subscribe/2, unsubscribe/1, set_source/2, set_socket/2, read_frame/2, publish/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, print_state/1]).

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-define(LIFE_TIMEOUT, 60000).

-export([behaviour_info/1]).

%%-------------------------------------------------------------------------
%% @spec (Callbacks::atom()) -> CallBackList::list()
%% @doc  List of require functions in a video file reader
%% @hidden
%% @end
%%-------------------------------------------------------------------------
behaviour_info(callbacks) -> [{init, 1}, {handle_frame,2}, {handle_control,2}, {handle_info,2}];
behaviour_info(_Other) -> undefined.


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
  format,
  
  last_dts,
  ts_delta,
  
  life_timeout,
  timeout_ref
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
  gen_server:call(Media, {start, self()}),
  ok.

stop(Media) ->
  unsubscribe(Media).

subscribe(Media, Options) ->
  gen_server2:call(Media, {subscribe, self(), Options}).

unsubscribe(Media) ->
  gen_server:call(Media, {unsubscribe, self()}).
  
resume(Media) ->
  gen_server:call(Media, {resume, self()}).

pause(Media) ->
  gen_server:call(Media, {pause, self()}).

set_source(Media, Source) ->
  gen_server:cast(Media, {set_source, Source}).
  
set_socket(Media, Socket) ->
  gen_tcp:controlling_process(Socket, Media),
  gen_server:cast(Media, {set_socket, Socket}).
  
read_frame(Media, Key) ->
  gen_server2:call(Media, {read_frame, Key}).

seek(Media, BeforeAfter, DTS) ->
  gen_server2:call(Media, {seek, self(), BeforeAfter, DTS}).

seek_info(Media, BeforeAfter, DTS) ->
  gen_server2:call(Media, {seek_info, BeforeAfter, DTS}).
  
  
metadata(Media) ->
  gen_server2:call(Media, metadata).  

setopts(Media, Options) ->
  %TODO add options
  ok.
  
publish(Media, #video_frame{} = Frame) ->
  Media ! Frame.

  
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
  Media = #ems_media{options = Options, module = Module, 
                     clients = ets:new(clients, [set,  {keypos,#client.consumer}]),
                     life_timeout = proplists:get_value(life_timeout, Options, ?LIFE_TIMEOUT)},
  case Module:init(Options) of
    {ok, State} ->
      {Format,Storage} = case proplists:get_value(timeshift, Options) of
        undefined -> 
          {undefined, undefined};
        Timeshift when is_number(Timeshift) andalso Timeshift > 0 ->
          case array_timeshift:init(Options) of
            {ok, TSModule} ->
              {array_timeshift, TSModule};
            _ ->
              {undefined, undefined}
          end
      end,
      {ok, Media#ems_media{state = State, format = Format, storage = Storage}};
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
handle_call({subscribe, Client, Options}, From, #ems_media{timeout_ref = Ref} = Media) when Ref =/= undefined ->
  timer:cancel(Ref),
  handle_call({subscribe, Client, Options}, From, Media#ems_media{timeout_ref = undefined});


handle_call({subscribe, Client, Options}, _From, #ems_media{module = M, state = S, clients = Clients} = Media) ->
  StreamId = proplists:get_value(stream_id, Options),
  
  case M:handle_control({subscribe, Client, Options}, S) of
    {stop, Reason, S1} ->
      {stop, Reason, Media#ems_media{state = S1}};
    {reply, {error, Reason}, S1} ->
      {reply, {error, Reason}, Media#ems_media{state = S1}};
    {reply, Reply, S1} ->
      Ref = erlang:monitor(process,Client),
      RequireTicker = case Reply of
        tick -> true;
        _ -> proplists:get_value(start, Options) =/= undefined
      end,
      case RequireTicker of
        true ->
          {ok, Ticker} = ems_sup:start_ticker(self(), Client, Options),
          TickerRef = erlang:monitor(process,Ticker),
          Entry = #client{consumer = Client, stream_id = StreamId, ref = Ref, ticker = Ticker, ticker_ref = TickerRef, state = passive},
          ets:insert(Clients, Entry);
        false ->
          ets:insert(Clients, #client{consumer = Client, stream_id = StreamId, ref = Ref, state = starting})
      end,
      {reply, ok, Media#ems_media{state = S1}}
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
  case Format:seek(Storage, BeforeAfter, DTS) of
    {NewPos, NewDTS} ->
      case ets:lookup(Clients, Client) of
        [#client{ticker = Ticker, state = passive}] ->
          media_ticker:seek(Ticker, NewPos, NewDTS),
          {reply, {seek_success, NewDTS}, Media};
        
        [#client{stream_id = StreamId} = Entry] ->
          {ok, Ticker} = ems_sup:start_ticker(self(), Client, [{stream_id, StreamId}]),
          TickerRef = erlang:monitor(process,Ticker),
          media_ticker:seek(Ticker, NewPos, NewDTS),
          ets:insert(Clients, Entry#client{ticker = Ticker, ticker_ref = TickerRef, state = passive}),
          {reply, {seek_success, NewDTS}, Media};
        
        [] ->
          {reply, seek_failed, Media}
      end;    
    undefined ->
      {reply, seek_failed, Media}
  end;

handle_call({seek, _Client, _BeforeAfter, _DTS}, _From, #ems_media{format = undefined} = Media) ->
  {reply, seek_failed, Media};

%% It is information seek, required for outside needs.
handle_call({seek_info, BeforeAfter, DTS}, _From, #ems_media{format = Format, storage = Storage} = Media) ->
  {reply, Format:seek(Storage, BeforeAfter, DTS), Media};


handle_call({read_frame, Key}, _From, #ems_media{format = Format, storage = Storage} = Media) ->
  {reply, Format:read_frame(Storage, Key), Media};

handle_call(metadata, _From, #ems_media{} = Media) ->
  {reply, metadata_frame(Media), Media};

handle_call(info, _From, #ems_media{} = Media) ->
  {reply, storage_properties(Media), Media};

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
handle_cast({set_source, Source}, #ems_media{source_ref = OldRef, module = M, state = S1} = Media) ->
  case OldRef of
    undefined -> ok;
    _ -> erlang:demonitor(OldRef, [flush])
  end,
  case M:handle_control({set_source, Source}, S1) of
    {reply, _Reply, S2} ->
      Ref = erlang:monitor(process,Source),
      {noreply, Media#ems_media{source = Source, source_ref = Ref, state = S2}};
    {stop, Reason, S2} ->
      {stop, Reason, Media#ems_media{state = S2}}
  end;

handle_cast({set_socket, Socket}, #ems_media{module = M, state = S1} = Media) ->
  case M:handle_control({set_socket, Socket}, S1) of
    {reply, _Reply, S2} ->
      {noreply, Media#ems_media{state = S2}};
    {stop, Reason, S2} ->
      {stop, Reason, Media#ems_media{state = S2}}
  end;

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

handle_info({'DOWN', _Ref, process, Source, _Reason}, #ems_media{module = M, state = S1, source = Source, life_timeout = LifeTimeout} = Media) ->
  case M:handle_control({source_lost, Source}, S1) of
    {stop, Reason, S2} ->
      {stop, Reason, Media#ems_media{state = S2}};
    {reply, NewSource, S2} ->
      timer:send_after(LifeTimeout, graceful),
      {noreply, Media#ems_media{source = NewSource, state = S2}}
  end;
  % FIXME: should send notification
  % ems_event:stream_source_lost(Media#ems_stream.host, MediaInfo#media_info.name, self()),
  
handle_info({'DOWN', _Ref, process, Pid, Reason} = Msg, #ems_media{clients = Clients, module = M, state = S, life_timeout = LifeTimeout} = Media) ->
  Count = client_count(Media),
  {ok, TimeoutRef} = if
    Count < 2 -> timer:send_after(LifeTimeout, graceful);
    true -> {ok, undefined}
  end,
  Media1 = Media#ems_media{timeout_ref = TimeoutRef},
  case ets:lookup(Clients, Pid) of
    [#client{ticker = Ticker, ticker_ref = TickerRef}] when Ticker =/= undefined -> 
      erlang:demonitor(TickerRef, [flush]),
      (catch ems_media:stop(Ticker)),
      ets:delete(Clients, Pid),
      {noreply, Media1};
    [#client{}] -> 
      ets:delete(Clients, Pid),
      {noreply, Media1};
    [] -> 
      case ets:select(Clients, ets:fun2ms(fun(#client{ticker = Ticker} = Entry) when Ticker == Pid -> Entry end)) of
        [#client{consumer = Client, stream_id = StreamId, ref = Ref}] ->
          erlang:demonitor(Ref, [flush]),
          case Reason of 
            normal -> ok;
            _ -> Client ! {ems_stream, StreamId, play_failed}
          end,
          ets:delete(Clients, Client),
          {noreply, Media1};
        [] -> 
          case M:handle_info(Msg, S) of
            {noreply, S1} -> 
              {noreply, Media1#ems_media{state = S1}};
            {stop, Reason, S1} ->
              {stop, Reason, Media1#ems_media{state = S1}}
          end
      end
  end;
  

handle_info(#video_frame{} = Frame, #ems_media{} = Media) ->
  shift_dts(Frame, Media);

handle_info(graceful, #ems_media{source = undefined} = Media) ->
  case client_count(Media) of
    0 -> ?D("No clients and source"), {stop, normal, Media};
    _ -> {noreply, Media}
  end;
  
handle_info(graceful, #ems_media{} = Media) ->
  {noreply, Media};

handle_info(Message, #ems_media{module = M, state = S} = Media) ->
  case M:handle_info(Message, S) of
    {noreply, S1} ->
      {noreply, Media#ems_media{state = S1}};
    {stop, Reason, S1} ->
      {stop, Reason, Media#ems_media{state = S1}}
  end.


client_count(#ems_media{clients = Clients}) ->
  ets:info(Clients, size).


shift_dts(#video_frame{} = Frame, #ems_media{last_dts = undefined} = Media) ->
  shift_dts(Frame, Media#ems_media{last_dts = 0});

shift_dts(#video_frame{dts = DTS} = Frame, #ems_media{ts_delta = undefined, last_dts = LastDTS} = Media) ->
  ?D({"New instance of stream", LastDTS - DTS}),
  shift_dts(Frame, Media#ems_media{ts_delta = LastDTS - DTS}); %% Lets glue new instance of stream to old one

shift_dts(#video_frame{dts = DTS, pts = PTS} = Frame, #ems_media{ts_delta = Delta} = Media) ->
  % ?D({Frame#video_frame.type, Frame#video_frame.dts, Delta, DTS + Delta}),
  handle_shifted_frame(Frame#video_frame{dts = DTS + Delta, pts = PTS + Delta}, Media).

handle_shifted_frame(#video_frame{} = Frame, #ems_media{format = Format, storage = Storage} = Media) ->
  % ?D({Frame#video_frame.type, Frame#video_frame.frame_type, Frame#video_frame.dts}),
  start_on_keyframe(Frame, Media),
  Storage1 = save_frame(Format, Storage, Frame),
  handle_config(Frame, Media#ems_media{storage = Storage1}).

handle_config(#video_frame{type = video, body = Config}, #ems_media{video_config = #video_frame{body = Config}} = Media) -> 
  {noreply, Media};

handle_config(#video_frame{type = audio, body = Config}, #ems_media{audio_config = #video_frame{body = Config}} = Media) -> 
  {noreply, Media};

handle_config(#video_frame{type = video, decoder_config = true} = Config, #ems_media{} = Media) -> 
  handle_frame(Config, Media#ems_media{video_config = Config});

handle_config(#video_frame{type = audio, decoder_config = true} = Config, #ems_media{} = Media) -> 
  handle_frame(Config, Media#ems_media{audio_config = Config});
  
handle_config(Frame, Media) ->
  handle_frame(Frame, Media).

handle_frame(#video_frame{type = Type} = Frame, #ems_media{module = M, state = S, clients = Clients} = Media) ->
  case M:handle_frame(Frame, S) of
    {reply, F, S1} ->
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
  end.


save_frame(undefined, Storage, _) ->
  Storage;

save_frame(Format, Storage, Frame) ->
  case Format:write_frame(Frame, Storage) of
    {ok, Storage1} -> Storage1;
    _ -> Storage
  end.

start_on_keyframe(#video_frame{type = video, frame_type = keyframe, dts = DTS, decoder_config = false} = _F, #ems_media{clients = Clients, video_config = Video, audio_config = Audio} = M) ->
  MS = ets:fun2ms(fun(#client{state = starting, consumer = Client, stream_id = StreamId}) -> {Client,StreamId} end),
  Starting = ets:select(Clients, MS),
  [ets:update_element(Clients, Client, {#client.state, active}) || {Client,_} <- Starting],
  case Video of
    undefined -> ok;
    _ -> [Client ! Video#video_frame{dts = DTS, pts = DTS, stream_id = StreamId} || {Client,StreamId} <- Starting]
  end,
  case Audio of
    undefined -> ok;
    _ -> [Client ! Audio#video_frame{dts = DTS, pts = DTS, stream_id = StreamId} || {Client,StreamId} <- Starting]
  end,
  case metadata_frame(M) of
    undefined -> ok;
    Meta -> [Client ! Meta#video_frame{dts = DTS, pts = DTS, stream_id = StreamId} || {Client,StreamId} <- Starting]
  end,
  M;

start_on_keyframe(_, Media) ->
  Media.

storage_properties(#ems_media{format = undefined}) ->
  [];

storage_properties(#ems_media{format = Format, storage = Storage}) ->
  Props = Format:properties(Storage),
  case proplists:get_value(duration, Props) of
    undefined -> Props;
    Duration -> [{length,Duration*1000}|Props]
  end.


metadata_frame(#ems_media{format = undefined}) ->
  undefined;
  
metadata_frame(#ems_media{} = Media) ->
   #video_frame{type = metadata, body = [<<"onMetaData">>, {object, storage_properties(Media)}]}.
  

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
