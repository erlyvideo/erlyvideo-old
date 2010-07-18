%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Erlyvideo media
%%% EMS media is a process, that works both as a splitter of ``#video_frame'' flow between
%%% active clients and as a source of frames, used by passive clients.
%%%
%%% It is important to understand, that media is immutable across clients. It doesn't remember,
%%% what point in file or timeshift, client is watching now. However, any media, that is started
%%% with storage, respond to {@link ems_media:read_frame/2} method.
%%%
%%% Look first at schema of media serving:<br/>
%%% <img src="media-structure.png" />
%%% 
%%%
%%% You can create your own process, that will call ``media_provider:play(Host, Name, Options)'' on streams
%%% and pass this process to other created streams, using {@link set_source/2.}
%%%
%%% If you look at the source, you will see, that ems_media requires module for starting. ems_media is 
%%% an infrastructure for specific functionality. Specific functionality is how to start ems_media,
%%% what to do when source is lost, how to subscribe clients.
%%% 
%%% Currently there are:
%%% <ul>
%%%  <li>{@link file_media. file reading for ems_media}</li>
%%%  <li>{@link live_media. live published streams}</li>
%%%  <li>{@link mpegts_file_media. stream mpegts files}</li>
%%%  <li>{@link mpegts_media. stream mpegts or accept PUT mpegts}</li>
%%%  <li>{@link rtmp_media. go to remote rtmp servers for video}</li>
%%%  <li>{@link rtsp_media. grab video from RTSP cameras}</li>
%%% </ul>
%%%
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
-include("../include/ems_media.hrl").
-include("../include/ems.hrl").

%% External API
-export([start_link/2, start_custom/2]).
-export([play/2, stop/1, resume/1, pause/1, seek/3]).
-export([metadata/1, info/1, setopts/2, seek_info/3, status/1]).
-export([subscribe/2, unsubscribe/1, set_source/2, set_socket/2, read_frame/2, publish/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, format_status/1]).


-define(LIFE_TIMEOUT, 60000).

-export([behaviour_info/1]).

%%-------------------------------------------------------------------------
%% @spec (Callbacks::atom()) -> CallBackList::list()
%% @doc  List of require functions in a video file reader
%% @hidden
%% @end
%%-------------------------------------------------------------------------
behaviour_info(callbacks) -> [{init, 2}, {handle_frame,2}, {handle_control,2}, {handle_info,2}];
behaviour_info(_Other) -> undefined.


%% @private
start_link(Module, Options) ->
  gen_server:start_link(?MODULE, [Module, Options], []).

%% @private
start_custom(Module, Options) ->
  gen_server:start_link(Module, [Options], []).




-record(client, {
  consumer,
  ref,
  stream_id,
  ticker,
  ticker_ref,
  state = paused
}).

%%--------------------------------------------------------------------
%% @spec (Media::pid(), Options::list()) -> ok
%%
%% @doc Subscribe caller to stream and starts playing. Look {@link subscribe/2.} for options.
%% @end
%%----------------------------------------------------------------------
play(Media, Options) ->
  ok = subscribe(Media, Options),
  gen_server:call(Media, {start, self()}),
  ok.

%%--------------------------------------------------------------------
%% @spec (Media::pid()) -> ok
%%
%% @doc The same as {@link unsubscribe/2.}
%% @end
%%----------------------------------------------------------------------
stop(Media) ->
  unsubscribe(Media).

%%----------------------------------------------------------------------
%% @spec (Media::pid(), Options::list()) -> ok
%%
%% @doc Subscribe caller to stream with options. Options are:
%% <dl>
%% <dt>``{stream_id,StreamId}''</dt>
%% <dd>Each ``#video_frame'' has stream_id field. All frames to caller will have provided StreamId.
%% It is usefull to connect from one process to many streams</dd>
%% <dt>``{client_buffer, ClientBuffer}''</dt>
%% <dd>If you are reading file, than first ClientBuffer milliseconds will be sent to you without timeout.
%% Very important for fast seeks and playstarts</dd>
%% </dl>
%% @end
%%----------------------------------------------------------------------
subscribe(Media, Options) ->
  gen_server:call(Media, {subscribe, self(), Options}).

%%----------------------------------------------------------------------
%% @spec (Media::pid()) -> ok
%%
%% @doc Unsubscribe caller from stream
%% @end
%%----------------------------------------------------------------------
unsubscribe(Media) ->
  gen_server:call(Media, {unsubscribe, self()}).
  
%%----------------------------------------------------------------------
%% @spec (Media::pid()) -> ok
%%
%% @doc Resume stream for calling client
%% @end
%%----------------------------------------------------------------------
resume(Media) ->
  gen_server:call(Media, {resume, self()}).

%%----------------------------------------------------------------------
%% @spec (Media::pid()) -> ok
%%
%% @doc Pauses stream for calling client
%% @end
%%----------------------------------------------------------------------
pause(Media) ->
  gen_server:call(Media, {pause, self()}).

%%----------------------------------------------------------------------
%% @spec (Media::pid(), Source::pid()) -> ok
%%
%% @doc Sets new source of frames for media. Media can work only with one source
%% @end
%%----------------------------------------------------------------------
set_source(Media, Source) ->
  gen_server:cast(Media, {set_source, Source}).

%%----------------------------------------------------------------------
%% @spec (Media::pid(), Socket::port()) -> ok
%%
%% @doc Passes socket to media. Generic ems_media doesn't know anything about 
%% such sockets, but it will call ``Module:handle_control({set_socket, Socket}, State)'' on
%% submodule. For example, PUT mpegts requires it.
%% @end
%%----------------------------------------------------------------------
set_socket(Media, Socket) ->
  gen_tcp:controlling_process(Socket, Media),
  gen_server:cast(Media, {set_socket, Socket}).
  
%%----------------------------------------------------------------------
%% @spec (Media::pid(), Key::any()) -> Frame::video_frame() |
%%                                     eof
%%
%% @doc Read frame from media. Call with Key = undefined to read first frame. Next keys
%% will be in ``#video_frame.next_id'' field.
%% Caller is responsible to wait for proper timeout between frames
%% @end
%%----------------------------------------------------------------------
read_frame(Media, Key) ->
  gen_server:call(Media, {read_frame, Key}).

%%----------------------------------------------------------------------
%% @spec (Media::pid(), BeforeAfter::before|after, DTS::number()) -> ok |
%%                                                                   {error, Reason}
%%
%% @doc Seek in storage. Looks either keyframe before DTS or keyframe after DTS.
%% Seeks private caller stream and starts sending frames from NewDTS.
%% @end
%%----------------------------------------------------------------------
seek(Media, BeforeAfter, DTS) ->
  gen_server:call(Media, {seek, self(), BeforeAfter, DTS}).

%%----------------------------------------------------------------------
%% @spec (Media::pid(), BeforeAfter::before|after, DTS::number()) -> {Key::any(), NewDTS::number()} |
%%                                                                   undefined
%%
%% @doc Seek in storage. Looks either keyframe before DTS or keyframe after DTS.
%% Returns Key for this keyframe and its NewDTS.
%% @end
%%----------------------------------------------------------------------
seek_info(Media, BeforeAfter, DTS) ->
  gen_server:call(Media, {seek_info, BeforeAfter, DTS}).


%%----------------------------------------------------------------------
%% @spec (Media::pid()) -> Status::list()
%%  
%%
%% @doc Returns miscelaneous info about media, such as client_count
%% @end
%%----------------------------------------------------------------------
status(Media) ->
  gen_server:call(Media, status).
  
%%----------------------------------------------------------------------
%% @spec (Media::pid()) -> Metadata::video_frame()
%%
%% @doc Returns video_frame, prepared to send into flash
%% @end
%%----------------------------------------------------------------------
metadata(Media) ->
  gen_server:call(Media, metadata).  

%%----------------------------------------------------------------------
%% @spec (Media::pid()) -> Info::list()
%%
%% @doc Information about stream
%% @end
%%----------------------------------------------------------------------
info(Media) ->
  gen_server:call(Media, info).

%%----------------------------------------------------------------------
%% @spec (Media::pid(), Options::list()) -> ok
%%
%% @doc One day will set same options as in {@link subscribe/2.} dynamically
%% Available options:
%% buffer_size : BufferSize - size of prepush in seconds
%% send_video : boolean() - send video or not
%% send_audio : boolean() - send audio or not
%% @end
%%----------------------------------------------------------------------
setopts(Media, Options) ->
  %TODO add options
  ok.
  
  
%%----------------------------------------------------------------------
%% @spec (Media::pid(), Frame::video_frame()) -> any()
%%
%% @doc Publishes frame to media
%% @end
%%----------------------------------------------------------------------
publish(Media, #video_frame{} = Frame) ->
  Media ! Frame.

  
%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%% @hidden
format_status(#ems_media{} = Media) ->
  Media#ems_media{storage = storage}.


%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @private
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------




init([Module, Options]) ->
  ?D({init,Module,Options}),
  Name = proplists:get_value(name, Options),
  URL = proplists:get_value(url, Options),
  Media = #ems_media{options = Options, module = Module, name = Name, url = URL, type = proplists:get_value(type, Options),
                     clients = ets:new(clients, [set,  {keypos,#client.consumer}])},
  ?D("Starting"),
  case Module:init(Media, Options) of
    {ok, Media1} ->
      Media2 = case proplists:get_value(timeshift, Options) of
        undefined -> 
          Media1;
        Timeshift when is_number(Timeshift) andalso Timeshift > 0 ->
          case array_timeshift:init(Options) of
            {ok, TSData} ->
              Media1#ems_media{format = array_timeshift, storage = TSData};
            _ ->
              Media1
          end
      end,
      ?D("Started"),
      Media3 = Media2#ems_media{
        source_timeout = or_time(proplists:get_value(source_timeout, Options), Media2#ems_media.source_timeout),
        clients_timeout = or_time(proplists:get_value(clients_timeout, Options), Media2#ems_media.clients_timeout),
        retry_limit = or_time(proplists:get_value(retry_limit, Options), Media2#ems_media.retry_limit)
      },
      
      {ok, Media3, ?TIMEOUT};
    {stop, Reason} ->
      ?D({"ems_media failed to initialize",Module,Reason}),
      {stop, Reason}
  end.


or_time(undefined, undefined) -> ?LIFE_TIMEOUT;
or_time(undefined, Timeout) -> Timeout;
or_time(Timeout, _) -> Timeout.

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
handle_call({subscribe, Client, Options}, From, #ems_media{clients_timeout_ref = Ref} = Media) when Ref =/= undefined ->
  timer:cancel(Ref),
  handle_call({subscribe, Client, Options}, From, Media#ems_media{clients_timeout_ref = undefined});


handle_call({subscribe, Client, Options}, _From, #ems_media{module = M, clients = Clients} = Media) ->
  StreamId = proplists:get_value(stream_id, Options),

  DefaultSubscribe = fun(Reply, #ems_media{audio_config = A, last_dts = DTS} = Media1) ->
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
        % case V of
        %   undefined -> ok;
        %   _ -> Client ! V#video_frame{dts = DTS, pts = DTS, stream_id = StreamId}
        % end,
        case A of
          undefined -> ok;
          _ -> Client ! A#video_frame{dts = DTS, pts = DTS, stream_id = StreamId}
        end,
        case metadata_frame(Media1) of
          undefined -> ok;
          Meta -> Client ! Meta#video_frame{dts = DTS, pts = DTS, stream_id = StreamId}
        end,
        ets:insert(Clients, #client{consumer = Client, stream_id = StreamId, ref = Ref, state = starting})
    end,
    {reply, ok, Media1, ?TIMEOUT}
  end,
  
  case M:handle_control({subscribe, Client, Options}, Media) of
    {stop, Reason, Media1} ->
      ?D({"ems_media failed to subscribe",Client,M,Reason}),
      {stop, Reason, Media1};
    {reply, {error, Reason}, Media1} ->
      {reply, {error, Reason}, Media1, ?TIMEOUT};
    {reply, Reply, Media1} ->
      DefaultSubscribe(Reply, Media1);
    {noreply, Media1} ->
      DefaultSubscribe(ok, Media1)
  end;
  
handle_call({stop, Client}, _From, Media) ->
  unsubscribe_client(Client, Media);
  
handle_call({unsubscribe, Client}, _From, Media) ->
  unsubscribe_client(Client, Media);


handle_call({start, Client}, From, Media) ->
  handle_call({resume, Client}, From, Media);
  
handle_call({resume, Client}, _From, #ems_media{clients = Clients} = Media) ->
  case ets:lookup(Clients, Client) of
    [#client{state = passive, ticker = Ticker}] ->
      media_ticker:start(Ticker),
      {reply, ok, Media, ?TIMEOUT};

    [#client{state = paused}] ->
      ets:update_element(Clients, Client, {#client.state, starting}),
      {reply, ok, Media, ?TIMEOUT};

    [] ->
      {reply, {error, no_client}, Media, ?TIMEOUT};
      
    _ ->
      {reply, ok, Media, ?TIMEOUT}
  end;
      
handle_call({pause, Client}, _From, #ems_media{clients = Clients} = Media) ->
  case ets:lookup(Clients, Client) of
    [#client{state = passive, ticker = Ticker}] ->
      ?D({"Pausing passive client", Client}),
      media_ticker:pause(Ticker),
      {reply, ok, Media, ?TIMEOUT};
    
    [#client{}] ->
      ?D({"Pausing active client", Client}),
      ets:update_element(Clients, Client, {#client.state, paused}),
      {reply, ok, Media, ?TIMEOUT};
      
    [] ->
      ?D({"No client to pause", Client}),
      {reply, {error, no_client}, Media, ?TIMEOUT}
  end;
      
handle_call({seek, Client, BeforeAfter, DTS} = Seek, _From, #ems_media{module = M} = Media) ->
  
  DefaultReply = fun({NewPos, NewDTS}, #ems_media{clients = Clients} = Media1) ->
    case ets:lookup(Clients, Client) of
      [#client{ticker = Ticker, state = passive}] ->
        media_ticker:seek(Ticker, NewPos, NewDTS),
        {reply, {seek_success, NewDTS}, Media1, ?TIMEOUT};
    
      [#client{stream_id = StreamId} = Entry] ->
        {ok, Ticker} = ems_sup:start_ticker(self(), Client, [{stream_id, StreamId}]),
        TickerRef = erlang:monitor(process,Ticker),
        media_ticker:seek(Ticker, NewPos, NewDTS),
        ets:insert(Clients, Entry#client{ticker = Ticker, ticker_ref = TickerRef, state = passive}),
        {reply, {seek_success, NewDTS}, Media1, ?TIMEOUT};
    
      [] ->
        {reply, seek_failed, Media1, ?TIMEOUT}
    end
  end,
  
  DefaultSeek = fun(#ems_media{format = undefined} = Media1) ->
    {reply, seek_failed, Media1, ?TIMEOUT};
                   (#ems_media{format = Format, storage = Storage} = Media1) ->
    case Format:seek(Storage, BeforeAfter, DTS) of
      {NewPos, NewDTS} ->
        DefaultReply({NewPos, NewDTS}, Media1);
      undefined ->
        {reply, seek_failed, Media1, ?TIMEOUT}
    end
  end,
  
  case M:handle_control(Seek, Media) of
    {noreply, Media1} ->
      DefaultSeek(Media1);
    {stop, Reason, Media1} ->
      {stop, Reason, Media1};
    {reply, Reply, Media1} ->
      DefaultReply(Reply, Media1)
  end;


%% It is information seek, required for outside needs.
handle_call({seek_info, BeforeAfter, DTS}, _From, #ems_media{format = Format, storage = Storage} = Media) ->
  {reply, Format:seek(Storage, BeforeAfter, DTS), Media, ?TIMEOUT};


handle_call({read_frame, Key}, _From, #ems_media{format = Format, storage = Storage} = Media) ->
  Frame = Format:read_frame(Storage, Key),
  Media1 = case Frame of
    #video_frame{content = video, flavor = config} -> Media#ems_media{video_config = Frame};
    #video_frame{content = audio, flavor = config} -> Media#ems_media{audio_config = Frame};
    _ -> Media
  end,
  {reply, Frame, Media1, ?TIMEOUT};

handle_call(metadata, _From, #ems_media{} = Media) ->
  {reply, metadata_frame(Media), Media, ?TIMEOUT};

handle_call(info, _From, #ems_media{} = Media) ->
  {reply, storage_properties(Media), Media, ?TIMEOUT};

handle_call(status, _From, #ems_media{} = Media) ->
  {reply, [{client_count, client_count(Media)}], Media, ?TIMEOUT};

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
handle_cast({set_source, Source}, #ems_media{source_ref = OldRef, module = M} = Media) ->
  case OldRef of
    undefined -> ok;
    _ -> erlang:demonitor(OldRef, [flush])
  end,
  
  DefaultSource = fun(OtherSource, Media1) ->
    Ref = case OtherSource of
      undefined -> undefined;
      _ -> erlang:monitor(process, OtherSource)
    end,
    {noreply, Media1#ems_media{source = OtherSource, source_ref = Ref, ts_delta = undefined}, ?TIMEOUT}
  end,
  
  case M:handle_control({set_source, Source}, Media) of
    {noreply, Media1} ->
      DefaultSource(Source, Media1);
    {reply, OtherSource, Media1} ->
      DefaultSource(OtherSource, Media1);
    {stop, Reason, S2} ->
      ?D({"ems_media failed to set_source", M, Source, Reason}),
      {stop, Reason, Media#ems_media{state = S2}}
  end;

handle_cast(Cast, #ems_media{module = M} = Media) ->
  case M:handle_control(Cast, Media) of
    {noreply, Media1} ->
      {noreply, Media1, ?TIMEOUT};
    {reply, _Reply, Media1} ->
      {noreply, Media1, ?TIMEOUT};
    {stop, Reason, Media1} ->
      ?D({"ems_media failed to cast", M, Cast, Reason}),
      {stop, Reason, Media1}
  end.

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

handle_info({'DOWN', _Ref, process, Source, _Reason}, #ems_media{module = M, source = Source, source_timeout = SourceTimeout} = Media) ->
  ?D({"ems_media lost source", Source, _Reason}),
  case M:handle_control({source_lost, Source}, Media) of
    {stop, Reason, Media1} ->
      ?D({"ems_media is stopping due to source_lost", M, Source, Reason}),
      {stop, Reason, Media1};
    {noreply, Media1} when is_number(SourceTimeout) andalso SourceTimeout > 0 ->
      ?D({"ems_media lost source and sending graceful", SourceTimeout, round(Media1#ems_media.last_dts)}),
      mark_clients_as_starting(Media),
      {ok, Ref} = timer:send_after(SourceTimeout, no_source),
      {noreply, Media1#ems_media{source = undefined, source_ref = undefined, source_timeout_ref = Ref}, ?TIMEOUT};
    {noreply, Media1} when SourceTimeout == false ->
      ?D({"ems_media lost source but source_timeout = false"}),
      mark_clients_as_starting(Media),
      {noreply, Media1#ems_media{source = undefined, source_ref = undefined}, ?TIMEOUT};
    {reply, NewSource, Media1} ->
      ?D({"ems_media lost source and sending graceful, but have new source", SourceTimeout, NewSource}),
      Ref = erlang:monitor(process, NewSource),
      mark_clients_as_starting(Media),
      {noreply, Media1#ems_media{source = NewSource, source_ref = Ref, ts_delta = undefined}, ?TIMEOUT}
  end;
  % FIXME: should send notification
  % ems_event:stream_source_lost(Media#ems_stream.host, MediaInfo#media_info.name, self()),

  
handle_info({'DOWN', _Ref, process, Pid, ClientReason} = Msg, #ems_media{clients = Clients, module = M} = Media) ->
  case unsubscribe_client(Pid, Media) of
    {reply, {error, no_client}, Media2, _} ->
      case ets:select(Clients, ets:fun2ms(fun(#client{ticker = Ticker} = Entry) when Ticker == Pid -> Entry end)) of
        [#client{consumer = Client, stream_id = StreamId}] ->
          case ClientReason of 
            normal -> ok;
            _ -> Client ! {ems_stream, StreamId, play_failed}
          end,
          case unsubscribe_client(Client, Media2) of
            {reply, _Reply, Media3, _} -> {noreply, Media3, ?TIMEOUT};
            {stop, Reason, Media3} -> 
              ?D({"ems_media is stopping after unsubscribe", M, Client, Reason}),
              {stop, Reason, Media3}
          end;
        [] -> 
          case M:handle_info(Msg, Media2) of
            {noreply, Media3} -> {noreply, Media3};
            {stop, Reason, Media3} -> 
              ?D({"ems_media is stopping after handling info", M, Msg, Reason}),
              {stop, normal, Media3}
          end
      end;
    {reply, _Reply, Media2, _} -> {noreply, Media2, ?TIMEOUT};
    {stop, Reason, Media2} -> 
      ?D({"ems_media is stopping after unsubscribe", M, Pid, Reason}),
      {stop, Reason, Media2}
  end;

handle_info(#video_frame{} = Frame, #ems_media{} = Media) ->
  % ?D({Frame#video_frame.content, Frame#video_frame.flavor, Frame#video_frame.dts}),
  case transcode(Frame) of
    undefined ->
      {noreply, Media, ?TIMEOUT};
    Transcoded ->  
      shift_dts(Transcoded, Media)
  end;

handle_info(no_source, #ems_media{source = undefined, module = M} = Media) ->
  case M:handle_control(no_source, Media) of
    {noreply, Media1} ->
      ?D({"Media has no source after timeout and exiting", self(), Media#ems_media.name}),
      {stop, normal, Media1};
    {stop, Reason, Media1} ->
      ?D({"Media has no source after timeout and exiting", self(), Media#ems_media.name}),
      {stop, Reason, Media1};
    {reply, NewSource, Media1} ->
      Ref = erlang:monitor(process, NewSource),
      mark_clients_as_starting(Media),
      {noreply, Media1#ems_media{source = NewSource, source_timeout_ref = undefined, source_ref = Ref, ts_delta = undefined}, ?TIMEOUT}
  end;


handle_info(no_clients, #ems_media{module = M} = Media) ->
  case client_count(Media) of
    0 ->
      ?D("graceful received, handling"),
      case M:handle_control(no_clients, Media) of
        {noreply, Media1} ->
          ?D({"ems_media is stopping", M, Media#ems_media.name}),
          {stop, normal, Media1};
        {stop, Reason, Media1} ->
          ?D({"ems_media is stopping after graceful", M, Reason}),
          {stop, Reason, Media1};
        {reply, ok, Media1} ->
          ?D({M, "rejected stopping of ems_media due to 0 clients"}),
          {noreply, Media1#ems_media{clients_timeout_ref = undefined}, ?TIMEOUT}
      end;
    _ -> {noreply, Media, ?TIMEOUT}
  end;
  



% handle_info(timeout, #ems_media{timeout_ref = Ref} = Media) when Ref =/= undefined ->
%   {noreply, Media}; % No need to set timeout, because Timeout is already going to arrive
% 
handle_info(timeout, #ems_media{module = M, source = Source} = Media) when Source =/= undefined ->
  case M:handle_control(timeout, Media) of
    {stop, Reason, Media1} ->
      error_logger:error_msg("Source of media doesnt send frames, stopping...~n"),
      {stop, Reason, Media1};
    {noreply, Media1} ->
      {noreply, Media1, ?TIMEOUT};
    {reply, _Reply, Media1} ->
      {noreply, Media1, ?TIMEOUT}
  end;

handle_info(Message, #ems_media{module = M} = Media) ->
  case M:handle_info(Message, Media) of
    {noreply, Media1} ->
      {noreply, Media1, ?TIMEOUT};
    {stop, Reason, Media1} ->
      ?D({"ems_media is stopping after handle_info", M, Message, Reason}),
      {stop, Reason, Media1}
  end.




transcode(#video_frame{content = audio, codec = Codec} = Frame) when Codec == pcma orelse 
                                                                     Codec == pcm orelse
                                                                     % Codec == pcm_le orelse
                                                                     Codec == pcmu ->
  ems_sound:adapt_sound(Frame);

transcode(Frame) ->
  Frame.



unsubscribe_client(Client, #ems_media{clients = Clients, module = M, clients_timeout = Timeout} = Media) ->
  case ets:lookup(Clients, Client) of
    [#client{ref = Ref, ticker = Ticker, ticker_ref = TickerRef}] ->
      case M:handle_control({unsubscribe, Client}, Media) of
        {noreply, Media1} ->
          case TickerRef of
            undefined -> ok;
            _ ->
              erlang:demonitor(TickerRef, [flush]),
              (catch media_ticker:stop(Ticker))
          end,
        
          erlang:demonitor(Ref, [flush]),
          ets:delete(Clients, Client),

          Count = client_count(Media1),
          {ok, TimeoutRef} = case Count of
            0 when is_number(Timeout) -> 
              ?D({"No clients, sending delayed graceful", Timeout}), 
              timer:send_after(Timeout, no_clients);
            _ -> 
              {ok, undefined}
          end,
          {reply, ok, Media1#ems_media{clients_timeout_ref = TimeoutRef}, ?TIMEOUT};
        {reply, Reply, Media1} ->
          {reply, Reply, Media1, ?TIMEOUT};

        {stop, Reason, Media1} ->
          {stop, Reason, Media1}
      end;    

    [] ->
      {reply, {error, no_client}, Media, ?TIMEOUT}
  end.


mark_clients_as_starting(#ems_media{clients = Clients}) ->
  MS = ets:fun2ms(fun(#client{state = active, consumer = Client}) -> Client end),
  Starting = ets:select(Clients, MS),
  [ets:update_element(Clients, Client, {#client.state, starting}) || Client <- Starting],
  ok.


client_count(#ems_media{clients = Clients}) ->
  ets:info(Clients, size).


shift_dts(#video_frame{} = Frame, #ems_media{last_dts = undefined} = Media) ->
  shift_dts(Frame, Media#ems_media{last_dts = 0});

shift_dts(#video_frame{dts = undefined} = Frame, #ems_media{last_dts = LastDTS} = Media) ->
  handle_shifted_frame(Frame#video_frame{dts = LastDTS, pts = LastDTS}, Media);

shift_dts(#video_frame{dts = DTS} = Frame, #ems_media{ts_delta = undefined, last_dts = LastDTS} = Media) ->
  ?D({"New instance of stream", LastDTS, DTS, LastDTS - DTS}),
  shift_dts(Frame, Media#ems_media{ts_delta = LastDTS - DTS}); %% Lets glue new instance of stream to old one

shift_dts(#video_frame{dts = DTS, pts = PTS} = Frame, #ems_media{ts_delta = Delta} = Media) ->
  % ?D({Frame#video_frame.content, round(Frame#video_frame.dts), round(Delta), round(DTS + Delta)}),
  handle_shifted_frame(Frame#video_frame{dts = DTS + Delta, pts = PTS + Delta}, Media).

handle_shifted_frame(#video_frame{dts = DTS} = Frame, #ems_media{format = Format, storage = Storage} = Media) ->
  % ?D({Frame#video_frame.type, Frame#video_frame.frame_type, Frame#video_frame.dts}),
  start_on_keyframe(Frame, Media),
  Storage1 = save_frame(Format, Storage, Frame),
  handle_config(Frame, Media#ems_media{storage = Storage1, last_dts = DTS}).

handle_config(#video_frame{content = video, body = Config}, #ems_media{video_config = #video_frame{body = Config}} = Media) -> 
  {noreply, Media, ?TIMEOUT};

handle_config(#video_frame{content = audio, body = Config}, #ems_media{audio_config = #video_frame{body = Config}} = Media) -> 
  {noreply, Media, ?TIMEOUT};

handle_config(#video_frame{content = video, flavor = config} = Config, #ems_media{} = Media) -> 
  handle_frame(Config, Media#ems_media{video_config = Config});

handle_config(#video_frame{content = audio, flavor = config} = Config, #ems_media{} = Media) -> 
  handle_frame(Config, Media#ems_media{audio_config = Config});
  
handle_config(Frame, Media) ->
  handle_frame(Frame, Media).

handle_frame(#video_frame{content = Content} = Frame, #ems_media{module = M, clients = Clients} = Media) ->
  case M:handle_frame(Frame, Media) of
    {reply, F, Media1} ->
      case Content of
        audio -> send_frame(F, Clients, starting);
        _ -> ok
      end,
      send_frame(F, Clients, active),
      {noreply, Media1, ?TIMEOUT};
    {noreply, Media1} ->
      {noreply, Media1, ?TIMEOUT};
    {stop, Reason, Media1} ->
      {stop, Reason, Media1}
  end.


save_frame(undefined, Storage, _) ->
  Storage;

save_frame(Format, Storage, Frame) ->
  case Format:write_frame(Frame, Storage) of
    {ok, Storage1} -> Storage1;
    _ -> Storage
  end.

start_on_keyframe(#video_frame{content = video, flavor = keyframe, dts = DTS} = _F, #ems_media{clients = Clients, video_config = V} = M) ->
  MS = ets:fun2ms(fun(#client{state = starting, consumer = Client, stream_id = StreamId}) -> {Client,StreamId} end),
  Starting = ets:select(Clients, MS),
  [begin 
    (catch Client ! V#video_frame{dts = DTS, stream_id = StreamId}),
    ets:update_element(Clients, Client, {#client.state, active}) 
  end || {Client,StreamId} <- Starting],
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


metadata_frame(#ems_media{format = undefined} = M) ->
  #video_frame{content = metadata, body = [<<"onMetaData">>, {object, video_parameters(M)}]};
  % undefined;
  
metadata_frame(#ems_media{} = Media) ->
  Meta = lists:map(fun({K,V}) when is_atom(V) -> {K, atom_to_binary(V,latin1)};
                      (Else) -> Else end, storage_properties(Media)),
  #video_frame{content = metadata, body = [<<"onMetaData">>, {object, lists:ukeymerge(1, Meta, video_parameters(Media))}]}.



video_parameters(#ems_media{video_config = undefined}) ->
  [{duration,0}];
  
video_parameters(#ems_media{video_config = #video_frame{body = Config}}) ->
  [{duration,0}] ++ h264:metadata(Config).
  

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
terminate(normal, #ems_media{source = Source}) when Source =/= undefined ->
  ?D("ems_media exit normal"),
  erlang:exit(Source, shutdown),
  ok;

terminate(_Reason, _State) ->
  ?D({"ems_media exit", _Reason}),
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%
%  Tests
% 
-include_lib("eunit/include/eunit.hrl").

