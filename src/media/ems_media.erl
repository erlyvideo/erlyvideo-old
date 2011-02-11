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
%%%
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
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
-module(ems_media).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).
-include_lib("erlmedia/include/video_frame.hrl").
-include("../include/ems_media.hrl").
-include("ems_media_client.hrl").
-include("../log.hrl").

%% External API
-export([start_link/2, start_custom/2, stop_stream/1]).
-export([play/2, stop/1, resume/1, pause/1, seek/2, seek/3]).
-export([metadata/1, metadata/2, play_setup/2, seek_info/2, seek_info/3]).
-export([info/1, info/2, status/1]).
-export([subscribe/2, unsubscribe/1, set_source/2, set_socket/2, read_frame/2, read_frame/3, publish/2]).
-export([decoder_config/1, metadata_frame/1, metadata_frame/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %, format_status/2


-export([get/2, set/3, set/2]).


-define(LIFE_TIMEOUT, 60000).
-define(TIMEOUT, 120000).

-export([behaviour_info/1]).


-include("../meta_access.hrl").

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
  gen_server_ems:start_link(?MODULE, [Module, Options], [{fullsweep_after, 10}]).

%% @private
start_custom(Module, Options) ->
  gen_server_ems:start_link(Module, [Options], [{fullsweep_after, 10}]).




%%--------------------------------------------------------------------
%% @spec (Media::pid(), Options::list()) -> ok
%%
%% @doc Subscribe caller to stream and starts playing. Look {@link subscribe/2.} for options.
%% @end
%%----------------------------------------------------------------------
play(Media, Options) when is_pid(Media) andalso is_list(Options) ->
  ok = subscribe(Media, Options),
  gen_server:call(Media, {start, self()}),
  ok.

%%--------------------------------------------------------------------
%% @spec (Media::pid()) -> ok
%%
%% @doc The same as {@link unsubscribe/2.}
%% @end
%%----------------------------------------------------------------------
stop(Media) when is_pid(Media) ->
  unsubscribe(Media).

%%--------------------------------------------------------------------
%% @spec (Media::pid()) -> ok
%%
%% @doc stops stream. Called by source, when going to leave
%% @end
%%----------------------------------------------------------------------
stop_stream(Media) when is_pid(Media) ->
  gen_server:call(Media, stop).

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
subscribe(Media, Options) when is_pid(Media) andalso is_list(Options) ->
  gen_server:call(Media, {subscribe, self(), Options}, 10000).

%%----------------------------------------------------------------------
%% @spec (Media::pid()) -> ok
%%
%% @doc Unsubscribe caller from stream
%% @end
%%----------------------------------------------------------------------
unsubscribe(Media) when is_pid(Media) ->
  (catch gen_server:call(Media, {unsubscribe, self()})).

%%----------------------------------------------------------------------
%% @spec (Media::pid()) -> ok
%%
%% @doc Resume stream for calling client
%% @end
%%----------------------------------------------------------------------
resume(Media) when is_pid(Media) ->
  gen_server:call(Media, {resume, self()}).

%%----------------------------------------------------------------------
%% @spec (Media::pid()) -> ok
%%
%% @doc Pauses stream for calling client
%% @end
%%----------------------------------------------------------------------
pause(Media) when is_pid(Media) ->
  gen_server:call(Media, {pause, self()}).

%%----------------------------------------------------------------------
%% @spec (Media::pid(), Source::pid()) -> ok
%%
%% @doc Sets new source of frames for media. Media can work only with one source
%% @end
%%----------------------------------------------------------------------
set_source(Media, Source) when is_pid(Media) ->
  gen_server:cast(Media, {set_source, Source}).

%%----------------------------------------------------------------------
%% @spec (Media::pid(), Socket::port()) -> ok
%%
%% @doc Passes socket to media. Generic ems_media doesn't know anything about 
%% such sockets, but it will call ``Module:handle_control({set_socket, Socket}, State)'' on
%% submodule. For example, PUT mpegts requires it.
%% @end
%%----------------------------------------------------------------------
set_socket(Media, Socket) when is_pid(Media) ->
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
  gen_server:call(Media, {read_frame, self(), Key}, 10000).


%%----------------------------------------------------------------------
%% @spec (Media::pid(), Client::pid(), Key::any()) -> Frame::video_frame() |
%%                                     eof
%%
%% @doc The same as read_frame/2, but specify client for accounting meanings.
%%
%% @end
%%----------------------------------------------------------------------
read_frame(Media, Client, Key) ->
  gen_server:call(Media, {read_frame, Client, Key}, 10000).

%%----------------------------------------------------------------------
%% @spec (Media::pid(), DTS::number()) -> ok | {error, Reason}
%%
%% @doc Seek in storage. Looks either keyframe before DTS or keyframe after DTS.
%% Seeks private caller stream and starts sending frames from NewDTS.
%% @end
%%----------------------------------------------------------------------
seek(Media, DTS) ->
  gen_server:call(Media, {seek, self(), DTS}, 5000).

seek(Media, BeforeAfter, DTS) when BeforeAfter == before orelse BeforeAfter == 'after' ->
  seek(Media, DTS).

%%----------------------------------------------------------------------
%% @spec (Media::pid(), DTS::number(), Options::proplist()) -> {Key::any(), NewDTS::number()} |
%%                                                                   undefined
%%
%% @doc Seek in storage. Looks either keyframe before DTS or keyframe after DTS.
%% Returns Key for this keyframe and its NewDTS. Takes options to determine which track to choose
%% @end
%%----------------------------------------------------------------------
seek_info(Media, BeforeAfter, DTS) when BeforeAfter == before orelse BeforeAfter == 'after' ->
  seek_info(Media, DTS, []);

seek_info(Media, DTS, Options) when is_list(Options) ->
  gen_server:call(Media, {seek_info, DTS, Options}).


seek_info(Media, DTS) when is_number(DTS) ->
  seek_info(Media, DTS, []).


%%----------------------------------------------------------------------
%% @spec (Media::pid()) -> Status::list()
%%  
%% @end
%%----------------------------------------------------------------------
status(Media) ->
  info(Media).

%%----------------------------------------------------------------------
%% @spec (Media::pid()) -> Status::list()
%%  
%% @end
%%----------------------------------------------------------------------
info(Media) ->
  info(Media, [client_count, url, type, storage, last_dts]).
  
%%----------------------------------------------------------------------
%% @spec (Media::pid(), Properties::list()) -> Info::list()
%%
%% @doc Returns miscelaneous info about media, such as client_count
%% Here goes list of known properties
%%
%% client_count
%% url
%% type
%% storage
%% clients
%% @end
%%----------------------------------------------------------------------
info(Media, Properties) ->
  properties_are_valid(Properties) orelse erlang:error({badarg, Properties}),
  gen_server:call(Media, {info, Properties}).
  
known_properties() ->
  [client_count, url, type, storage, clients, last_dts].
  
properties_are_valid(Properties) ->
  lists:subtract(Properties, known_properties()) == [].
  
  
%%----------------------------------------------------------------------
%% @spec (Media::pid()) -> Metadata::video_frame()
%%
%% @doc Returns video_frame, prepared to send into flash
%% @end
%%----------------------------------------------------------------------
metadata(Media) when is_pid(Media) ->
  metadata(Media, []).

%%----------------------------------------------------------------------
%% @spec (Media::pid(), Options::proplist()) -> Metadata::video_frame()
%%
%% @doc Returns video_frame, prepared to send into flash
%% @end
%%----------------------------------------------------------------------
metadata(Media, Options) when is_pid(Media) ->
  gen_server:call(Media, {metadata, Options}).  


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
play_setup(Media, Options) when is_pid(Media) andalso is_list(Options) ->
  gen_server:cast(Media, {play_setup, self(), Options}).
  
  
%%----------------------------------------------------------------------
%% @spec (Media::pid(), Frame::video_frame()) -> any()
%%
%% @doc Publishes frame to media
%% @end
%%----------------------------------------------------------------------
publish(Media, #video_frame{} = Frame) when is_pid(Media) ->
  Media ! Frame.


%%----------------------------------------------------------------------
%% @spec (Media::pid()) -> proplist()
%%
%% @doc Returns decoder config, extracted from media. Important: it can block for some time
%% or return undefined, when there may be config.
%% @end
%%----------------------------------------------------------------------
decoder_config(Media) when is_pid(Media) ->
  Timeout = 8000,
  gen_server:call(Media, decoder_config, Timeout).


  
%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%% @hidden
% format_status(_Reason, [_Dict, #ems_media{} = Media]) ->
%   Media#ems_media{storage = storage}.


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
  % ?D({init,Module,Options}),
  Name = proplists:get_value(name, Options),
  URL_ = proplists:get_value(url, Options),
  URL = if 
    is_list(URL_) -> list_to_binary(URL_);
    is_binary(URL_) -> URL_
  end,
  Media = #ems_media{options = Options, module = Module, name = Name, url = URL, type = proplists:get_value(type, Options),
                     clients = ems_media_clients:init(), host = proplists:get_value(host, Options)},
                     
  timer:send_interval(30000, garbage_collect),
  case Module:init(Media, Options) of
    {ok, Media1} ->
      Media2 = init_timeshift(Media1, Options),
      Media3 = init_timeouts(Media2, Options),
      Media4 = init_transcoder(Media3, Options),
      ?D({"Started", Module,Options}),
      {ok, Media4, ?TIMEOUT};
    {stop, Reason} ->
      ?D({"ems_media failed to initialize",Module,Reason}),
      {stop, Reason}
  end.


init_timeshift(#ems_media{format = Format} = Media, Options) ->
  case proplists:get_value(timeshift, Options) of
    undefined -> 
      Media;
    Timeshift when is_number(Timeshift) andalso Timeshift > 0 andalso Format =/= undefined ->
      erlang:error({initialized_timeshift_and_storage, Format, Timeshift});
    Timeshift when is_number(Timeshift) andalso Timeshift > 0 ->
      case array_timeshift:init(Options, []) of
        {ok, TSData} ->
          Media#ems_media{format = array_timeshift, storage = TSData};
        _ ->
          Media
      end
  end.
  
init_timeouts(Media, Options) ->
  Media#ems_media{
    source_timeout = or_time(proplists:get_value(source_timeout, Options), Media#ems_media.source_timeout),
    clients_timeout = or_time(proplists:get_value(clients_timeout, Options), Media#ems_media.clients_timeout),
    retry_limit = or_time(proplists:get_value(retry_limit, Options), Media#ems_media.retry_limit)
  }.
  
  
init_transcoder(Media, Options) ->
  case proplists:get_value(transcoder, Options) of
    undefined ->
      Media;
    {Transcoder, TransArgs} ->
      {ok, TransState} = erlang:apply(Transcoder, init, TransArgs),
      Media#ems_media{transcoder = Transcoder, trans_state = TransState}
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


handle_call({subscribe, Client, Options}, _From, #ems_media{module = M, clients = Clients, audio_config = A, last_dts = DTS} = Media) ->
  StreamId = proplists:get_value(stream_id, Options),

  DefaultSubscribe = fun(Reply, #ems_media{} = Media1) ->
    Ref = erlang:monitor(process,Client),
    RequireTicker = case Reply of
      tick -> true;
      _ -> proplists:get_value(start, Options) =/= undefined
    end,
    Clients1 = case RequireTicker of
      true ->
        {ok, Ticker} = ems_sup:start_ticker(self(), Client, Options),
        TickerRef = erlang:monitor(process,Ticker),
        Entry = #client{consumer = Client, stream_id = StreamId, ref = Ref, ticker = Ticker, ticker_ref = TickerRef, state = passive},
        ems_media_clients:insert(Clients, Entry);
      false ->
        %
        % It is very important to understand, that we need to send audio config here, because client starts receiving music
        % right after subscribing, but it will wait for video till keyframe
        %
        (catch Client ! A#video_frame{dts = DTS, pts = DTS, stream_id = StreamId}),
        ClientState = case proplists:get_value(paused, Options, false) of
          true -> paused;
          false -> starting
        end,
        ems_media_clients:insert(Clients, #client{consumer = Client, stream_id = StreamId, ref = Ref, state = ClientState})
    end,
    {reply, ok, Media1#ems_media{clients = Clients1}, ?TIMEOUT}
  end,
  
  case M:handle_control({subscribe, Client, Options}, Media) of
    {stop, Reason, Media1} ->
      ?D({"ems_media failed to subscribe",Client,M,Reason}),
      {stop, Reason, Media1};
    {stop, Reason, Reply, Media1} ->
      {stop, Reason, Reply, Media1};
    {reply, {error, Reason}, Media1} ->
      {reply, {error, Reason}, Media1, ?TIMEOUT};
    {reply, Reply, Media1} ->
      DefaultSubscribe(Reply, Media1);
    {noreply, Media1} ->
      DefaultSubscribe(ok, Media1)
  end;

handle_call(stop, _From, Media) ->
  {stop, normal, Media};

  
handle_call({stop, Client}, _From, Media) ->
  unsubscribe_client(Client, Media);
  
handle_call({unsubscribe, Client}, _From, Media) ->
  unsubscribe_client(Client, Media);


handle_call({start, Client}, From, Media) ->
  handle_call({resume, Client}, From, Media);

handle_call(decoder_config, From, #ems_media{video_config = undefined, audio_config = undefined, storage = undefined,
            frame_number = Number, waiting_for_config = Waiting} = Media) when Number < ?WAIT_FOR_CONFIG ->
  ?D({"No decoder config in live stream, waiting"}),
  {noreply, Media#ems_media{waiting_for_config = [From|Waiting]}, ?TIMEOUT};

handle_call(decoder_config, _From, #ems_media{video_config = undefined, audio_config = undefined, 
            storage = Storage} = Media) when Storage =/= undefined ->
  Media1 = try_find_config(Media),
  {reply, {ok, [{audio,Media1#ems_media.audio_config},{video,Media1#ems_media.video_config}]}, Media1, ?TIMEOUT};

handle_call(decoder_config, _From, #ems_media{video_config = V, audio_config = A} = Media) ->
  {reply, {ok, [{audio,A},{video,V}]}, Media, ?TIMEOUT};
  
handle_call({resume, Client}, _From, #ems_media{clients = Clients} = Media) ->
  case ems_media_clients:find(Clients, Client) of
    #client{state = passive, ticker = Ticker} ->
      media_ticker:resume(Ticker),
      {reply, ok, Media, ?TIMEOUT};

    #client{state = paused} ->
      Clients1 = ems_media_clients:update_state(Clients, Client, starting),
      {reply, ok, Media#ems_media{clients = Clients1}, ?TIMEOUT};
      
    #client{} ->
      {reply, ok, Media, ?TIMEOUT};

    undefined ->
      {reply, {error, no_client}, Media, ?TIMEOUT}
  end;
      
handle_call({pause, Client}, _From, #ems_media{clients = Clients} = Media) ->
  case ems_media_clients:find(Clients, Client) of
    #client{state = passive, ticker = Ticker} ->
      ?D({"Pausing passive client", Client}),
      media_ticker:pause(Ticker),
      {reply, ok, Media, ?TIMEOUT};
    
    #client{} ->
      ?D({"Pausing active client", Client}),
      Clients1 = ems_media_clients:update_state(Clients, Client, paused),
      {reply, ok, Media#ems_media{clients = Clients1}, ?TIMEOUT};
      
    undefined ->
      ?D({"No client to pause", Client}),
      {reply, {error, no_client}, Media, ?TIMEOUT}
  end;

handle_call({seek, _Client, _DTS} = Seek, _From, #ems_media{} = Media) ->
  handle_seek(Seek, Media);
      

%% It is information seek, required for outside needs.
handle_call({seek_info, DTS, Options} = SeekInfo, _From, #ems_media{format = Format, storage = Storage, module = M} = Media) ->
  case M:handle_control(SeekInfo, Media) of
    {noreply, Media1} when Format == undefined ->
      {reply, undefined, Media1, ?TIMEOUT};
    {noreply, Media1} ->
      {reply, Format:seek(Storage, DTS, Options), Media1, ?TIMEOUT};
    {stop, Reason, Media1} ->
      {stop, Reason, Media1};
    {stop, Reason, Reply, Media1} ->
      {stop, Reason, Reply, Media1};
    {reply, Reply, Media1} ->
      {reply, Reply, Media1, ?TIMEOUT}
  end;

handle_call({read_frame, Client, Key}, _From, #ems_media{format = Format, storage = Storage, clients = Clients} = Media) ->
  {Storage1, Frame} = case Format:read_frame(Storage, Key) of
    #video_frame{} = F -> {Storage, F};
    {S, #video_frame{} = F} -> {S, F};
    Else -> {Storage, Else}
  end,
  Media1 = case Frame of
    #video_frame{content = video, flavor = config} -> Media#ems_media{video_config = Frame};
    #video_frame{content = audio, flavor = config} -> Media#ems_media{audio_config = Frame};
    #video_frame{content = C, body = Body} when C == audio orelse C == video ->
      Bytes = try erlang:iolist_size(Body) of
        Size -> Size
      catch
        _:_ -> 0
      end,  
      Clients1 = ems_media_clients:increment_bytes(Clients, Client, Bytes),
      Media#ems_media{clients = Clients1};
    _ -> Media
  end,
  {reply, Frame, Media1#ems_media{storage = Storage1}, ?TIMEOUT};

handle_call({metadata, Options}, _From, #ems_media{} = Media) ->
  {reply, metadata_frame(Media, Options), Media, ?TIMEOUT};

handle_call({info, Properties}, _From, Media) ->
  {reply, reply_with_info(Media, Properties), Media, ?TIMEOUT};

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

handle_cast({play_setup, Client, Options}, #ems_media{clients = Clients} = Media) ->
  case ems_media_clients:find(Clients, Client) of
    #client{state = passive, ticker = Ticker} ->
      ?D({"Setup play options for passive client", Client, Options}),
      media_ticker:play_setup(Ticker, Options),
      {noreply, Media, ?TIMEOUT};

    #client{} ->
      %?D({"Play options for active clients are not supported not", Client, Options}),
      {noreply, Media, ?TIMEOUT};

    undefined ->
      ?D({"Unknown client asked to change his play options", Client, Options}),
      {noreply, Media, ?TIMEOUT}
  end;


handle_cast(Cast, #ems_media{module = M} = Media) ->
  case M:handle_control(Cast, Media) of
    {noreply, Media1} ->
      {noreply, Media1, ?TIMEOUT};
    {reply, _Reply, Media1} ->
      {noreply, Media1, ?TIMEOUT};
    {stop, Reason, _Reply, Media1} ->
      {stop, Reason, Media1};
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

handle_info({'DOWN', _Ref, process, Source, _Reason}, #ems_media{source = Source, source_timeout = shutdown} = Media) ->
  ?D({"ems_media lost source with source_timeout=shutdown", Source, _Reason}),
  {stop, normal, Media};

handle_info({'DOWN', _Ref, process, Source, _Reason}, #ems_media{module = M, source = Source, source_timeout = SourceTimeout} = Media) ->
  ?D({"ems_media lost source", Source, _Reason}),
  ems_event:stream_source_lost(proplists:get_value(host,Media#ems_media.options), Media#ems_media.name, self()),
  case M:handle_control({source_lost, Source}, Media#ems_media{source = undefined}) of
    {stop, Reason, Media1} ->
      ?D({"ems_media is stopping due to source_lost", M, Source, Reason}),
      {stop, Reason, Media1};
    {stop, Reason, _Reply, Media1} ->
      {stop, Reason, Media1};
    {noreply, Media1} when is_number(SourceTimeout) andalso SourceTimeout > 0 ->
      ?D({"ems_media lost source and sending graceful", SourceTimeout, round(Media1#ems_media.last_dts)}),
      {ok, Ref} = timer:send_after(SourceTimeout, no_source),
      {noreply, Media1#ems_media{source_ref = undefined, source_timeout_ref = Ref}, ?TIMEOUT};
    {noreply, Media1} when SourceTimeout == 0 ->
      {stop, normal, Media1};
    {noreply, Media1} when SourceTimeout == false ->
      ?D({"ems_media lost source but source_timeout = false"}),
      {noreply, Media1#ems_media{source_ref = undefined}, ?TIMEOUT};
    {reply, NewSource, Media1} ->
      ?D({"ems_media lost source and sending graceful, but have new source", SourceTimeout, NewSource}),
      Ref = erlang:monitor(process, NewSource),
      {noreply, Media1#ems_media{source = NewSource, source_ref = Ref, ts_delta = undefined}, ?TIMEOUT}
  end;

  
handle_info({'DOWN', _Ref, process, Pid, ClientReason} = Msg, #ems_media{clients = Clients, module = M} = Media) ->
  case unsubscribe_client(Pid, Media) of
    {reply, {error, no_client}, Media2, _} ->
      case ems_media_clients:find_by_ticker(Clients, Pid)  of
        #client{consumer = Client, stream_id = StreamId} ->
          case ClientReason of 
            normal -> ok;
            _ -> Client ! {ems_stream, StreamId, play_failed}
          end,
          case unsubscribe_client(Client, Media2) of
            {reply, _Reply, Media3, _} -> {noreply, Media3, ?TIMEOUT};
            {stop, Reason, _Reply, Media3} ->
              ?D({"ems_media is stopping after unsubscribe", M, Client, Reason}),
              {stop, Reason, Media3};
            {stop, Reason, Media3} -> 
              ?D({"ems_media is stopping after unsubscribe", M, Client, Reason}),
              {stop, Reason, Media3}
          end;
        undefined -> 
          case M:handle_info(Msg, Media2) of
            {noreply, Media3} -> {noreply, Media3, ?TIMEOUT};
            {stop, Reason, Media3} -> 
              ?D({"ems_media is stopping after handling info", M, Msg, Reason}),
              {stop, normal, Media3}
          end
      end;
    {reply, _Reply, Media2, _} -> {noreply, Media2, ?TIMEOUT};
    {stop, Reason, _Reply, Media1} ->
      {stop, Reason, Media1};
    {stop, Reason, Media2} -> 
      ?D({"ems_media is stopping after unsubscribe", M, Pid, Reason}),
      {stop, Reason, Media2}
  end;

handle_info(#video_frame{} = Frame, Media) ->
  % ?D({Frame#video_frame.codec, Frame#video_frame.flavor, Frame#video_frame.dts}),
  {Media1, Frames} = case ems_media_frame:transcode(Frame, Media) of
    {Media1_, undefined} -> {Media1_, []};
    {Media1_, Frames1} when is_list(Frames1) -> {Media1_, Frames1};
    {Media1_, Frame1} -> {Media1_, [Frame1]}
  end,
  lists:foldl(fun
    (_, {stop,_,_} = Stop) -> 
      Stop;
    (OutFrame, {noreply, State, _}) ->
      ems_media_frame:send_frame(OutFrame, State)
  end, {noreply, Media1, ?TIMEOUT}, Frames);

handle_info(no_source, #ems_media{source = undefined, module = M} = Media) ->
  case M:handle_control(no_source, Media) of
    {noreply, Media1} ->
      ?D({"Media has no source after timeout and exiting", self(), Media#ems_media.name}),
      {stop, normal, Media1};
    {stop, Reason, _Reply, Media1} ->
      {stop, Reason, Media1};
    {stop, Reason, Media1} ->
      ?D({"Media has no source after timeout and exiting", self(), Media#ems_media.name}),
      {stop, Reason, Media1};
    {reply, NewSource, Media1} ->
      Ref = erlang:monitor(process, NewSource),
      Media2 = mark_clients_as_starting(Media1),
      {noreply, Media2#ems_media{source = NewSource, source_timeout_ref = undefined, source_ref = Ref, ts_delta = undefined}, ?TIMEOUT}
  end;


handle_info(no_clients, #ems_media{module = M} = Media) ->
  case client_count(Media) of
    0 ->
      % ?D({"graceful received, handling", self(), Media#ems_media.name}),
      case M:handle_control(no_clients, Media) of
        {noreply, Media1} ->
          ?D({"ems_media is stopping", M, Media#ems_media.name}),
          {stop, normal, Media1};
        {stop, Reason, _Reply, Media1} ->
          {stop, Reason, Media1};
        {stop, Reason, Media1} ->
          case Reason of
            normal -> ok;
            _ -> ?D({"ems_media is stopping after graceful", M, Reason, Media#ems_media.name})
          end,
          {stop, Reason, Media1};
        {reply, ok, Media1} ->
          ?D({M, "rejected stopping of ems_media due to 0 clients", self(), Media#ems_media.name}),
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
    {stop, Reason, _Reply, Media1} ->
      {stop, Reason, Media1};
    {noreply, Media1} ->
      {noreply, Media1, ?TIMEOUT};
    {reply, _Reply, Media1} ->
      {noreply, Media1, ?TIMEOUT}
  end;

handle_info(timeout, #ems_media{source = undefined} = Media) ->
  {noreply, Media};


handle_info(garbage_collect, Media) ->
  garbage_collect(self()),
  {noreply, Media};

handle_info(Message, #ems_media{module = M} = Media) ->
  case M:handle_info(Message, Media) of
    {noreply, Media1} ->
      {noreply, Media1, ?TIMEOUT};
    {stop, Reason, _Reply, Media1} ->
      {stop, Reason, Media1};
    {stop, Reason, Media1} ->
      ?D({"ems_media is stopping after handle_info", M, Message, Reason}),
      {stop, Reason, Media1}
  end.


try_find_config(#ems_media{audio_config = undefined, video_config = undefined, format = undefined} = Media) ->
  Media;

try_find_config(#ems_media{audio_config = undefined, video_config = undefined, format = Format} = Media) 
  when Format =/= undefined ->

  try_n_frames(Media, 10, undefined).


try_n_frames(#ems_media{audio_config = A, video_config = V} = Media, _, _) 
  when A =/= undefined andalso V =/= undefined ->
  Media;

try_n_frames(Media, _, eof) ->
  Media;
  
try_n_frames(Media, 0, _) -> 
  Media;

try_n_frames(#ems_media{format = Format, storage = Storage} = Media, N, Key) ->
  Frame = Format:read_frame(Storage, Key),
  case Frame of
    #video_frame{content = video, flavor = config, next_id = Next} -> 
      try_n_frames(Media#ems_media{video_config = Frame}, N-1, Next);
    #video_frame{content = audio, flavor = config, next_id = Next} -> 
      try_n_frames(Media#ems_media{audio_config = Frame}, N-1, Next);
    #video_frame{content = audio, codec = Codec, next_id = Next} when Codec =/= aac -> 
    % none is not undefined. none means, that this stream doesn't have any config
      try_n_frames(Media#ems_media{audio_config = none}, N-1, Next);
    #video_frame{content = video, codec = Codec, next_id = Next} when Codec =/= h264 -> 
      try_n_frames(Media#ems_media{video_config = none}, N-1, Next);
    #video_frame{next_id = Next} -> 
      try_n_frames(Media, N+1, Next);
    eof ->
      Media
  end.




handle_seek({seek, Client, DTS} = Seek, #ems_media{module = M} = Media) ->
  ?D({"Going to seek", Seek}),
  case M:handle_control(Seek, Media) of
    {noreply, Media1} ->
      ?D({"default seek", Seek}),
      default_ems_media_seek(Seek, Media1);
    {stop, Reason, Media1} ->
      {stop, Reason, Media1};
    {reply, {NewKey, NewDTS}, Media1} ->
      default_seek_reply(Client, {DTS, NewKey, NewDTS}, Media1);
    {reply, Reply, Media1} ->
      default_seek_reply(Client, Reply, Media1)
  end.


default_ems_media_seek(_, #ems_media{format = undefined} = Media) ->
  ?D("no format"),
  {reply, seek_failed, Media, ?TIMEOUT};
  
default_ems_media_seek({seek, Client, DTS}, #ems_media{format = Format, storage = Storage} = Media) ->
  ?D({"default seek", Client, DTS}),
  case Format:seek(Storage, DTS, []) of
    {NewPos, NewDTS} ->
      ?D({"seek ready", NewPos, NewDTS}),
      default_seek_reply(Client, {DTS, NewPos, NewDTS}, Media);
    undefined ->
      ?D({"no flv seek"}),
      {reply, seek_failed, Media, ?TIMEOUT}
  end.


default_seek_reply(Client, {DTS, NewPos, NewDTS}, #ems_media{clients = Clients} = Media) ->
  ?D({dst,DTS,NewPos,NewDTS}),
  case ems_media_clients:find(Clients, Client) of
    #client{ticker = Ticker, state = passive} ->
      media_ticker:seek(Ticker, DTS),
      {reply, {seek_success, NewDTS}, Media, ?TIMEOUT};

    #client{stream_id = StreamId} = Entry ->
      {ok, Ticker} = ems_sup:start_ticker(self(), Client, [{stream_id, StreamId}]),
      TickerRef = erlang:monitor(process,Ticker),
      media_ticker:seek(Ticker, NewDTS),
      Clients1 = ems_media_clients:update(Clients, Client, Entry#client{ticker = Ticker, ticker_ref = TickerRef, state = passive}),
      {reply, {seek_success, NewDTS}, Media#ems_media{clients = Clients1}, ?TIMEOUT};

    _ ->
      ?D("Client requested seek not in list"),
      {reply, seek_failed, Media, ?TIMEOUT}
  end.



unsubscribe_client(Client, #ems_media{options = Options, clients = Clients, module = M} = Media) ->
  case ems_media_clients:find(Clients, Client) of
    #client{ref = Ref, ticker = Ticker, ticker_ref = TickerRef} ->
      case M:handle_control({unsubscribe, Client}, Media) of
        {noreply, Media1} ->
          case TickerRef of
            undefined -> ok;
            _ ->
              erlang:demonitor(TickerRef, [flush]),
              (catch media_ticker:stop(Ticker))
          end,
        
          erlang:demonitor(Ref, [flush]),
          Entry = ems_media_clients:find(Clients, Client),
          Host = proplists:get_value(host, Options),
          Stats = [{bytes_sent, Entry#client.bytes}],
          Clients1 = ems_media_clients:delete(Clients, Client),

          ems_event:user_stop(Host, Client, self(), Stats),

          {reply, ok, check_no_clients(Media1#ems_media{clients = Clients1}), ?TIMEOUT};
        {reply, Reply, Media1} ->
          {reply, Reply, Media1, ?TIMEOUT};

        {stop, Reason, Reply, Media1} ->
          {stop, Reason, Reply, Media1};

        {stop, Reason, Media1} ->
          {stop, Reason, Media1}
      end;    

    undefined ->
      {reply, {error, no_client}, check_no_clients(Media), ?TIMEOUT}
  end.


check_no_clients(#ems_media{clients_timeout = Timeout} = Media) ->
  Count = client_count(Media),
  {ok, TimeoutRef} = case Count of
    0 when is_number(Timeout) -> 
      % ?D({"No clients, sending delayed graceful", Timeout, self(), Media#ems_media.name}), 
      timer:send_after(Timeout, no_clients);
    _ -> 
      {ok, undefined}
  end,
  Media#ems_media{clients_timeout_ref = TimeoutRef}.

mark_clients_as_starting(#ems_media{clients = Clients} = Media) ->
  Clients1 = ems_media_clients:mass_update_state(Clients, active, starting),
  Media#ems_media{clients = Clients1}.


client_count(#ems_media{clients = Clients}) ->
  ems_media_clients:count(Clients).


storage_properties(Media) ->
  storage_properties(Media, []).

storage_properties(#ems_media{format = undefined}, Options) ->
  Options;

storage_properties(#ems_media{format = Format, storage = Storage}, Options) ->
  Props = lists:ukeymerge(1, lists:ukeysort(1,Options), lists:ukeysort(1,Format:properties(Storage))),
  case proplists:get_value(duration, Props) of
    undefined -> Props;
    Duration -> lists:ukeymerge(1, [{duration,Duration/1000},{length,Duration}], lists:ukeysort(1,Props))
  end.

metadata_frame(#ems_media{} = Media) ->
  metadata_frame(Media, []).

metadata_frame(#ems_media{format = undefined} = M, Options) ->
  #video_frame{content = metadata, body = [<<"onMetaData">>, {object, video_parameters(M, Options)}]};
  % undefined;
  
metadata_frame(#ems_media{} = Media, Options) ->
  Meta = lists:map(fun({K,V}) when is_atom(V) -> {K, atom_to_binary(V,latin1)};
                      (Else) -> Else end, storage_properties(Media, Options)),
  #video_frame{content = metadata, body = [<<"onMetaData">>, {object, lists:ukeymerge(1, lists:keysort(1,Meta), video_parameters(Media, Options))}]}.



video_parameters(#ems_media{video_config = #video_frame{body = Config}}, Options) ->
  lists:ukeymerge(1, [{duration,proplists:get_value(duration, Options, 0)}], lists:keysort(1, h264:metadata(Config)));
  
video_parameters(#ems_media{}, Options) ->  
  [{duration,proplists:get_value(duration, Options, 0)}].




reply_with_info(#ems_media{type = Type, url = URL, last_dts = LastDTS} = Media, Properties) ->
  lists:foldl(fun
    (type, Props) -> [{type,Type}|Props];
    (url, Props) -> [{url,URL}|Props];
    (last_dts, Props) -> [{last_dts,LastDTS}|Props];
    (client_count, Props) -> [{client_count,client_count(Media)}|Props];
    (storage, Props) -> storage_properties(Media) ++ Props;
    (clients, Props) -> [{clients,ems_media_clients:list(Media#ems_media.clients)}|Props]
  end, [], Properties).



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

terminate(normal, #ems_media{source = undefined}) ->
  ok;

terminate(_Reason, Media) ->
  ?D({"ems_media exit", self(), Media#ems_media.name}),
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

