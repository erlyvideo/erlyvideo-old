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
-include_lib("erlmedia/include/media_info.hrl").
-include("ems_media.hrl").
-include("ems_media_client.hrl").
-include("../log.hrl").

%% External API
-export([start_link/2, start_custom/2, stop_stream/1]).
-export([play/2, stop/1, resume/1, pause/1, seek/2, seek/3]).
-export([play_setup/2, seek_info/2, seek_info/3]).
-export([subscribe/2, unsubscribe/1, set_source/2, set_socket/2, read_frame/2, read_frame/3, publish/2]).


-export([media_info/1, set_media_info/2]).
-export([status/1, info/1, info/2, full_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %, format_status/2


-export([source_is_lost/1, source_is_restored/1]).


-define(LIFE_TIMEOUT, 60000).
-define(TIMEOUT, 120000).
-define(TIMEOUT_RESTART, 1000).

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
  _Info = media_info(Media), %% It is very important, because no client should be without media_info
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
set_socket(Media, Socket) when is_pid(Media) andalso is_port(Socket) ->
  ok = gen_tcp:controlling_process(Socket, Media),
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
%% @spec (Media::pid()) -> Status::list()
%%  
%% @end
%%----------------------------------------------------------------------
media_info(Media) ->
  gen_server:call(Media, media_info, 120000).



set_media_info(Media, #media_info{} = Info) when is_pid(Media) ->
  gen_server:call(Media, {set_media_info, Info});

set_media_info(#ems_media{media_info = Info} = Media, Info) ->
  Media;
  
set_media_info(#ems_media{waiting_for_config = Waiting, options = Options} = Media, #media_info{audio = A, video = V} = Info) ->
  Info1 = case proplists:get_value(duration, Options) of
    undefined -> Info;
    Duration -> Info#media_info{duration = Duration}
  end,
  case A == wait orelse V == wait of
    true ->
      Media#ems_media{media_info = Info1};
    false ->
      Reply = reply_with_media_info(Media, Info1),
      [gen_server:reply(From, Reply) || From <- Waiting],
      Media#ems_media{media_info = Info1, waiting_for_config = []}
  end.

%%----------------------------------------------------------------------
%% @spec (Media::pid()) -> Status::list()
%%  
%% @end
%%----------------------------------------------------------------------
status(Media) ->
  info(Media).

%%----------------------------------------------------------------------
%% @spec (Media::pid()) -> Status::list()
%% @doc Returns brief and fast-to-retrieve info about media
%% @end
%%----------------------------------------------------------------------
info(Media) ->
  info(Media, [client_count, url, type, storage, last_dts, ts_delay, options]).
  
%%----------------------------------------------------------------------
%% @spec (Media::pid(), Properties::list()) -> Info::list()
%%
%% @doc Returns specified info about media, such as client_count
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
  ValidProperties = clean_properties(Properties),
  gen_server:call(Media, {info, ValidProperties}).

%%----------------------------------------------------------------------
%% @spec (Media::pid(), Properties::list()) -> Info::list()
%%
%% @doc Returns full info about media
%% @end
%%----------------------------------------------------------------------
full_info(Media) ->
  info(Media, known_properties()).
  
known_properties() ->
  [client_count, url, type, storage, clients, last_dts, ts_delay, created_at, options, hls_playlist].
  
clean_properties(Properties) ->
  clean_properties(Properties, []).

clean_properties([], Acc) -> lists:reverse(Acc);
clean_properties([{hls_segment, N}|Props], Acc) when is_integer(N) -> clean_properties(Props, [{hls_segment, N}|Acc]);
clean_properties([Property|Props], Acc) ->
  case lists:member(Property, known_properties()) of
    true -> clean_properties(Props, [Property|Acc]);
    false -> clean_properties(Props, Acc)
  end.
  
  




  
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
    is_binary(URL_) -> URL_;
    is_atom(URL_) -> atom_to_binary(URL_, latin1);
    true -> iolist_to_binary(io_lib:format("~p", [URL_]))
  end,
  Media = #ems_media{options = Options, module = Module, name = Name, url = URL, type = proplists:get_value(type, Options),
                     clients = ems_media_clients:init(Options), host = proplists:get_value(host, Options),
                     media_info = proplists:get_value(media_info, Options, #media_info{flow_type = stream}),
                     glue_delta = proplists:get_value(glue_delta, Options, ?DEFAULT_GLUE_DELTA),
                     sort_count = proplists:get_value(sort_count, Options, 10),
                     created_at = ems:now(utc), last_dts_at = os:timestamp()},
  
  Media_ = ems_media_frame:init(Media),                   
  timer:send_interval(30000, garbage_collect),
  timer:send_after(5000, stop_wait_for_config),
  case Module:init(Media_, Options) of
    {ok, Media1} ->
      Media2 = init_timeshift(Media1, Options),
      Media3 = init_timeouts(Media2, Options),
      Media4 = init_transcoder(Media3, Options),
      ?D({"Started", Module, URL}),
      {ok, Media4, ?TIMEOUT};
    {stop, Reason} ->
      ?D({"ems_media failed to initialize",Module,Reason}),
      ExitReason = case Reason of
        normal -> normal;
        notfound -> normal;
        _ -> Reason
      end,
      {stop, ExitReason}
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
handle_call({subscribe, _, _} = Call, From, Media) ->
  ems_media_client_control:handle_call(Call, From, Media);

handle_call({stop, _} = Call, From, Media) ->
  ems_media_client_control:handle_call(Call, From, Media);

handle_call({unsubscribe, _} = Call, From, Media) ->
  ems_media_client_control:handle_call(Call, From, Media);

handle_call({start, _} = Call, From, Media) ->
  ems_media_client_control:handle_call(Call, From, Media);

handle_call({resume, _} = Call, From, Media) ->
  ems_media_client_control:handle_call(Call, From, Media);

handle_call({pause, _} = Call, From, Media) ->
  ems_media_client_control:handle_call(Call, From, Media);

handle_call({seek, _, _} = Call, From, Media) ->
  ems_media_client_control:handle_call(Call, From, Media);


handle_call(stop, _From, Media) ->
  {stop, normal, Media};

  
handle_call({get_field, Key}, _From, Media) ->
  {reply, get(Media, Key), Media};

handle_call({set_field, Key, Value}, _From, Media) ->
  {reply, ok, set(Media, Key, Value)};


handle_call(media_info, _From, #ems_media{media_info = #media_info{audio = A, video = V} = Info} = Media) when A =/= wait andalso V =/= wait ->
  {reply, reply_with_media_info(Media, Info), Media, ?TIMEOUT};


handle_call(media_info, From, #ems_media{waiting_for_config = Waiting} = Media) ->
  ?D({"No decoder config in live stream, waiting"}),
  {noreply, Media#ems_media{waiting_for_config = [From|Waiting]}, ?TIMEOUT};

handle_call({set_media_info, Info}, _From, #ems_media{} = Media) ->
  {reply, ok, set_media_info(Media, Info), ?TIMEOUT};
  

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

handle_call({info, Properties}, _From, #ems_media{hls_state = HLS} = Media) ->
  Media1 = case HLS of
    undefined ->
      HLSRequested = lists:member(hls_playlist, Properties) or (length([1 || {hls_segment, _} <- Properties]) > 0),
      case HLSRequested of
        true -> Media#ems_media{hls_state = hls_media:init(Media)};
        false -> Media
      end;
    _ -> Media
  end,  
  {reply, reply_with_info(Media1, Properties), Media1, ?TIMEOUT};

handle_call(Request, _From, #ems_media{module = M} = Media) ->
  case M:handle_control(Request, Media) of
    {noreply, Media1} ->
      {reply, {error, {unknown_call, Request}}, Media1, ?TIMEOUT};
    {reply, Reply, Media1} ->
      {reply, Reply, Media1, ?TIMEOUT};
    {stop, Reason, Reply, Media1} ->
      {stop, Reason, Reply, Media1};
    {stop, Reason, Media1} ->
      {stop, Reason, Media1}
  end.


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
      % ?D({"Setup play options for passive client", Client, Options}),
      media_ticker:play_setup(Ticker, Options),
      {noreply, Media, ?TIMEOUT};

    #client{} ->
      %?D({"Play options for active clients are not supported not", Client, Options}),
      {noreply, Media, ?TIMEOUT};

    undefined ->
      ?D({"Unknown client asked to change his play options", Client, Options}),
      {noreply, Media, ?TIMEOUT}
  end;

handle_cast({set_frame_filters, Filters}, Media) ->
  {noreply, Media#ems_media{frame_filters = Filters}};

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
handle_info({inet_reply,_Socket,_Reply}, Media) ->
  {noreply, Media};

handle_info({'DOWN', _Ref, process, Source, _Reason}, #ems_media{source = Source, source_timeout = shutdown} = Media) ->
  ?D({"ems_media lost source with source_timeout=shutdown", Source, _Reason}),
  {stop, normal, Media};

handle_info({'DOWN', _Ref, process, Source, _Reason}, #ems_media{source = Source} = Media) ->
  source_is_lost(Media);

  
handle_info({'DOWN', _Ref, process, _Pid, _Reason} = Msg, #ems_media{} = Media) ->
  ems_media_client_control:handle_info(Msg, Media);

handle_info(#video_frame{} = Frame, Media) ->
  % ?D({Media#ems_media.url, Frame#video_frame.codec, Frame#video_frame.flavor, Frame#video_frame.dts}),
  ems_media_frame:send_frame(Frame, Media);

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
      ems_media_utils:source_is_restored(Media1#ems_media{source = NewSource})
  end;

handle_info(no_clients, Media) ->
  ems_media_client_control:handle_info(no_clients, Media);
  

handle_info(stop_wait_for_config, #ems_media{media_info = #media_info{audio = [_], video = [_]}} = Media) ->
  {noreply, Media, ?TIMEOUT};

handle_info(stop_wait_for_config, #ems_media{media_info = #media_info{audio = A, video = V} = Info} = Media) -> % 
  Info1 = Info#media_info{audio = case A of wait -> []; _ -> A end, video = case V of wait -> []; _ -> V end},
  % ?D({flush_media_info, Media#ems_media.name, Info, Info1}),
  {noreply, set_media_info(Media, Info1), ?TIMEOUT};


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
  {noreply, Media, ?TIMEOUT};


handle_info(garbage_collect, Media) ->
  garbage_collect(self()),
  {noreply, Media, ?TIMEOUT};


handle_info(make_request, #ems_media{retry_count = Count, retry_limit = Limit} = Media) 
  when is_number(Count) andalso is_number(Limit) andalso Count > Limit ->
  {stop, normal, Media};

handle_info(make_request, #ems_media{retry_count = Count, host = Host, url = NativeURL, options = Options, module = M} = Media) ->
  FailoverURLs = [NativeURL] ++ proplists:get_value(failover,Options,[]),
  URL = lists:nth(Count rem length(FailoverURLs) + 1,FailoverURLs),
  ems_event:stream_source_requested(Host, URL, []),

  case M:handle_control({make_request, URL}, Media) of
    {ok, Reader} when is_pid(Reader) ->
      ems_media:set_source(self(), Reader),
      {noreply, Media#ems_media{retry_count = 0}, ?TIMEOUT};
    {ok, Reader, #media_info{} = MediaInfo} when is_pid(Reader) ->
      ems_media:set_source(self(), Reader),
      {noreply, ems_media:set_media_info(Media#ems_media{retry_count = 0}, MediaInfo), ?TIMEOUT};
    {noreply, Media1} ->
      handle_info_with_module(make_request, Media1);
    {stop, Reason, Media1} ->
      {stop, Reason, Media1};
    {error, _Error} ->
      Timer = proplists:get_value(connect_timeout, Options, ?TIMEOUT_RESTART),
      ?D({failed_request, URL, Timer, Count, Media#ems_media.retry_limit}),
      timer:send_after(Timer, make_request),
      {noreply, Media#ems_media{retry_count = Count + 1}, ?TIMEOUT}
  end;


handle_info(Message, Media) ->
  handle_info_with_module(Message, Media).

handle_info_with_module(Message, #ems_media{module = M} = Media) ->
  case M:handle_info(Message, Media) of
    {noreply, Media1} ->
      {noreply, Media1, ?TIMEOUT};
    {noreply, Media1, Timeout} ->
      {noreply, Media1, Timeout};
    {stop, Reason, _Reply, Media1} ->
      {stop, Reason, Media1};
    {stop, Reason, Media1} ->
      ?D({"ems_media is stopping after handle_info", M, Message, Reason}),
      {stop, Reason, Media1}
  end.


reply_with_media_info(#ems_media{} = Media, #media_info{options = Options} = Info) ->
  Props = storage_properties(Media),
  case lists:keytake(duration, 1, Props) of
    {value, {duration, Duration}, Props1} -> 
      Info#media_info{duration = Duration, options = lists:ukeymerge(1, Props1, lists:ukeysort(1, Options))};
    false ->
      Info#media_info{options = lists:ukeymerge(1, Props, lists:ukeysort(1, Options))}
  end.

storage_properties(#ems_media{format = undefined}) -> [];
storage_properties(#ems_media{format = Format, storage = Storage}) -> lists:ukeysort(1,Format:properties(Storage)).

  



reply_with_info(#ems_media{type = Type, url = URL, last_dts = LastDTS, last_dts_at = LastDTSAt, created_at = CreatedAt, options = Options} = Media, Properties) ->
  lists:foldl(fun
    (hls_playlist, Props) -> [{hls_playlist, hls_media:playlist(Media#ems_media.hls_state)}|Props];
    ({hls_segment, Num}, Props) -> [{{hls_segment, Num}, hls_media:segment(Media#ems_media.hls_state, Num)}|Props];
    (type, Props)         -> [{type,Type}|Props];
    (url, Props)          -> [{url,URL}|Props];
    (last_dts, Props)     -> [{last_dts,LastDTS}|Props];
    (created_at, Props)   -> [{created_at,CreatedAt}|Props];
    (ts_delay, Props) when Type == file -> [{ts_delay,0}|Props];
    (ts_delay, Props)     -> [{ts_delay,timer:now_diff(os:timestamp(), LastDTSAt) div 1000}|Props];
    (client_count, Props) -> [{client_count,ems_media_client_control:client_count(Media)}|Props];
    (storage, Props)      -> storage_properties(Media) ++ Props;
    (clients, Props)      -> [{clients,ems_media_clients:list(Media#ems_media.clients)}|Props];
    (options, Props)      -> Props ++ Options
  end, [], Properties).



source_is_lost(#ems_media{} = Media) ->
  ems_media_utils:source_is_lost(Media).


source_is_restored(#ems_media{} = Media) ->
  ems_media_utils:source_is_restored(Media).


%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(normal, #ems_media{source = Source, name = Name}) when Source =/= undefined ->
  ?D({"ems_media exit normal", Name}),
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

