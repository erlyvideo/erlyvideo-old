% Media entry is instance of some resource

-module(stream_media).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/ems.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlyvideo/include/media_info.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/3, codec_config/2, metadata/1, publish/2, set_owner/2, pass_socket/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link(Path, Type, Opts) ->
   gen_server:start_link(?MODULE, [Path, Type, Opts], []).
   
metadata(Server) ->
  gen_server:call(Server, metadata).

codec_config(MediaEntry, Type) -> gen_server:call(MediaEntry, {codec_config, Type}).

publish(undefined, _Frame) ->
  {error, no_stream};
  

publish(Server, #video_frame{} = Frame) ->
  % ?D({Type, Timestamp}),
  Server ! Frame.
  % gen_server:call(Server, {publish, Frame}).

set_owner(Server, Owner) ->
  gen_server:call(Server, {set_owner, Owner}).

pass_socket(Media, Socket) ->
  ok = gen_tcp:controlling_process(Socket, Media),
  gen_server:call(Media, {set_socket, Socket}).
  

connect_http(#media_info{name = URL}) ->
  
  {_, _, Host, Port, Path, Query} = http_uri:parse(URL),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, line}, {active, false}], 4000),
  ?D({Host, Path, Query, "GET "++Path++" HTTP/1.1\r\nHost: "++Host++":"++integer_to_list(Port)++"\r\nAccept: */*\r\n\r\n"}),
  gen_tcp:send(Socket, "GET "++Path++" HTTP/1.1\r\nHost: "++Host++":"++integer_to_list(Port)++"\r\nAccept: */*\r\n\r\n"),
  ok = inet:setopts(Socket, [{active, once}]),
  Socket.
  


init([URL, Type, Options]) ->
  Host = proplists:get_value(host, Options),
  Timeshift = proplists:get_value(timeshift, Options),
  LifeTimeout = proplists:get_value(life_timeout, Options, ?FILE_CACHE_TIME),
  Filter = proplists:get_value(filter, Options),
  Media = init(#media_info{host = Host, name = URL, type = Type, life_timeout = LifeTimeout, 
                   filter = Filter, timeshift = Timeshift, shift = init_timeshift(Options), options = Options}),
  {ok, Media, ?TIMEOUT};


init(#media_info{type = mpegts, options = Options} = Media) ->
  ?D({"Stream media", proplists:get_value(make_request, Options, true)}),
  Sock = case proplists:get_value(make_request, Options, true) of
    true -> connect_http(Media);
    _ -> undefined
  end,
  {ok, Reader} = ems_sup:start_mpegts_reader(self()),
  Media#media_info{socket = Sock, demuxer = Reader};


init(#media_info{type = mpegts_passive} = Media) ->
  {ok, Reader} = ems_sup:start_mpegts_reader(self()),
  Media#media_info{demuxer = Reader};

init(#media_info{type = shoutcast} = Media) ->
  {ok, Reader} = ems_sup:start_shoutcast_reader(self()),
  Media#media_info{demuxer = Reader};
  

init(#media_info{type = rtsp, name = URL, options = Options} = Media) ->
  Timeout = proplists:get_value(timeout, Options, 5000),
  {ok, Reader} = rtsp_socket:read(URL, [{consumer, self()},{interleaved,true},{timeout,Timeout}]),
  Media#media_info{demuxer = Reader};
  

init(#media_info{host = Host, type = Type, name = Name, options = Options} = Media) ->
  Owner = proplists:get_value(owner, Options),
  case Owner of
    undefined -> ok;
    _ -> erlang:monitor(process, Owner)
  end,
  Device = case Type of
    live -> 
      undefined;
    record ->
    	FileName = filename:join([ems_stream:file_dir(Host), binary_to_list(Name)]),
    	(catch file:delete(FileName)),
    	ok = filelib:ensure_dir(FileName),
      {ok, Writer} = flv_writer:start_link(FileName),
      Writer
  end,
	Media#media_info{owner = Owner, device = Device}.


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


handle_call(length, _From, MediaInfo) ->
  {reply, 0, MediaInfo, ?TIMEOUT};

handle_call(mode, _From, MediaInfo) ->
  {reply, stream, MediaInfo, ?TIMEOUT};
  
handle_call({subscribe, Client}, _From, #media_info{clients = Clients, audio_config = Audio, video_config = Video} = MediaInfo) ->
  Ref = erlang:monitor(process, Client),
  Client ! Audio,
  Client ! Video,
  {reply, {ok, stream}, MediaInfo#media_info{clients = [{Client, Ref}|Clients]}, ?TIMEOUT};

handle_call({unsubscribe, Client}, _From, #media_info{clients = Clients} = MediaInfo) ->
  Clients1 = case lists:keytake(Client, 1, Clients) of
    {value, {Client, Ref}, NewClients} ->
      erlang:demonitor(Ref),
      NewClients;
    false ->
      Clients
  end,
  {reply, {ok, stream}, MediaInfo#media_info{clients = Clients1}, ?TIMEOUT};

handle_call(clients, _From, #media_info{clients = Clients} = MediaInfo) ->
  % Entries = lists:map(fun(Pid) -> gen_fsm:sync_send_event(Pid, info) end, Clients),
  {reply, Clients, MediaInfo, ?TIMEOUT};

handle_call({codec_config, video}, _From, #media_info{video_config = Config} = MediaInfo) ->
  {reply, Config, MediaInfo, ?TIMEOUT};

handle_call({codec_config, audio}, _From, #media_info{audio_config = Config} = MediaInfo) ->
  {reply, Config, MediaInfo, ?TIMEOUT};

handle_call(metadata, _From, MediaInfo) ->
  {reply, undefined, MediaInfo, ?TIMEOUT};
  
handle_call({seek, Timestamp}, _From, MediaInfo) ->
  {reply, seek_in_timeshift(MediaInfo, Timestamp), MediaInfo, ?TIMEOUT};

handle_call({read, DTS}, _From, MediaInfo) ->
  {reply, read_from_timeshift(MediaInfo, DTS), MediaInfo, ?TIMEOUT};


handle_call({set_socket, Socket}, _From, #media_info{mode = Mode} = State) ->
  inet:setopts(Socket, [{active, once}, {packet, raw}]),
  ?D({"Stream received socket in mode", Mode}),
  {reply, ok, State#media_info{socket = Socket}, ?TIMEOUT};

handle_call({set_owner, Owner}, _From, #media_info{owner = undefined} = MediaInfo) ->
  ?D({"Owner of", MediaInfo#media_info.name, Owner}),
  erlang:monitor(process, Owner),
  {reply, ok, MediaInfo#media_info{owner = Owner}, ?TIMEOUT};

handle_call({set_owner, _Owner}, _From, #media_info{owner = Owner} = MediaInfo) ->
  {reply, {error, {owner_exists, Owner}}, MediaInfo, ?TIMEOUT};

handle_call(Request, _From, State) ->
  ?D({"Undefined call", Request, _From, State}),
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
  ?D({"Undefined cast", _Msg}),
  {noreply, State, ?TIMEOUT}.

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


handle_info(graceful, #media_info{owner = undefined, name = Name, clients = Clients, life_timeout = LifeTimeout} = MediaInfo) when length(Clients) == 0 andalso LifeTimeout =/= false ->
  ?D({self(), "No readers for stream", Name}),
  {stop, normal, MediaInfo};

handle_info(graceful, #media_info{owner = undefined} = MediaInfo) ->
  ?D({self(), "Graceful no owner"}),
  {noreply, MediaInfo, ?TIMEOUT};


handle_info(graceful, #media_info{owner = _Owner} = MediaInfo) ->
  ?D({self(), "Graceful", _Owner}),
  {noreply, MediaInfo, ?TIMEOUT};
  
handle_info({'DOWN', _Ref, process, Owner, _Reason}, #media_info{owner = Owner, life_timeout = LifeTimeout} = MediaInfo) ->
  case LifeTimeout of
    false ->
      ?D({MediaInfo#media_info.name, "Owner exits, we don't"}),
      {noreply, MediaInfo#media_info{owner = undefined}, ?TIMEOUT};
    _ ->
      timer:send_after(LifeTimeout, graceful),
      % ?D({MediaInfo#media_info.name, "Owner exits, wait him", LifeTimeout}),
      {noreply, MediaInfo#media_info{owner = undefined}, ?TIMEOUT}
  end;


handle_info({'DOWN', _Ref, process, Client, _Reason}, #media_info{clients = Clients, life_timeout = LifeTimeout} = MediaInfo) ->
  Clients1 = lists:keydelete(Client, 1, Clients),
  ?D({MediaInfo#media_info.name, "Removing client", Client, "left", length(Clients1), LifeTimeout}),
  case {length(Clients1), LifeTimeout} of
    {0, 0} -> self() ! graceful;
    {0, _} when is_number(LifeTimeout) andalso LifeTimeout > 0 -> timer:send_after(LifeTimeout, graceful);
    _ -> ok
  end,
  {noreply, MediaInfo#media_info{clients = Clients1}, ?TIMEOUT};


handle_info({http, Socket, {http_response, _Version, 200, _Reply}}, State) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, State, ?TIMEOUT};

handle_info({http, Socket, {http_header, _, _Header, _, _Value}}, State) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, State, ?TIMEOUT};


handle_info({http, Socket, http_eoh}, TSLander) ->
  inet:setopts(Socket, [{active, once}, {packet, raw}]),
  {noreply, TSLander, ?TIMEOUT};


handle_info({tcp, Socket, Bin}, #media_info{demuxer = Reader, byte_counter = Counter} = State) when Reader =/= undefined ->
  inet:setopts(Socket, [{active, once}]),
  Reader ! {data, Bin},
  {noreply, State#media_info{byte_counter = Counter + size(Bin)}, ?TIMEOUT};

handle_info({tcp_closed, _Socket}, #media_info{} = Media) ->
  ?D({"Disconnected socket in mode",Media#media_info.type}),
  Socket = connect_http(Media),
  {noreply, Media#media_info{socket = Socket}, ?TIMEOUT};


handle_info(#video_frame{} = Frame, #media_info{} = Recorder) ->
  {noreply, handle_frame(Frame, Recorder), ?TIMEOUT};


handle_info({filter, Module, Message}, #media_info{filter = {Module, State}} = Recorder) ->
  State1 = Module:handle_message(State, Recorder, Message),
  {noreply, Recorder#media_info{filter = {Module, State1}}, ?TIMEOUT};

handle_info(start, State) ->
  {noreply, State, ?TIMEOUT};

handle_info(stop, #media_info{type = live} = MediaInfo) ->
  {noreply, MediaInfo, ?TIMEOUT};

handle_info(stop, #media_info{host = Host, name = Name} = MediaInfo) ->
  media_provider:remove(Host, Name),
  {noreply, MediaInfo, ?TIMEOUT};

handle_info(exit, State) ->
  {stop, normal, State};

handle_info(timeout, #media_info{type = live, life_timeout = LifeTimeout} = State) when LifeTimeout =/= false ->
  {stop, normal, State};

handle_info(pause, State) ->
  {noreply, State, ?TIMEOUT};

handle_info(resume, State) ->
  {noreply, State, ?TIMEOUT};
  
handle_info({client_buffer, _Buffer}, State) ->
  {noreply, State, ?TIMEOUT};

handle_info(clean_timeshift, #media_info{timeshift = Timeshift} = MediaInfo) when is_number(Timeshift) andalso Timeshift > 0 ->
  {noreply, clean_timeshift(MediaInfo), ?TIMEOUT};
  
handle_info(clean_timeshift, MediaInfo) ->
  {noreply, MediaInfo, ?TIMEOUT};

handle_info(Message, State) ->
  {stop, {unhandled, Message}, State}.


%%%%%%%%%%%%%%%           Timeshift features         %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_timeshift(Options) ->
  case proplists:get_value(timeshift, Options) of
    undefined -> 
      undefined;
    Shift when is_number(Shift) andalso Shift > 0 ->
      First = 0,
      Last = 0,
      Size = round(Shift*7 / 100),
      timer:send_interval(5000, clean_timeshift),
      {First, Last, array:new([{size,Size},{fixed,true}])}
  end.

store_timeshift(MediaInfo, #video_frame{decoder_config = true}) ->
  MediaInfo;

store_timeshift(#media_info{shift = {First, Last, Frames}} = MediaInfo, #video_frame{} = Frame) ->
  Last1 = (Last + 1) rem array:size(Frames),
  First1 = case Last1 of
    First -> (First + 1) rem array:size(Frames);
    _ -> First
  end,
  MediaInfo#media_info{shift = {First1, Last1, array:set(Last, Frame, Frames)}};

store_timeshift(MediaInfo, _Frame) ->
  MediaInfo.



seek_in_timeshift(#media_info{shift = {First, Last, Frames}}, Timestamp) ->
  seek_in_timeshift(First, Last, Frames, Timestamp, {undefined, undefined}).

seek_in_timeshift(First, First, _Frames, _Timestamp, Key) ->
  Key;
  
seek_in_timeshift(First, Last, Frames, Timestamp, Key) ->
  case array:get(First, Frames) of
    #video_frame{dts = DTS} when DTS > Timestamp ->
      Key;
    #video_frame{type = video, frame_type = keyframe, decoder_config = false, dts = DTS} ->
      seek_in_timeshift(First+1, Last, Frames, Timestamp, {First, DTS});
    #video_frame{} ->
      seek_in_timeshift(First+1, Last, Frames, Timestamp, Key)
  end.
  

read_from_timeshift(#media_info{shift = {_First, _Last, Shift}}, I) ->
  Frame = array:get(I, Shift),
  {Frame, (I + 1) rem array:size(Shift)}.


clean_timeshift(#media_info{shift = {First, Last, Frames}} = MediaInfo) ->
  ?D({"Store", First, Last, array:size(Frames)}),
  MediaInfo.




%%%%%%%%%%%%%%%%%%        Frame handling         %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_frame(#video_frame{dts = DTS} = Frame, #media_info{device = Device} = Recorder) ->
  {Frame1, Recorder0} = pass_through_filter(Frame#video_frame{stream_id = 1}, Recorder#media_info{last_dts = DTS}),
  % Frame1 = Frame,
  % Recorder0 = Recorder,

  send_frame(Frame1, Recorder),
  Recorder1 = parse_metadata(Recorder0, Frame1),
  Recorder2 = copy_audio_config(Recorder1, Frame1),
  Recorder3 = copy_video_config(Recorder2, Frame1),
  Recorder4 = store_timeshift(Recorder3, Frame1),
  % Recorder4 = store_last_gop(Recorder3, Frame),
  Recorder5 = Recorder4,
  (catch Device ! Frame1),
  Recorder5.


send_client_frame({Client, _}, Frame) ->
  Client ! Frame.

send_frame(Frame, #media_info{clients = Clients}) ->
  % ?D({Frame#video_frame.type, Frame#video_frame.frame_type, Frame#video_frame.decoder_config, Frame#video_frame.dts}),
  lists:foldl(fun send_client_frame/2, Frame, Clients).
  
  
pass_through_filter(#video_frame{} = Frame, #media_info{filter = undefined} = Recorder) ->
  {Frame, Recorder};

pass_through_filter(#video_frame{} = Frame, #media_info{filter = {Module, State}} = Recorder) ->
  {ok, State1, Frame1} = Module:handle_frame(State, Recorder, Frame),
  {Frame1, Recorder#media_info{filter = {Module, State1}}}.

store_last_gop(MediaInfo, #video_frame{type = video, frame_type = keyframe} = Frame) ->
  ?D({"New GOP", round((Frame#video_frame.dts - MediaInfo#media_info.base_timestamp)/1000)}),
  MediaInfo#media_info{gop = [Frame]};

store_last_gop(#media_info{gop = GOP} = MediaInfo, _) when length(GOP) == 5000 ->
  ?D({"GOP longer than 5000 frames"}),
  MediaInfo#media_info{gop = []};

store_last_gop(#media_info{gop = GOP} = MediaInfo, Frame) when is_list(GOP) ->
  MediaInfo#media_info{gop = [Frame | GOP]};

  
store_last_gop(MediaInfo, _) ->
  MediaInfo.


copy_audio_config(MediaInfo, #video_frame{decoder_config = true, type = audio} = Frame) ->
  MediaInfo#media_info{audio_config = Frame};

copy_audio_config(MediaInfo, _) -> MediaInfo.

copy_video_config(MediaInfo, #video_frame{decoder_config = true, type = video} = Frame) ->
  % ?D({"Video config", Frame}),
  send_frame(h264:metadata(Frame#video_frame.body), MediaInfo),
  MediaInfo#media_info{video_config = Frame};

copy_video_config(MediaInfo, _) -> MediaInfo.


parse_metadata(MediaInfo, #video_frame{type = metadata, body = Metadata}) ->
  parse_metadata(MediaInfo, Metadata);

parse_metadata(MediaInfo, #video_frame{}) ->
  MediaInfo;
  
parse_metadata(MediaInfo, undefined) ->
  MediaInfo;
  
parse_metadata(MediaInfo, [{object, Metadata}]) ->
  % ?D({"Metadata", Metadata}),
  set_metadata(MediaInfo, Metadata);

parse_metadata(MediaInfo, [_|Metadata]) -> parse_metadata(MediaInfo, Metadata);
parse_metadata(MediaInfo, []) -> MediaInfo.

set_metadata(MediaInfo, [{framerate, Rate} | Metadata]) ->
  set_metadata(MediaInfo#media_info{framerate = Rate}, Metadata);

set_metadata(MediaInfo, [{width, Width} | Metadata]) ->
  set_metadata(MediaInfo#media_info{width = round(Width)}, Metadata);

set_metadata(MediaInfo, [{height, Height} | Metadata]) ->
  set_metadata(MediaInfo#media_info{height = round(Height)}, Metadata);

set_metadata(MediaInfo, [{videocodecid, VideoCodec} | Metadata]) ->
  set_metadata(MediaInfo#media_info{video_codec = video_codec(VideoCodec)}, Metadata);

set_metadata(MediaInfo, [{audiocodecid, AudioCodec} | Metadata]) ->
  set_metadata(MediaInfo#media_info{audio_codec = audio_codec(AudioCodec)}, Metadata);

set_metadata(MediaInfo, [{_Key, _Value} | Metadata]) ->
  % ?D({_Key, _Value}),
  set_metadata(MediaInfo, Metadata);

set_metadata(MediaInfo, []) -> MediaInfo.

video_codec(<<"avc1">>) -> avc;
video_codec(<<"VP62">>) -> vp6.


audio_codec(<<"nmos">>) -> nelly_moser;
audio_codec(<<".mp3">>) -> mp3.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, #media_info{device = Device} = _MediaInfo) ->
  (catch file:close(Device)),
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

