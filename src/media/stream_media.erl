% Media entry is instance of some resource

-module(stream_media).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/ems.hrl").
-include_lib("erlyvideo/include/video_frame.hrl").
-include("../include/media_info.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/3, codec_config/2, metadata/1, publish/2, set_owner/2]).

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
  

init([Name, live, Opts]) ->
  Host = proplists:get_value(host, Opts),
  LifeTimeout = proplists:get_value(life_timeout, Opts, ?FILE_CACHE_TIME),
  Owner = proplists:get_value(owner, Opts),
  case Owner of
    undefined -> ok;
    _ -> erlang:monitor(process, Owner)
  end,
  Clients = [],
  % Header = flv:header(#flv_header{version = 1, audio = 1, video = 1}),
	Recorder = #media_info{type = live, name = Name, host = Host, owner = Owner,
	                       clients = Clients, life_timeout = LifeTimeout},
	{ok, Recorder, ?TIMEOUT};


init([Name, record, Opts]) ->
  Host = proplists:get_value(host, Opts),
  LifeTimeout = proplists:get_value(life_timeout, Opts, ?FILE_CACHE_TIME),
  Owner = proplists:get_value(owner, Opts),
  Clients = [],
	FileName = filename:join([file_play:file_dir(Host), binary_to_list(Name)]),
	(catch file:delete(FileName)),
	ok = filelib:ensure_dir(FileName),
	Header = flv:header(),
	case file:open(FileName, [write, {delayed_write, 1024, 50}]) of
		{ok, Device} ->
		  file:write(Device, Header),
		  Recorder = #media_info{type = record, host = Host, device = Device, name = Name, owner = Owner,
		                         path = FileName, clients = Clients, life_timeout = LifeTimeout},
			{ok, Recorder, ?TIMEOUT};
		_Error ->
		  error_logger:error_msg("Failed to start recording stream to ~p because of ~p", [FileName, _Error]),
			ignore
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

handle_call({create_player, Options}, _From, #media_info{clients = Clients, gop = GOP} = MediaInfo) ->
  {ok, Pid} = ems_sup:start_stream_play(self(), Options),
  erlang:monitor(process, Pid),
  link(Pid),
  % ?D({"Creating media player for", MediaInfo#media_info.host, Name, "client", proplists:get_value(consumer, Options), "->", Pid}),
  case MediaInfo#media_info.video_decoder_config of
    undefined -> ok;
    VideoConfig -> Pid ! VideoConfig
  end,
  case MediaInfo#media_info.audio_decoder_config of
    undefined -> ok;
    AudioConfig -> Pid ! AudioConfig
  end,
  lists:foreach(fun(Frame) -> Pid ! Frame end, lists:reverse(GOP)),
  {reply, {ok, Pid}, MediaInfo#media_info{clients = [Pid | Clients]}, ?TIMEOUT};

handle_call(length, _From, MediaInfo) ->
  {reply, 0, MediaInfo};

handle_call(clients, _From, #media_info{clients = Clients} = MediaInfo) ->
  % Entries = lists:map(fun(Pid) -> gen_fsm:sync_send_event(Pid, info) end, Clients),
  {reply, Clients, MediaInfo, ?TIMEOUT};

handle_call({codec_config, video}, _From, #media_info{video_decoder_config = Config} = MediaInfo) ->
  {reply, Config, MediaInfo, ?TIMEOUT};

handle_call({codec_config, audio}, _From, #media_info{audio_decoder_config = Config} = MediaInfo) ->
  {reply, Config, MediaInfo, ?TIMEOUT};

handle_call(metadata, _From, MediaInfo) ->
  {reply, undefined, MediaInfo, ?TIMEOUT};

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

handle_info(graceful, #media_info{owner = undefined, name = Name, clients = Clients} = MediaInfo) when length(Clients) == 0 ->
  ?D({self(), "No readers for stream", Name}),
  {stop, normal, MediaInfo};

handle_info(graceful, #media_info{owner = undefined} = MediaInfo) ->
  ?D({self(), "Graceful no owner"}),
  {noreply, MediaInfo, ?TIMEOUT};


handle_info(graceful, #media_info{owner = _Owner} = MediaInfo) ->
  ?D({self(), "Graceful", _Owner}),
  {noreply, MediaInfo, ?TIMEOUT};
  
handle_info({'DOWN', _Ref, process, Owner, _Reason}, #media_info{owner = Owner, clients = Clients} = MediaInfo) when length(Clients) == 0 ->
  ?D({self(), "Owner exits", Owner}),
  {stop, normal, MediaInfo};

handle_info({'DOWN', _Ref, process, Owner, _Reason}, #media_info{owner = Owner, life_timeout = LifeTimeout} = MediaInfo) ->
  case LifeTimeout of
    false ->
      % ?D({MediaInfo#media_info.name, "Owner exits, we too"}),
      {stop, normal, MediaInfo};
    _ ->
      timer:send_after(LifeTimeout, graceful),
      % ?D({MediaInfo#media_info.name, "Owner exits, wait him", LifeTimeout}),
      {noreply, MediaInfo#media_info{owner = undefined}, ?TIMEOUT}
  end;

handle_info({'EXIT', Device, _Reason}, #media_info{device = Device, type = mpeg_ts, clients = Clients} = MediaInfo) ->
  lists:foreach(fun(Client) -> Client ! eof end, Clients),
  {stop, normal, MediaInfo};

handle_info({'DOWN', _Ref, process, Client, _Reason}, #media_info{clients = Clients, life_timeout = LifeTimeout} = MediaInfo) ->
  Clients1 = lists:delete(Client, Clients),
  ?D({MediaInfo#media_info.name, "Removing client", Client, "left", length(Clients1), LifeTimeout}),
  case {length(Clients1), LifeTimeout} of
    {0, 0} -> self() ! graceful;
    {0, _} when LifeTimeout > 0 -> timer:send_after(LifeTimeout, graceful);
    _ -> ok
  end,
  {noreply, MediaInfo#media_info{clients = Clients1}, ?TIMEOUT};

handle_info(#video_frame{dts = TS} = Frame, #media_info{base_timestamp = undefined} = Recorder) ->
  handle_info(Frame, Recorder#media_info{base_timestamp = TS});

handle_info(#video_frame{dts = DTS, pts = PTS} = Frame, 
            #media_info{device = Device, clients = Clients, base_timestamp = BaseTS} = Recorder) ->
  Frame1 = Frame#video_frame{dts = DTS - BaseTS, pts = PTS - BaseTS, stream_id = 1},
  lists:foreach(fun(Client) -> Client ! Frame1 end, Clients),
  Recorder1 = parse_metadata(Recorder, Frame),
  Recorder2 = copy_audio_config(Recorder1, Frame),
  Recorder3 = copy_video_config(Recorder2, Frame),
  % Recorder4 = store_last_gop(Recorder3, Frame),
  Recorder4 = Recorder3,
  % ?D({Frame#video_frame.type, Frame#video_frame.frame_type, TS-BaseTS}),
  case Device of
	  undefined -> ok;
	  _ -> file:write(Device, ems_flv:to_tag(Frame1))
	end,
  {noreply, Recorder4, ?TIMEOUT};

handle_info(start, State) ->
  {noreply, State, ?TIMEOUT};

handle_info(stop, #media_info{type = live} = MediaInfo) ->
  {noreply, MediaInfo, ?TIMEOUT};

handle_info(stop, #media_info{host = Host, name = Name} = MediaInfo) ->
  media_provider:remove(Host, Name),
  {noreply, MediaInfo, ?TIMEOUT};

handle_info(exit, State) ->
  {stop, normal, State};

handle_info(timeout, #media_info{type = live} = State) ->
  {stop, normal, State};

handle_info(timeout, #media_info{host = Host, name = Name} = State) ->
  media_provider:remove(Host, Name),
  {stop, normal, State};

handle_info(pause, State) ->
  {noreply, State, ?TIMEOUT};

handle_info(resume, State) ->
  {noreply, State, ?TIMEOUT};
  
handle_info({client_buffer, 0}, State) ->
  {noreply, State, ?TIMEOUT};

handle_info(Message, State) ->
  {stop, {unhandled, Message}, State}.

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
  MediaInfo#media_info{audio_decoder_config = Frame};

copy_audio_config(MediaInfo, _) -> MediaInfo.

copy_video_config(MediaInfo, #video_frame{decoder_config = true, type = video} = Frame) ->
  ?D({"Video decoder config"}),
  MediaInfo#media_info{video_decoder_config = Frame};

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

