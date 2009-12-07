% Media entry is instance of some resource

-module(stream_media).
-author(max@maxidoors.ru).
-include("../include/ems.hrl").
-include("../include/media_info.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/2, codec_config/2, metadata/1, publish/2, set_owner/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link(Path, Type) ->
   gen_server:start_link(?MODULE, [Path, Type], []).
   
metadata(Server) ->
  gen_server:call(Server, {metadata}).

codec_config(MediaEntry, Type) -> gen_server:call(MediaEntry, {codec_config, Type}).

publish(undefined, Frame) ->
  {error, no_stream};

publish(Server, Frame) ->
  gen_server:call(Server, {publish, Frame}).

set_owner(Server, Owner) ->
  gen_server:call(Server, {set_owner, Owner}).
  

init([Name, live]) ->
  process_flag(trap_exit, true),
  error_logger:info_msg("Live streaming stream ~p~n", [Name]),
  Clients = [],
  % Header = flv:header(#flv_header{version = 1, audio = 1, video = 1}),
	Recorder = #media_info{type=live, name = Name, clients = Clients},
	{ok, Recorder, ?TIMEOUT};


init([Name, record]) ->
  process_flag(trap_exit, true),
  error_logger:info_msg("Recording stream ~p~n", [Name]),
  Clients = [],
	FileName = filename:join([file_play:file_dir(), binary_to_list(Name)]),
	(catch file:delete(FileName)),
	ok = filelib:ensure_dir(FileName),
	Header = flv:header(#flv_header{version = 1, audio = 1, video = 1}),
	?D({"Recording to file", FileName}),
	case file:open(FileName, [write, {delayed_write, 1024, 50}]) of
		{ok, Device} ->
		  file:write(Device, Header),
		  Recorder = #media_info{type=record, device = Device, name = Name, path = FileName, clients = Clients},
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

handle_call({create_player, Options}, _From, #media_info{name = Name, clients = Clients} = MediaInfo) ->
  {ok, Pid} = ems_sup:start_stream_play(self(), Options),
  link(Pid),
  ?D({"Creating media player for", Name, "client", proplists:get_value(consumer, Options)}),
  case MediaInfo#media_info.video_decoder_config of
    undefined -> ok;
    VideoConfig -> Pid ! VideoConfig
  end,
  case MediaInfo#media_info.audio_decoder_config of
    undefined -> ok;
    AudioConfig -> Pid ! AudioConfig
  end,
  {reply, {ok, Pid}, MediaInfo#media_info{clients = [Pid | Clients]}, ?TIMEOUT};

handle_call(clients, _From, #media_info{clients = Clients} = MediaInfo) ->
  Entries = lists:map(fun(Pid) -> gen_fsm:sync_send_event(Pid, info) end, Clients),
  {reply, Entries, MediaInfo, ?TIMEOUT};

handle_call({codec_config, video}, _From, #media_info{video_decoder_config = Config} = MediaInfo) ->
  {reply, Config, MediaInfo, ?TIMEOUT};

handle_call({codec_config, audio}, _From, #media_info{audio_decoder_config = Config} = MediaInfo) ->
  {reply, Config, MediaInfo, ?TIMEOUT};

handle_call({metadata}, _From, MediaInfo) ->
  {reply, undefined, MediaInfo, ?TIMEOUT};

handle_call({set_owner, Owner}, _From, #media_info{owner = undefined} = MediaInfo) ->
  ?D({self(), "Setting owner to", Owner}),
  {reply, ok, MediaInfo#media_info{owner = Owner}, ?TIMEOUT};

handle_call({set_owner, _Owner}, _From, #media_info{owner = Owner} = MediaInfo) ->
  {reply, {error, {owner_exists, Owner}}, MediaInfo, ?TIMEOUT};


handle_call({publish, #channel{timestamp = TS} = Channel}, _From, #media_info{base_timestamp = undefined} = Recorder) ->
  handle_call({publish, Channel}, _From, Recorder#media_info{base_timestamp = TS});

handle_call({publish, #channel{timestamp = TS} = Channel}, _From, 
            #media_info{device = Device, clients = Clients, base_timestamp = BaseTS} = Recorder) ->
  % ?D({"Record",Channel#channel.type, TS - BaseTS}),
  Channel1 = Channel#channel{timestamp = TS - BaseTS},
	Tag = ems_flv:to_tag(Channel1),
	case Device of
	  undefined -> ok;
	  _ -> file:write(Device, Tag)
	end,
  Packet = Channel1#channel{id = ems_play:channel_id(Channel1#channel.type,1)},
  lists:foreach(fun(Pid) -> Pid ! Packet end, Clients),
	{reply, ok, Recorder, ?TIMEOUT};

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

handle_info({graceful}, #media_info{owner = undefined, name = Name, clients = Clients} = MediaInfo) when length(Clients) == 0 ->
  ?D({"No readers for stream", Name}),
  {stop, normal, MediaInfo};

handle_info({graceful}, #media_info{owner = undefined} = MediaInfo) ->
  {noreply, MediaInfo, ?TIMEOUT};


handle_info({graceful}, #media_info{owner = _Owner} = MediaInfo) ->
  {noreply, MediaInfo, ?TIMEOUT};
  
handle_info({'EXIT', Owner, _Reason}, #media_info{owner = Owner, clients = Clients} = MediaInfo) when length(Clients) == 0 ->
  ?D({self(), "Owner exits", Owner}),
  {stop, normal, MediaInfo};

handle_info({'EXIT', Owner, _Reason}, #media_info{owner = Owner} = MediaInfo) ->
  timer:send_after(?FILE_CACHE_TIME, {graceful}),
  {noreply, MediaInfo#media_info{owner = undefined}, ?TIMEOUT};

handle_info({'EXIT', Device, _Reason}, #media_info{device = Device, type = mpeg_ts, clients = Clients} = MediaInfo) ->
  lists:foreach(fun(Client) -> Client ! eof end, Clients),
  {stop, normal, MediaInfo};

handle_info({'EXIT', Client, _Reason}, #media_info{clients = Clients} = MediaInfo) ->
  Clients1 = lists:delete(Client, Clients),
  ?D({"Removing client", Client, "left", length(Clients1)}),
  case length(Clients1) of
    0 -> timer:send_after(?FILE_CACHE_TIME, {graceful});
    _ -> ok
  end,
  {noreply, MediaInfo#media_info{clients = Clients}, ?TIMEOUT};

handle_info(#video_frame{} = Frame, #media_info{clients = Clients} = MediaInfo) ->
  lists:foreach(fun(Client) -> Client ! Frame end, Clients),
  {noreply, MediaInfo, ?TIMEOUT};

handle_info(start, State) ->
  {noreply, State, ?TIMEOUT};

handle_info(stop, #media_info{name = Name} = MediaInfo) ->
  media_provider:remove(Name),
  {noreply, MediaInfo, ?TIMEOUT};

handle_info(exit, #media_info{name = Name} = State) ->
  media_provider:remove(Name),
  {noreply, State, ?TIMEOUT};

handle_info(timeout, #media_info{name = Name} = State) ->
  media_provider:remove(Name),
  {stop, normal, State};

handle_info(pause, State) ->
  {noreply, State, ?TIMEOUT};

handle_info(resume, State) ->
  {noreply, State, ?TIMEOUT};

handle_info(_Info, State) ->
  ?D({"Undefined info", _Info}),
  {noreply, State, ?TIMEOUT}.

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
  ?D({"Media entry terminating", _Reason}),
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

