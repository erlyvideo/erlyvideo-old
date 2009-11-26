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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


start_link(Path, Type) ->
   gen_server:start_link(?MODULE, [Path, Type], []).
   
metadata(Server) ->
  gen_server:call(Server, {metadata}).

codec_config(MediaEntry, Type) -> gen_server:call(MediaEntry, {codec_config, Type}).

publish(Server, Frame) ->
  gen_server:call(Server, {publish, Frame}).

set_owner(Server, Owner) ->
  gen_server:call(Server, {set_owner, Owner}).
  

init([URL, mpeg_ts]) ->
  process_flag(trap_exit, true),
  error_logger:info_msg("HTTP MPEG TS ~p~n", [URL]),
  Clients = ets:new(clients, [set, private]),
  % Header = flv:header(#flv_header{version = 1, audio = 1, video = 1}),
  {ok, Device} = ems_sup:start_ts_lander(URL, self()),
  link(Device),
	Recorder = #media_info{type=mpeg_ts, file_name = URL, ts_prev = 0, clients = Clients, device = Device},
	{ok, Recorder, ?TIMEOUT};
  

init([Name, live]) ->
  process_flag(trap_exit, true),
  error_logger:info_msg("Live streaming stream ~p~n", [Name]),
  Clients = ets:new(clients, [set, private]),
  % Header = flv:header(#flv_header{version = 1, audio = 1, video = 1}),
	Recorder = #media_info{type=live, ts_prev = 0, clients = Clients},
	{ok, Recorder, ?TIMEOUT};


init([Name, record]) ->
  process_flag(trap_exit, true),
  error_logger:info_msg("Recording stream ~p~n", [Name]),
  Clients = ets:new(clients, [set, private]),
	FileName = filename:join([file_play:file_dir(), Name ++ ".flv"]),
	(catch file:delete(FileName)),
	ok = filelib:ensure_dir(FileName),
	Header = flv:header(#flv_header{version = 1, audio = 1, video = 1}),
	?D({"Recording to file", FileName}),
	case file:open(FileName, [write, {delayed_write, 1024, 50}]) of
		{ok, Device} ->
		  file:write(Device, Header),
		  Recorder = #media_info{type=record, device = Device, file_name = FileName, ts_prev = 0, clients = Clients},
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

handle_call({create_player, Options}, {Caller, _Ref}, 
  #media_info{file_name = Name, clients = Clients, video_decoder_config = Video, audio_decoder_config = Audio} = MediaInfo) ->
  % {ok, Pid} = stream_play:start(self(), Options),
  Pid = proplists:get_value(consumer, Options, Caller),
  ets:insert(Clients, {Pid}),
  link(Pid),
  ?D({"Creating media player for", Name, "client", Pid}),
  ems_play:send(Pid, Video),
  ems_play:send(Pid, Audio),
  {reply, {ok, self()}, MediaInfo};


handle_call(clients, _From, #media_info{clients = Clients} = MediaInfo) ->
  Entries = lists:map(
    fun([Pid]) -> gen_fsm:sync_send_event(Pid, info) end,
  ets:match(Clients, {'$1'})),
  {reply, Entries, MediaInfo};

handle_call({codec_config, video}, _From, #media_info{video_decoder_config = Config} = MediaInfo) ->
  {reply, Config, MediaInfo};

handle_call({codec_config, audio}, _From, #media_info{audio_decoder_config = Config} = MediaInfo) ->
  {reply, Config, MediaInfo};

handle_call({metadata}, _From, MediaInfo) ->
  {reply, undefined, MediaInfo};

handle_call({set_owner, Owner}, _From, #media_info{owner = undefined} = MediaInfo) ->
  ?D({"Setting owner to", Owner}),
  {reply, ok, MediaInfo#media_info{owner = Owner}};

handle_call({set_owner, _Owner}, _From, #media_info{owner = Owner} = MediaInfo) ->
  {reply, {error, {owner_exists, Owner}}, MediaInfo};


handle_call({publish, Channel}, _From, #media_info{device = Device, clients = Clients} = Recorder) ->
	Tag = ems_flv:to_tag(Channel),
  % ?D({"Record",Channel#channel.type, Channel#channel.timestamp}),
	case Device of
	  undefined -> ok;
	  _ -> file:write(Device, Tag)
	end,
  Packet = Channel#channel{id = ems_play:channel_id(Channel#channel.type,1)},
  ets:foldl(fun send_packet/2, Packet, Clients),
	{reply, ok, Recorder};

handle_call(Request, _From, State) ->
  ?D({"Undefined call", Request, _From}),
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
  {noreply, State}.

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

handle_info({graceful}, #media_info{owner = undefined, file_name = FileName, clients = Clients} = MediaInfo) ->
  case ets:info(Clients, size) of
    0 -> ?D({"No readers for file", FileName}),
         {stop, normal, MediaInfo};
    _ -> {noreply, MediaInfo}
  end;


handle_info({graceful}, #media_info{owner = _Owner} = MediaInfo) ->
  {noreply, MediaInfo};
  
handle_info({'EXIT', Owner, _Reason}, #media_info{owner = Owner, clients = Clients} = MediaInfo) ->
  case ets:info(Clients, size) of
    0 -> timer:send_after(?FILE_CACHE_TIME, {graceful});
    _ -> ok
  end,
  {noreply, MediaInfo#media_info{owner = Owner}};

handle_info({'EXIT', Device, _Reason}, #media_info{device = Device, type = mpeg_ts, clients = Clients} = MediaInfo) ->
  ?D("MPEG TS finished"),
  ets:foldl(fun({Client}, Packet) -> gen_fsm:send_event(Client, Packet), Packet end, {status, ?NS_PLAY_COMPLETE, 1}, Clients),
  {stop, normal, MediaInfo};

handle_info({'EXIT', Client, _Reason}, #media_info{clients = Clients} = MediaInfo) ->
  ets:delete(Clients, Client),
  ?D({"Removing client", Client, "left", ets:info(Clients, size)}),
  case ets:info(Clients, size) of
    0 -> timer:send_after(?FILE_CACHE_TIME, {graceful});
    _ -> ok
  end,
  {noreply, MediaInfo};

handle_info({video, Video}, #media_info{clients = Clients} = MediaInfo) ->
  ets:foldl(fun({Client}, Packet) -> ems_play:send(Client, Packet), Packet end, Video, Clients),
  {noreply, MediaInfo};

handle_info({video_config, Video}, #media_info{clients = Clients} = MediaInfo) ->
  ets:foldl(fun({Client}, Packet) -> ems_play:send(Client, Packet), Packet end, Video, Clients),
  {noreply, MediaInfo#media_info{video_decoder_config = Video}};

handle_info({audio, Audio}, #media_info{clients = Clients} = MediaInfo) ->
  ets:foldl(fun({Client}, Packet) -> ems_play:send(Client, Packet), Packet end, Audio, Clients),
  {noreply, MediaInfo};

handle_info({audio_config, Audio}, #media_info{clients = Clients} = MediaInfo) ->
  ets:foldl(fun({Client}, Packet) -> ems_play:send(Client, Packet), Packet end, Audio, Clients),
  {noreply, MediaInfo#media_info{audio_decoder_config = Audio}};

handle_info(start, State) ->
  {noreply, State};

handle_info(stop, State) ->
  {noreply, State};

handle_info(pause, State) ->
  {noreply, State};

handle_info(resume, State) ->
  {noreply, State};

handle_info(_Info, State) ->
  ?D({"Undefined info", _Info}),
  {noreply, State}.

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

send_packet({Client}, #channel{msg = Data} = Channel) ->
  % ?D({"Send to", Client}),
  gen_fsm:send_event(Client, {send, {Channel, Data}}),
  Channel;

send_packet({Client}, Packet) ->
  % ?D({"Send to", Client}),
  gen_fsm:send_event(Client, {send, Packet}),
  Packet.
  
