% Media entry is instance of some resource

-module(media_entry).
-author(max@maxidoors.ru).
-include("../include/ems.hrl").
-include("../include/media_info.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/2, subscribe/2, first/1, read/2, file_name/1, seek/2, metadata/1, publish/2, is_stream/1, set_owner/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


start_link(Path, Type) ->
   gen_server:start_link(?MODULE, [Path, Type], []).
   
subscribe(Server, Client) ->
  gen_server:call(Server, {subscribe, Client}).
  
read(Server, Player) ->
  gen_server:call(Server, {read, Player}).

first(Server) ->
  gen_server:call(Server, {first}).

file_name(Server) ->
  gen_server:call(Server, {file_name}).

seek(Server, Timestamp) ->
  gen_server:call(Server, {seek, Timestamp}).

metadata(Server) ->
  gen_server:call(Server, {metadata}).

publish(Server, Frame) ->
  gen_server:call(Server, {publish, Frame}).

is_stream(Server) ->
  gen_server:call(Server, {is_stream}).
  
set_owner(Server, Owner) ->
  gen_server:call(Server, {set_owner, Owner}).
  

init([Name, file]) ->
  process_flag(trap_exit, true),
  FileName = filename:join([file_play:file_dir(), Name]),
  case filelib:is_regular(FileName) of
    true ->
      error_logger:info_msg("Opening file ~p~n", [FileName]),
      Clients = ets:new(clients, [set, private]),
      {ok, Info} = open_file(FileName),
      {ok, Info#media_info{clients = Clients, type = file}};
    _ -> 
      ignore
  end;
  

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
	Header = flv:header(#flv_header{version = 1, audio = 1, video = 1}),
	?D({"Recording to file", FileName}),
	case file:open(FileName, [write, {delayed_write, 1024, 50}]) of
		{ok, Device} ->
		  file:write(Device, Header),
		  Recorder = #media_info{type=record, device = Device, file_name = FileName, ts_prev = 0, clients = Clients},
			{ok, Recorder, ?TIMEOUT};
		_Error ->
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

handle_call({subscribe, Client}, _From, #media_info{file_name = Name, clients = Clients} = MediaInfo) when is_pid(Client) ->
  ets:insert(Clients, {Client}),
  link(Client),
  ?D({"Link from to", Name, self(), Client, ets:info(Clients, size)}),
  % link(_From),
  {reply, ok, MediaInfo};


handle_call({first}, _From, #media_info{frames = FrameTable} = MediaInfo) ->
  {reply, ets:first(FrameTable), MediaInfo};


handle_call({read, State}, _From, #media_info{format = FileFormat} = MediaInfo) ->
  {reply, FileFormat:read_frame(State, MediaInfo), MediaInfo};

handle_call({file_name}, _From, #media_info{file_name = FileName} = MediaInfo) ->
  {reply, FileName, MediaInfo};
  
handle_call({seek, Timestamp}, _From, #media_info{frames = FrameTable} = MediaInfo) ->
  Ids = ets:select(FrameTable, ets:fun2ms(fun(#file_frame{id = Id,timestamp = FrameTimestamp, keyframe = true} = _Frame) when FrameTimestamp =< Timestamp ->
    {Id, FrameTimestamp}
  end)),
  [Item | _] = lists:reverse(Ids),
  {reply, Item, MediaInfo};


handle_call({metadata}, _From, #media_info{format = mp4} = MediaInfo) ->
  {reply, mp4:metadata(MediaInfo), MediaInfo};

handle_call({metadata}, _From, MediaInfo) ->
  {reply, undefined, MediaInfo};


handle_call({is_stream}, _From, #media_info{type = record} = MediaInfo) ->
  {reply, true, MediaInfo};

handle_call({is_stream}, _From, MediaInfo) ->
  {reply, false, MediaInfo};


handle_call({set_owner, Owner}, _From, #media_info{owner = undefined} = MediaInfo) ->
  ?D({"Setting owner to", Owner}),
  {reply, ok, MediaInfo#media_info{owner = Owner}};

handle_call({set_owner, _Owner}, _From, #media_info{owner = Owner} = MediaInfo) ->
  {reply, {error, {owner_exists, Owner}}, MediaInfo};


handle_call({publish, Channel}, _From, #media_info{ts_prev = PrevTs, device = Device, clients = Clients} = Recorder) ->
  % ?D({"Record",Channel#channel.id, Channel#channel.type,size(Channel#channel.msg),Channel#channel.timestamp,PrevTs}),
	{Tag,NextTimeStamp} = ems_flv:to_tag(Channel,PrevTs),
	case Device of
	  undefined -> ok;
	  _ -> file:write(Device, Tag)
	end,
	
  NextTimeStamp = PrevTs + Channel#channel.timestamp,
  %	?D({"Broadcast",Channel#channel.id,Channel#channel.type,size(Channel#channel.msg),NextTimeStamp}),
  Packet = Channel#channel{id = ems_play:channel_id(Channel#channel.type,1), timestamp = NextTimeStamp},
  % ?D({"Broadcast to", ets:info(Clients, size)}),
  ets:foldl(fun send_packet/2, Packet, Clients),
	
	{reply, ok, Recorder#media_info{ts_prev = NextTimeStamp}};

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
  
  
handle_info({'EXIT', Owner, _Reason}, #media_info{owner = Owner, clients = Clients} = MediaInfo) ->
  case ets:info(Clients, size) of
    0 -> timer:send_after(?FILE_CACHE_TIME, {graceful});
    _ -> ok
  end,
  {noreply, MediaInfo#media_info{owner = Owner}};

handle_info({'EXIT', Client, _Reason}, #media_info{clients = Clients} = MediaInfo) ->
  ets:delete(Clients, Client),
  ?D({"Removing client", Client, "left", ets:info(Clients, size)}),
  case ets:info(Clients, size) of
    0 -> timer:send_after(?FILE_CACHE_TIME, {graceful});
    _ -> ok
  end,
  {noreply, MediaInfo};

handle_info({'$gen_event', {stop}}, State) ->
  {noreply, State};

handle_info({'$gen_event', {start}}, State) ->
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
  Channel.
  
open_file(Name) ->
  FileName = filename:join([file_play:file_dir(), Name]), 
	{ok, Device} = file:open(FileName, [read, binary, {read_ahead, 100000}]),
	FileFormat = file_play:file_format(FileName),
	MediaInfo = #media_info{
	  device = Device,
	  file_name = FileName,
    format = FileFormat
	},
	case FileFormat:init(MediaInfo) of
		{ok, MediaInfo1} -> 
		  {ok, MediaInfo1};
    _HdrError -> 
		  ?D(_HdrError),
		  {error, "Invalid header", _HdrError}
	end.

