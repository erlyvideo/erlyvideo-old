% Media entry is instance of some resource

-module(file_media).
-author(max@maxidoors.ru).
-include("../include/ems.hrl").
-include("../include/media_info.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/3, codec_config/2, read_frame/2, name/1, seek/2, metadata/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link(Path, Type, Opts) ->
   gen_server:start_link(?MODULE, [Path, Type, Opts], []).


codec_config(MediaEntry, Type) -> gen_server:call(MediaEntry, {codec_config, Type}).
   
read_frame(MediaEntry, Key) -> gen_server:call(MediaEntry, {read, Key}).

name(Server) ->
  gen_server:call(Server, {name}).

seek(Server, Timestamp) ->
  gen_server:call(Server, {seek, Timestamp}).

metadata(Server) ->
  gen_server:call(Server, {metadata}).


init([Name, file, Opts]) ->
  process_flag(trap_exit, true),
  Clients = ets:new(clients, [set, private]),
  Host = proplists:get_value(host, Opts),
  {ok, Info} = open_file(Name, Host),
  {ok, Info#media_info{clients = Clients, type = file, host = Host}}.




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

handle_call({create_player, Options}, _From, #media_info{clients = Clients} = MediaInfo) ->
  {ok, Pid} = ems_sup:start_file_play(self(), Options),
  ets:insert(Clients, {Pid}),
  link(Pid),
  {reply, {ok, Pid}, MediaInfo};

handle_call(clients, _From, #media_info{clients = Clients} = MediaInfo) ->
  Entries = lists:map(
    fun([Pid]) -> file_play:client(Pid) end,
  ets:match(Clients, {'$1'})),
  {reply, Entries, MediaInfo};

handle_call({codec_config, Type}, _From, #media_info{format = FileFormat} = MediaInfo) ->
  {reply, FileFormat:codec_config(Type, MediaInfo), MediaInfo};

handle_call({read, '$end_of_table'}, _From, MediaInfo) ->
  {reply, {done, undefined}, MediaInfo};

handle_call({read, done}, _From, MediaInfo) ->
  {reply, {done, undefined}, MediaInfo};

handle_call({read, undefined}, _From, #media_info{frames = FrameTable} = MediaInfo) ->
  handle_call({read, ets:first(FrameTable)}, _From, MediaInfo);

handle_call({read, Key}, _From, #media_info{frames = FrameTable, format = FileFormat} = MediaInfo) ->
  Frame = FileFormat:read_frame(MediaInfo, Key),
  Next = ets:next(FrameTable, Key),
  {reply, {Frame, Next}, MediaInfo};

handle_call({name}, _From, #media_info{name = FileName} = MediaInfo) ->
  {reply, FileName, MediaInfo};
  
handle_call({seek, Timestamp}, _From, #media_info{frames = FrameTable} = MediaInfo) ->
  TimestampInt = round(Timestamp),
  Ids = ets:select(FrameTable, ets:fun2ms(fun(#file_frame{id = Id,timestamp = FrameTimestamp, keyframe = true} = _Frame) when FrameTimestamp =< TimestampInt ->
    {Id, FrameTimestamp}
  end)),
  case lists:reverse(Ids) of
    [Item | _] -> {reply, Item, MediaInfo};
    _ -> {reply, undefined, MediaInfo}
  end;


handle_call({metadata}, _From, #media_info{format = mp4} = MediaInfo) ->
  {reply, mp4:metadata(MediaInfo), MediaInfo};

handle_call({metadata}, _From, MediaInfo) ->
  {reply, undefined, MediaInfo};


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

handle_info({graceful}, #media_info{owner = undefined, name = FileName, clients = Clients} = MediaInfo) ->
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

handle_info({'EXIT', Client, _Reason}, #media_info{clients = Clients, name = FileName} = MediaInfo) ->
  ets:delete(Clients, Client),
  ?D({"Removing client of", FileName, Client, "left", ets:info(Clients, size)}),
  case ets:info(Clients, size) of
    0 -> timer:send_after(?FILE_CACHE_TIME, {graceful});
    _ -> ok
  end,
  {noreply, MediaInfo};

  
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
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

open_file(Name, Host) when is_binary(Name) ->
  open_file(binary_to_list(Name), Host);
  
open_file(Name, Host) ->
  FileName = filename:join([file_play:file_dir(Host), Name]), 
	{ok, Device} = file:open(FileName, [read, binary, {read_ahead, 100000}]),
	FileFormat = file_play:file_format(FileName),
	MediaInfo = #media_info{
	  device = Device,
	  name = FileName,
    format = FileFormat
	},
	case FileFormat:init(MediaInfo) of
		{ok, MediaInfo1} -> 
		  {ok, MediaInfo1};
    _HdrError -> 
		  ?D(_HdrError),
		  {error, "Invalid header", _HdrError}
	end.

