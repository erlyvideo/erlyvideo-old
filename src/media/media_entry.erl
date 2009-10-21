% Media entry is instance of some resource

-module(media_entry).
-author(max@maxidoors.ru).
-include("../include/ems.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/1, subscribe/2, first/1, read/2, file_name/1, seek/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


start_link(Path) ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [Path], []).
   
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


init([Name]) ->
  process_flag(trap_exit, true),
  error_logger:info_msg("Opening file ~p~n", [Name]),
  Clients = ets:new(clients, [set, private]),
  {ok, Info} = open_file(Name),
  {ok, Info#media_info{clients = Clients}}.
  


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

handle_call({subscribe, Client}, _From, #media_info{clients = Clients} = MediaInfo) ->
  ets:insert(Clients, {Client}),
  % link(Client),
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
terminate(_Reason, _State) ->
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

