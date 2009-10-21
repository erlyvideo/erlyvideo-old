% Media entry is instance of some resource

-module(media_entry).
-author(max@maxidoors.ru).
-include("../include/ems.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/1, subscribe/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(media_entry, {
  name,
  clients,
  info
}).


start_link(Path) ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [Path], []).
   
subscribe(Server, Client) ->
  gen_server:call(Server, {subscribe, Client}).


init([Name]) ->
  process_flag(trap_exit, true),
  error_logger:info_msg("Opening file ~p~n", [Name]),
  Clients = ets:new(clients, [set, private]),
  Info = open_file(Name),
  {ok, #media_entry{clients = Clients, name = Name, info = Info}}.
  


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

handle_call({subscribe, Client}, _From, #media_entry{clients = Clients} = MediaEntry) ->
  ets:insert_new(Clients, {Client}),
  {reply, ok, MediaEntry};
  
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
handle_cast(_Msg, State) ->
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
  % case FileFormat:init(#video_player{device = Device, 
  %                                    file_name = FileName,
  %                                    format = FileFormat}) of
  ok.

