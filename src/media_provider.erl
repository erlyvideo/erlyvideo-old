

-module(media_provider).
-author(max@maxidoors.ru).
-include("../include/ems.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(media_provider, {
  path,
  opened_files,
  clients
}).

-record(file_entry, {
  file_name,
  device,
  header
}).

-record(client_entry, {
  file_name,
  client
}).


start_link(Path) ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [Path], []).


init([Path]) ->
  process_flag(trap_exit, true),
  error_logger:info_msg("Starting with file directory ~p~n", [Path]),
  OpenedFiles = ets:new(opened_files, [set, private, {keypos, #file_entry.file_name}]),
  Clients = ets:new(clients, [bag, private, {keypos, #client_entry.file_name}]),
  {ok, #media_provider{path = Path, clients = Clients, opened_files = OpenedFiles}}.
  


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

handle_call({open, FileName}, Opener, #media_provider{path = Path, clients = Clients, opened_files = OpenedFiles} = MediaProvider) ->
  case get_file_entry(OpenedFiles, Path) of
    false -> {reply, false, MediaProvider};
    FileEntry -> subscribe_client(FileEntry, MediaProvider, Opener)
  end;

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
terminate(_Reason, State) ->
    gen_tcp:close(State#ems_server.listener),
    ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


get_file_entry(OpenedFiles, Path) ->
  case ets:lookup(OpenedFiles, Path) of
    [FileEntry] -> FileEntry;
    [] -> open_file_entry(OpenedFiles, Path)
  end.
  
open_file_entry(Path, OpenedFiles) ->
  FileName = filename:join([file_play:file_dir(), Path]), 
	{ok, Device} = file:open(FileName, [read, binary, {read_ahead, 100000}]),
	FileFormat = file_play:file_format(FileName),
  % case FileFormat:init(#video_player{device = Device, 
  %                                    file_name = FileName,
  %                                    format = FileFormat}) of

  ets:insert(OpenedFiles, #file_entry{file_name = Path, header = undefined, device = Device}).

subscribe_client(FileName, #media_provider{clients = Clients} = MediaProvider, Opener) ->
  ets:insert(Clients, #client_entry{file_name = FileName, client = Opener}),
  {reply, ok, MediaProvider}.

