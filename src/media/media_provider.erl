% Server, that handle links to all opened files and streams. You should
% go here to open file. If file is already opened, you will get cached copy. 

-module(media_provider).
-author(max@maxidoors.ru).
-include("../include/ems.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/0, create/2, play/1, play/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(media_provider, {
  opened_media
}).

-record(media_entry, {
  name,
  handler
}).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


create(Name, Type) ->
  ?D({"Create", Name, Type}),
  gen_server:call(?MODULE, {create, Name}).

open(Name) ->
  gen_server:call(?MODULE, {open, Name}).

find(Name) ->
  gen_server:call(?MODULE, {find, Name}).
   

% Plays media with default options
play(Name) -> play(Name, []).

% Plays media named Name
% Valid options:
%   consumer: pid of media consumer
%   stream_id: for RTMP, FLV stream id
%  client_buffer: client buffer size
play(Name, Options) ->
  case find_or_open(Name) of
    {notfound, Reason} -> {notfound, Reason};
    MediaEntry -> create_player(MediaEntry, Options)
  end.
  
find_or_open(Name) ->
  case find(Name) of
    undefined -> open(Name);
    MediaEntry -> MediaEntry
  end.


create_player({notfound, Reason}, _) ->
  {notfound, Reason};
  
create_player(MediaEntry, Options) ->
  gen_server:call(MediaEntry, {create_player, lists:keymerge(1, Options, [{consumer, self()}])}).
  
  

init([]) ->
  process_flag(trap_exit, true),
  % error_logger:info_msg("Starting with file directory ~p~n", [Path]),
  OpenedMedia = ets:new(opened_media, [set, private, {keypos, #media_entry.name}]),
  {ok, #media_provider{opened_media = OpenedMedia}}.
  


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


handle_call({find, Name}, _From, MediaProvider) ->
  {reply, find_in_cache(Name, MediaProvider), MediaProvider};
  
handle_call({open, Name}, {_Opener, _Ref}, MediaProvider) ->
  {reply, open_media_entry(Name, MediaProvider), MediaProvider};

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.


find_in_cache(Name, #media_provider{opened_media = OpenedMedia}) ->
  case ets:lookup(OpenedMedia, Name) of
    [#media_entry{handler = Pid}] -> Pid;
    _ -> undefined
  end.

open_media_entry(Name, #media_provider{opened_media = OpenedMedia} = MediaProvider) ->
  case find_in_cache(Name, MediaProvider) of
    undefined ->
      Type = detect_type(Name),
      case ems_sup:start_media(Name, Type) of
        {ok, Pid} ->
          link(Pid),
          ets:insert(OpenedMedia, #media_entry{name = Name, handler = Pid}),
          ?D({"Opened", Type, Name, Pid}),
          Pid;
        _ ->
          ?D({"Error opening", Type, Name}),
          {notfound, "Failed to open "++Name}
      end;
    MediaEntry ->
      MediaEntry
  end.
  
detect_type(Name) ->
  detect_mpeg_ts(Name).

detect_mpeg_ts(Name) ->
  {ok, Re} = re:compile("http://(.*)"),
  case re:run(Name, Re) of
    {match, _Captured} -> mpeg_ts;
    _ -> detect_file(Name)
  end.

detect_file(Name) ->
  FileName = filename:join([file_play:file_dir(), Name]),
  case filelib:is_regular(FileName) of
    true -> file;
    _ -> live
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
handle_info({'EXIT', Media, _Reason}, #media_provider{opened_media = OpenedMedia} = MediaProvider) ->
  case ets:match(OpenedMedia, #media_entry{name = '$1', handler = Media}) of
    [] -> 
      {noreply, MediaProvider};
    [[Name]] ->
      ets:delete(OpenedMedia, Name),
      {noreply, MediaProvider}
  end;

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
    ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
