% Server, that handle links to all opened files and streams. You should
% go here to open file. If file is already opened, you will get cached copy. 

-module(media_provider).
-author(max@maxidoors.ru).
-include("../include/ems.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/0, open/2, play/1, play/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(media_provider, {
  opened_media
}).

-record(media_entry, {
  name,
  handler
}).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
   
   
play(Name) -> play(Name, []).

play(Name, Options) ->
  case find(Name) of
    undefined -> open_file(Name, Options);
    Server -> connect_to_media(Server, Options)
  end.
  
connect_to_media(Server, Options) ->
  case media_entry:is_stream(Server) of
    true -> 
      media_entry:subscribe(Server, self()),
      {ok, Server};
    _ -> 
      file_play:start(Server, Options)
  end.

% init_file(Name, StreamId, State) ->
%   case start_file_play(Name, State, StreamId) of
%     {ok, Pid} -> {ok, Pid};
%     _ -> init_mpeg_ts(Name, StreamId, State)
%   end.
% 
% init_mpeg_ts(FileName, StreamId,  State) ->
%   {ok, Re} = re:compile("http://(.*).ts"),
%   case re:run(FileName, Re) of
%     {match, _Captured} -> mpeg_ts:play(FileName, StreamId, State);
%     _ -> init_stream(FileName, StreamId, State)
%   end.
% 
% init_stream(Name, _StreamId, _State) ->
%   case ems:get_var(netstream, undefined) of
%     undefined -> {notfound};
%     NetStreamNode -> case rpc:call(NetStreamNode, rtmp, start, [Name], ?TIMEOUT) of
%       {ok, NetStream} ->
%         link(NetStream),
%         ?D({"Netstream created", NetStream}),
%         {ok, NetStream};
%       _ ->
%         {notfound}
%       end
%   end.


open_file(Name, Options) ->
  case open(Name, file) of
    undefined -> {notfound};
    Server -> file_play:start(Server, Options)
  end.
   
open(Name, Type) ->
  gen_server:call(?MODULE, {open, Name, Type}).

find(Name) ->
  gen_server:call(?MODULE, {find, Name}).


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


handle_call({find, Name}, _From, #media_provider{opened_media = OpenedMedia} = MediaProvider) ->
  Server = case ets:lookup(OpenedMedia, Name) of
    [#media_entry{handler = Pid}] -> Pid;
    [] -> undefined
  end,
  {reply, Server, MediaProvider};
  

handle_call({open, Name, Type}, {Opener, _Ref}, #media_provider{opened_media = OpenedMedia} = MediaProvider) ->
  ?D({"Lookup for media", Name, ets:lookup(OpenedMedia, Name)}),
  Server = case ets:lookup(OpenedMedia, Name) of
    [#media_entry{handler = Pid}] -> Pid;
    [] -> 
      case ems_sup:start_media(Name, Type) of
        {ok, Pid} ->
          link(Pid),
          ets:insert(OpenedMedia, #media_entry{name = Name, handler = Pid}),
          ?D({"Inserting", Name, Pid}),
          Pid;
        ignore ->
          undefined
      end
  end,
  case {Server, Type} of
    {undefined, _} -> 
      {reply, undefined, MediaProvider};
    {_, file} ->
      ok = media_entry:subscribe(Server, Opener),
      {reply, Server, MediaProvider};
    {_, live} ->
      {reply, Server, MediaProvider};
    {_, append} ->
      {reply, Server, MediaProvider};
    {_, record} ->
      {reply, Server, MediaProvider}
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
handle_info({'EXIT', Media, Reason}, #media_provider{opened_media = OpenedMedia} = MediaProvider) ->
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
