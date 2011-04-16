%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Mediaprovider
%%% Server, that handle links to all opened files and streams, called medias. You should
%%% go here to open file or stream. If media is already opened, you will get cached copy. 
%%% There is one media_provider instance per virtual host, so they never mix.
%%%
%%% Most often usage of media_provider is: 
%%% ```media_provider:play(Host, Name, [{stream_id,StreamId},{client_buffer,Buffer}])'''
%%%
%%% Read more about {@link ems_media.} to understand how to create plugins, that work with video streams.
%%%
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------

-module(media_provider).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../log.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/1, create/3, open/2, open/3, play/2, play/3, entries/1, remove/2, find/2, register/3, register/4]).
-export([info/1, info/2, media_info/2, detect_type/3]). % just for getStreamLength

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([init_names/0, name/1]).

-export([static_stream_name/2, start_static_stream/2, start_static_streams/0]).

-record(media_provider, {
  opened_media,
  host,
  master_pid,
  counter = 1
}).

-record(media_entry, {
  name,
  ref,
  handler
}).

%% @hidden
static_stream_name(Host, Name) ->
  list_to_atom(atom_to_list(Host) ++ "_" ++ Name).

%% @hidden  
start_static_stream(Host, Name) ->
  open(Host, Name).

%%-------------------------------------------------------------------------
%% @spec start_static_streams() -> {ok, Pid}
%% @doc Starts all preconfigured static stream.
%% Erlyvideo has concept of static streams: they are opened right after all erlyvideo was initialized
%% and monitored via supervisor.
%% This is suitable for example for reading video stream from survielance cameras.
%% @end
%%-------------------------------------------------------------------------
start_static_streams() ->
  ems_sup:start_static_streams().

%%-------------------------------------------------------------------------
%% @doc Returns registered name for media provider on host Host
%% @end
%%-------------------------------------------------------------------------
name(Host) ->
  media_provider_names:name(Host).

%% @hidden
start_link(Host) ->
  gen_server:start_link({local, name(Host)}, ?MODULE, [Host], []).



%%-------------------------------------------------------------------------
%% @spec create(Host, Name, Options) -> {ok, Pid::pid()}
%% @doc Create stream. Must be called by process, that wants to send 
%% video frames to this stream.
%% Usually called as ``media_provider:create(Host, Name, [{type,live}])''
%% @end
%%-------------------------------------------------------------------------
create(Host, Name, Options) ->
  ?D({"Create", Name, Options}),
  {ok, Pid} = open(Host, Name, Options),
  ems_media:set_source(Pid, self()),
  {ok, Pid}.
  

% Plays media named Name
% Required options:
%   stream_id: for RTMP, FLV stream id
%
% Valid options:
%   consumer: pid of media consumer
%   client_buffer: client buffer size
%
play(Host, Name, Options) ->
  case open(Host, Name, Options) of
    {notfound, Reason} -> 
      {notfound, Reason};
    {ok, Stream} ->
      Opts = lists:ukeymerge(1, lists:ukeysort(1, Options), [{host,Host},{stream_id,1}]),
      ems_media:play(Stream, Opts),
      ems_event:user_play(Host, self(), Stream, lists:ukeymerge(1,[{name,Name}], Opts)),
      {ok, Stream}
  end.


play(Stream, Options) when is_pid(Stream) andalso is_list(Options) ->
  ems_media:play(Stream, lists:ukeymerge(1, lists:ukeysort(1, Options), [{stream_id,1}])),
  % ems_event:user_play(Host, self(), Stream, [{name,Name}|Options]),
  {ok, Stream}.
  

open(Host, Name) when is_list(Name)->
  open(Host, list_to_binary(Name));

open(Host, Name) ->
  open(Host, Name, []).

%%-------------------------------------------------------------------------
%% @spec open(Host, Name, Options) -> {ok, Pid::pid()}|undefined
%% @doc Open or start stream.
%% @end
%%-------------------------------------------------------------------------
open(Host, Name, Opts) when is_list(Name)->
  open(Host, list_to_binary(Name), Opts);

open(Host, Name, Options) ->
  case find(Host, Name) of
    {ok, Media} -> {ok, Media};
    undefined -> internal_open(Host, Name, Options)
  end.

find(Host, Name) when is_list(Name)->
  find(Host, list_to_binary(Name));

find(Host, Name) ->
  gen_server:call(name(Host), {find, Name}, infinity).

register(Host, Name, Pid) ->
  register(Host, Name, Pid, []).

register(Host, Name, Pid, Options) ->
  gen_server:call(name(Host), {register, Name, Pid, Options}).

entries(Host) ->
  Entries = gen_server:call(name(Host), entries),
  
  Info1 = [{Name, Pid, (catch ems_media:status(Pid))} || {Name,Pid} <- Entries],
  Info = [Entry || Entry <- Info1, is_list(element(3, Entry))],
  Info.
  
remove(Host, Name) when is_list(Name) ->
  remove(Host, list_to_binary(Name));
  
remove(Host, Name) when is_binary(Name) ->
  gen_server:cast(name(Host), {remove, Name}).

info(Host, Name) ->
  case open(Host, Name) of
    {ok, Media} -> media_provider:info(Media);
    _ -> []
  end.
  

info(undefined) ->
  [];
  
info(Media) ->
  ems_media:info(Media).
  

media_info(Host, Name) ->
  case open(Host, Name) of
    {ok, Media} -> ems_media:media_info(Media);
    _ -> undefined
  end.
  

init_names() ->
  Module = erl_syntax:attribute(erl_syntax:atom(module), 
                                [erl_syntax:atom("media_provider_names")]),
  Export1 = erl_syntax:attribute(erl_syntax:atom(export),
                                     [erl_syntax:list(
                                      [erl_syntax:arity_qualifier(
                                       erl_syntax:atom(name),
                                       erl_syntax:integer(1))])]),

          
  Clauses1 = lists:map(fun({Host, _}) ->
    Name = binary_to_atom(<<"media_provider_sup_", (atom_to_binary(Host, latin1))/binary>>, latin1),
    erl_syntax:clause([erl_syntax:atom(Host)], none, [erl_syntax:atom(Name)])
  end, ems:get_var(vhosts, [])),
  Function1 = erl_syntax:function(erl_syntax:atom(name), Clauses1),

  Forms = [erl_syntax:revert(AST) || AST <- [Module, Export1, Function1]],

  ModuleName = media_provider_names,
  code:purge(ModuleName),
  case compile:forms(Forms) of
    {ok,ModuleName,Binary}           -> code:load_binary(ModuleName, "media_provider_names.erl", Binary);
    {ok,ModuleName,Binary,_Warnings} -> code:load_binary(ModuleName, "media_provider_names.erl", Binary)
  end,

  ok.


  

init([Host]) ->
  % error_logger:info_msg("Starting with file directory ~p~n", [Path]),
  OpenedMedia = ets:new(opened_media, [set, private, {keypos, #media_entry.name}]),
  {ok, #media_provider{opened_media = OpenedMedia, host = Host}}.
  


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
  
handle_call({unregister, Pid}, _From, #media_provider{host = Host, opened_media = OpenedMedia} = MediaProvider) ->
  case ets:match(OpenedMedia, #media_entry{name = '$1', ref = '$2', handler = Pid}) of
    [] -> 
      {noreply, MediaProvider};
    [[Name, Ref]] ->
      erlang:demonitor(Ref, [flush]),
      ets:delete(OpenedMedia, Name),
      ?D({"Unregistering", Name, Pid}),
      ems_event:stream_stopped(Host, Name, Pid),
      {noreply, MediaProvider}
  end;
    
handle_call({register, Name, Pid, Options}, _From, #media_provider{host = Host, opened_media = OpenedMedia} = MediaProvider) ->
  case find_in_cache(Name, MediaProvider) of
    {ok, OldPid} ->
      {reply, {error, {already_set, Name, OldPid}}, MediaProvider};
    undefined ->
      Ref = erlang:monitor(process, Pid),
      ets:insert(OpenedMedia, #media_entry{name = Name, handler = Pid, ref = Ref}),
      ems_event:stream_created(Host, Name, Pid, Options),
      % ?D({"Registering", Name, Pid}),
      {reply, {ok, {Name, Pid}}, MediaProvider}
  end;

handle_call(host, _From, #media_provider{host = Host} = MediaProvider) ->
  {reply, Host, MediaProvider};

handle_call(entries, _From, #media_provider{opened_media = OpenedMedia} = MediaProvider) ->
  Info = [{Name, Pid} || #media_entry{name = Name, handler = Pid} <- ets:tab2list(OpenedMedia)],
  {reply, Info, MediaProvider};

handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.



find_in_cache(Name, #media_provider{opened_media = OpenedMedia}) ->
  case ets:lookup(OpenedMedia, Name) of
    [#media_entry{handler = Pid}] -> {ok, Pid};
    _ -> undefined
  end.
  
  
%%%%%%%%   Function in caller  
  

internal_open(Host, Name, Opts) ->
  Opts0 = lists:ukeysort(1, Opts),
  Opts1 = case proplists:get_value(type, Opts0) of
    undefined ->
      DetectedOpts = detect_type(Host, Name, Opts0),
      % ?D({"Detecting type", Host, Name, Opts0, DetectedOpts}),
      lists:ukeymerge(1, DetectedOpts, Opts0);
    _ ->
      Opts0
  end,
  Opts2 = lists:ukeymerge(1, Opts1, [{host, Host}, {name, Name}, {url, Name}]),
  case proplists:get_value(type, Opts2) of
    notfound ->
      {notfound, <<"No file ", Name/binary>>};
    undefined ->
      {notfound, <<"Error ", Name/binary>>};
    alias ->
      NewName = proplists:get_value(url, Opts2),
      ?D({"Aliasing", Name, NewName}),
      internal_open(Host, NewName, Opts1);
    _ ->
      start_new_media_entry(Host, Name, Opts2)
  end.

lists_except(Opts, []) ->
  Opts;

lists_except(Opts, [Key|Keys]) ->
  lists_except(lists:keydelete(Key,1,Opts), Keys).

start_new_media_entry(Host, Name, Opts) ->
  Type = proplists:get_value(type, Opts),
  URL = proplists:get_value(url, Opts, Name),

  Reply = case Type of
    remote ->
      Node = proplists:get_value(node, Opts),
      net_adm:ping(Node),
      ?D({open_remote,Node,Host,Name}),
      rpc:call(Node, ?MODULE, open, [Host, URL, lists_except(Opts, [node,type,url])], 5000);
    _ ->
      ems_sup:start_media(URL, Type, Opts)
  end,
      
  case Reply of
    {ok, Pid} ->
      case proplists:get_value(public, Opts, true) of
        true ->
          case register(Host, Name, Pid, Opts) of
            {ok, _} -> {ok, Pid};
            {error, {already_set, Name, OldPid}} ->
              %% This means, that several clients simultaneously requested one media and someone was first to register.
              %% Shutdown duplicate and use old.
              erlang:exit(Pid, shutdown),
              {ok, OldPid}
          end;
        _ ->
          ?D({"Skip registration of", Type, URL}),
          {ok, Pid}
      end;
    Else ->
      ?D({"Error opening", Type, Name, Else}),
      {notfound, <<"Failed to open ", Name/binary>>}
  end.

detect_type(Host, Name, Opts) when is_list(Name) ->
  detect_type(Host, list_to_binary(Name), Opts);
  
detect_type(Host, Name, Opts) ->
  Detectors = ems:get_var(detectors, Host, [rewrite, http, rtsp, ts_file, file, livestream]),
  detect_type(Detectors, Host, Name, Opts).
  
detect_type([], _, _, _) ->
  [{type, notfound}];
  
detect_type([Detector|Detectors], Host, Name, Opts) ->
  {Module,Function} = case Detector of
    {M,F} -> {M,F};
    F -> {media_detector,F}
  end,
  case Module:Function(Host, Name, Opts) of
    false -> detect_type(Detectors, Host, Name, Opts);
    Else -> Else
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
handle_cast({remove, Name}, #media_provider{opened_media = OpenedMedia} = MediaProvider) ->
  (catch ets:delete(OpenedMedia, Name)),
  {noreply, MediaProvider};

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
handle_info({'DOWN', _, process, MasterPid, _Reason}, #media_provider{master_pid = MasterPid, host = Host} = MediaProvider) ->
  ?D({"Master pid is down, new elections", Host}),
  global:register_name(media_provider_names:global_name(Host), self(), {?MODULE, resolve_global}),
  {noreply, MediaProvider#media_provider{master_pid = undefined}};

handle_info({'DOWN', _, process, Media, _Reason}, #media_provider{host = Host, opened_media = OpenedMedia} = MediaProvider) ->
  MS = ets:fun2ms(fun(#media_entry{handler = Pid, name = Name}) when Pid == Media -> Name end),
  case ets:select(OpenedMedia, MS) of
    [] -> 
      {noreply, MediaProvider};
    [Name] ->
      ets:delete(OpenedMedia, Name),
      case _Reason of
        normal -> ok;
        _ -> ?D({"Stream died", Media, Name, io_lib_pretty_limited:print(_Reason, 2000)})
      end,
      ems_event:stream_stopped(Host, Name, Media),
      {noreply, MediaProvider}
  end;

handle_info({wait_for, Pid}, MediaProvider) ->
  ?D({"Selected as slave provider, wait for", Pid}),
  erlang:monitor(process, Pid),
  {noreply, MediaProvider#media_provider{master_pid = Pid}};

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
