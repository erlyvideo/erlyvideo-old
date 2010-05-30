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
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------

-module(media_provider).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/ems.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).

%% External API
-export([start_link/1, create/3, open/2, open/3, play/3, entries/1, remove/2, find/2, find_or_open/2, find_or_open/3, register/3]).
-export([info/1, info/2, detect_type/3]). % just for getStreamLength

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([init_names/0, name/1, resolve_global/3]).

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
  Pid = find_or_open(Host, Name),
  {ok, Pid}.

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
global_name(Host) ->
  media_provider_names:name(Host).

%% @hidden
start_link(Host) ->
  gen_server:start_link({local, name(Host)}, ?MODULE, [Host], []).

%% @hidden
find_provider(Host) ->
  % {global, global_name(Host)}.
  name(Host).

%% @hidden  
resolve_global(Name, Pid1, Pid2) ->
  ?D({"Resolving global clash for", Name, Pid1, Pid2}),
  Entries = gen_server:call(Pid2, entries),
  % Host = gen_server:call(Pid2, host),
  % supervisor:terminate_child(ems_sup, name(Host)),
  % supervisor:delete_child(ems_sup, name(Host)),
  gen_server:call(Pid1, {import, Entries}),
  Pid2 ! {wait_for, Pid1},
  Pid1.

%%-------------------------------------------------------------------------
%% @spec create(Host, Name, Options) -> Pid::pid()
%% @doc Create stream. Must be called by process, that wants to send 
%% video frames to this stream.
%% Usually called as ``media_provider:create(Host, Name, [{type,live}])''
%% @end
%%-------------------------------------------------------------------------
create(Host, Name, Options) ->
  ?D({"Create", Name, Options}),
  Pid = open(Host, Name, Options),
  ems_media:set_source(Pid, self()),
  Pid.

open(Host, Name) when is_list(Name)->
  open(Host, list_to_binary(Name));

open(Host, Name) ->
  open(Host, Name, []).

%%-------------------------------------------------------------------------
%% @spec open(Host, Name, Options) -> Pid::pid()
%% @doc Open or start stream.
%% @end
%%-------------------------------------------------------------------------
open(Host, Name, Opts) when is_list(Name)->
  open(Host, list_to_binary(Name), Opts);

open(Host, Name, Opts) ->
  gen_server:call(find_provider(Host), {open, Name, Opts}, infinity).

find(Host, Name) when is_list(Name)->
  find(Host, list_to_binary(Name));

find(Host, Name) ->
  gen_server:call(find_provider(Host), {find, Name}, infinity).

register(Host, Name, Pid) ->
  gen_server:call(find_provider(Host), {register, Name, Pid}).

entries(Host) ->
  gen_server:call(find_provider(Host), entries).
  
remove(Host, Name) ->
  gen_server:cast(find_provider(Host), {remove, Name}).

info(Host, Name) ->
  case find_or_open(Host, Name) of
    Media when is_pid(Media) -> media_provider:info(Media);
    _ -> []
  end.
  

info(undefined) ->
  [];
  
info(Media) ->
  ems_media:info(Media).
  

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


% Plays media named Name
% Required options:
%   stream_id: for RTMP, FLV stream id
%
% Valid options:
%   consumer: pid of media consumer
%   client_buffer: client buffer size
%
play(Host, Name, Options) ->
  case find_or_open(Host, Name, Options) of
    {notfound, Reason} -> 
      {notfound, Reason};
    Stream when is_pid(Stream) ->
      ems_media:play(Stream, lists:ukeymerge(1, lists:ukeysort(1, Options), [{stream_id,1}])),
      {ok, Stream}
  end.

find_or_open(Host, Name) ->
  find_or_open(Host, Name, []).
  
find_or_open(Host, Name, Options) ->
  case find(Host, Name) of
    undefined -> open(Host, Name, Options);
    MediaEntry -> MediaEntry
  end.


  

init([Host]) ->
  % error_logger:info_msg("Starting with file directory ~p~n", [Path]),
  OpenedMedia = ets:new(opened_media, [set, private, {keypos, #media_entry.name}]),
  % global:register_name(global_name(Host), self(), {?MODULE, resolve_global}),
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
handle_call({import, Entries}, _From, #media_provider{opened_media = OpenedMedia} = MediaProvider) ->
  ?D({"Importing", Entries}),
  ets:insert(OpenedMedia, Entries),
  lists:foreach(fun(#media_entry{handler = Pid}) ->
    erlang:monitor(process, Pid)
  end, Entries),
  {reply, ok, MediaProvider};

handle_call({find, Name}, _From, MediaProvider) ->
  {reply, find_in_cache(Name, MediaProvider), MediaProvider};
  
handle_call({open, Name, Opts}, {_Opener, _Ref}, MediaProvider) ->
  case find_in_cache(Name, MediaProvider) of
    undefined ->
      {reply, internal_open(Name, Opts, MediaProvider), MediaProvider};
    Player ->
      {reply, Player, MediaProvider}
  end;

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
    
handle_call({register, Name, Pid}, _From, #media_provider{host = Host, opened_media = OpenedMedia} = MediaProvider) ->
  case find_in_cache(Name, MediaProvider) of
    undefined ->
      Ref = erlang:monitor(process, Pid),
      ets:insert(OpenedMedia, #media_entry{name = Name, handler = Pid, ref = Ref}),
      ems_event:stream_started(Host, Name, Pid, []),
      ?D({"Registering", Name, Pid}),
      {reply, {ok, {Name, Pid}}, MediaProvider};
    OldPid ->
      {reply, {error, {already_set, Name, OldPid}}, MediaProvider}
  end;

handle_call(host, _From, #media_provider{host = Host} = MediaProvider) ->
  {reply, Host, MediaProvider};

handle_call(entries, _From, #media_provider{opened_media = OpenedMedia} = MediaProvider) ->
  % Entries = lists:map(
  %   fun([Name, Handler]) -> 
  %     Clients = try gen_server:call(Handler, clients, 1000) of
  %       C when is_list(C) -> C
  %     catch
  %       exit:{timeout, _} -> [];
  %       Class:Else ->
  %         ?D({"Media",Name,"error",Class,Else}),
  %         []
  %     end,
  %     {Name, Clients}
  %   end,
  % ets:match(OpenedMedia, {'_', '$1', '$2'})),
  Info = [{Name, Pid, (catch ems_media:status(Pid))} || #media_entry{name = Name, handler = Pid} <- ets:tab2list(OpenedMedia)],
  {reply, Info, MediaProvider};

handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.


find_in_cache(Name, #media_provider{opened_media = OpenedMedia}) ->
  case ets:lookup(OpenedMedia, Name) of
    [#media_entry{handler = Pid}] -> Pid;
    _ -> undefined
  end.


internal_open(Name, Opts, #media_provider{host = Host} = MediaProvider) ->
  Opts0 = lists:ukeysort(1, Opts),
  Opts1 = case proplists:get_value(type, Opts0) of
    undefined ->
      DetectedOpts = detect_type(Host, Name, Opts0),
      ?D({"Detecting type", Host, Name, Opts0, DetectedOpts}),
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
    _ ->
      open_media_entry(Name, MediaProvider, Opts2)
  end.


open_media_entry(Name, #media_provider{host = Host, opened_media = OpenedMedia} = MediaProvider, Opts) ->
  Type = proplists:get_value(type, Opts),
  URL = proplists:get_value(url, Opts, Name),
  Public = proplists:get_value(public, Opts, true),
  case find_in_cache(Name, MediaProvider) of
    undefined ->
      case ems_sup:start_media(URL, Type, Opts) of
        {ok, Pid} ->
          case Public of
            true ->
              Ref = erlang:monitor(process, Pid),
              ets:insert(OpenedMedia, #media_entry{name = Name, handler = Pid, ref = Ref}),
              ems_event:stream_started(Host, Name, Pid, [{type,Type}|Opts]);
            _ ->
              ?D({"Skip registration of", Type, URL}),
              ok
          end,
          Pid;
        _ ->
          ?D({"Error opening", Type, Name}),
          {notfound, <<"Failed to open ", Name/binary>>}
      end;
    MediaEntry ->
      MediaEntry
  end.
  
detect_type(Host, Name, Opts) ->
  detect_rewrite(Host, Name, Opts).


detect_rewrite(Host, Name, Opts) ->
  Rewrite = ems:get_var(rewrite, Host, []),
  case lists:keyfind(binary_to_list(Name), 1, Rewrite) of
    false -> detect_mpegts(Host, Name, Opts);
    {_NameS, Type, URL} -> [{type, Type}, {url, URL}];
    {_NameS, Type, URL, Options} -> [{type, Type}, {url, URL} | Options]
  end.
  
detect_mpegts(Host, Name, Opts) ->
  Urls = ems:get_var(mpegts, Host, []),
  case proplists:get_value(binary_to_list(Name), Urls) of
    undefined -> detect_rtsp(Host, Name, Opts);
    URL -> 
      ems_log:error(Host, "Stop using {mpegts, []} style in config, switch to {rewrite, []}"),
      [{type, http}, {url, URL}]
  end.
  
detect_rtsp(Host, Name, Opts) ->
  Urls = ems:get_var(rtsp, Host, []),
  case proplists:get_value(binary_to_list(Name), Urls) of
    undefined -> detect_http(Host, Name, Opts);
    URL -> 
      ems_log:error(Host, "Stop using {rtsp, []} style in config, switch to {rewrite, []}"),
      [{type, rtsp}, {url, URL}]
  end.

detect_http(Host, Name, Opts) ->
  {ok, Re} = re:compile("http://(.*)"),
  case re:run(Name, Re) of
    {match, _Captured} -> [{type, http}];
    _ -> detect_ts_file(Host, Name, Opts)
  end.

detect_ts_file(Host, Name, Opts) ->
  case {check_path(Host, Name), mpegts_file_media:can_open_file(Name)} of
    {true, true} -> [{type, mpegts_file},{life_timeout,0}];
    _ -> detect_file(Host, Name, Opts)
  end.

detect_file(Host, Name, Opts) ->
  case check_path(Host, Name) of
    true -> [{type, file}, {url, Name}];
    _ ->
      case check_path(Host, <<Name/binary, ".flv">>) of
        true -> [{type, file}, {url, <<Name/binary, ".flv">>}];
        _ -> detect_prefixed_file(Host, Name, Opts)
      end
  end.

detect_prefixed_file(Host, <<"flv:", Name/binary>>, _Opts) ->
  case check_path(Host, Name) of
    true -> [{type, file}, {url, Name}];
    _ -> 
      case check_path(Host, <<Name/binary, ".flv">>) of
        true ->
          [{type, file}, {url, <<Name/binary, ".flv">>}];
        false ->
          [{type, notfound}]
      end
  end;

detect_prefixed_file(Host, <<"mp4:", Name/binary>>, _Opts) ->
  case check_path(Host, Name) of
    true -> 
      ?D({"File found", Name}),
      [{type, file}, {url, Name}];
    _ -> 
      case check_path(Host, <<Name/binary, ".mp4">>) of
        true ->
          [{type, file}, {url, <<Name/binary, ".mp4">>}];
        false ->
          [{type, notfound}]
      end
  end;
  
detect_prefixed_file(Host, Name, Opts) ->
  detect_livestream(Host, Name, Opts).
  
detect_livestream(_Host, Name, Opts) ->
  case proplists:get_value(wait, Opts) of
    undefined ->
      [{type, notfound}];
    _ ->
      [{type, live}, {url, Name}]
  end.



check_path(Host, Name) when is_binary(Name) ->
  check_path(Host, binary_to_list(Name));

check_path(Host, Name) ->
  case file_media:file_dir(Host) of
    undefined -> false;
    Dir -> filelib:is_regular(filename:join([Dir, Name]))
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
      ?D({"Stream died", Media, Name, _Reason}),
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
