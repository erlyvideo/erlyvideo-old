-module(http_file).
-include("log.hrl").

% Application API
-export([start/2, stop/1, config_change/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([open/2, pread/3, close/1]).

-export([cache_path/2, add_client/2, ems_client_load/0, ems_client_unload/0]).

-export([start/0, stop/0, start_link/2, reload/0, archive/0]).

-behaviour(gen_server).

-record(http_file, {
  url,
  cache_file,
  temp_path,
  path,
  request_id,
  options,
  streams = [],
  requests = [],
  size,
  file_cached = false,
  clients = []
}).


-record(stream, {
  pid,
  offset,
  size,
  ref
}).


-define(COVER_LIMIT, 100000).

start() ->
  application:start(http_file).

stop() ->
  application:stop(http_file),
  application:unload(http_file).
  
  
ems_client_load() ->
  start().

ems_client_unload() ->
  stop().
  

reload() ->
  {ok, Modules} = application:get_key(http_file,modules),
  [begin
    code:soft_purge(Module),
    code:load_file(Module)
  end || Module <- Modules].



archive() ->
  make:all([load]),
  application:load(http_file),
  {ok, Version} = application:get_key(http_file,vsn),
  zip:create("http_file-"++Version++".ez", ["http_file/ebin"], [{cwd, "../"},{compress,all},{uncompress,[".beam",".app"]},verbose]).


  
%%--------------------------------------------------------------------
%% @spec (Type::any(), Args::list()) -> any()
%% @doc Starts RTMP library
%% @end 
%%--------------------------------------------------------------------

start(_Type, _Args) -> 
  http_file_sup:start_link().



%%--------------------------------------------------------------------
%% @spec (Any::any()) -> ok()
%% @doc Stop RTMP library
%% @end 
%%--------------------------------------------------------------------
stop(_S) ->
  ok.


%%--------------------------------------------------------------------
%% @spec (Any::any(),Any::any(),Any::any()) -> any()
%% @doc Reload ErlMedia Application config
%% @end 
%%--------------------------------------------------------------------
config_change(_Changed, _New, _Remove) ->
  ok.
  

%%%%%%%%% File API  %%%%%%%%%%%%

autostart() ->
  case lists:keyfind(http_file,1, application:loaded_applications()) of
    false -> http_file:start();
    _ -> ok
  end.


open(URL, Options) ->
  % {ok, CachePath} = application:get_env(http_file, cache_path),
  % http_file_sup:start_file(URL, [{cache_path,CachePath}|Options]).
  autostart(),
  http_file_tracker:open(URL, Options).
  
  
add_client(File, Opener) ->
  gen_server:call(File, {add_client, Opener}).

pread({cached,File}, Offset, Limit) ->
  file:pread(File, Offset, Limit);

pread({http_file,File,_Ref}, Offset, Limit) ->
  % ?D({"Requesting", Offset, Limit}),
  gen_server:call(File, {pread, Offset, Limit}, infinity).

close({cached,File}) ->
  file:close(File);
  
close({http_file,File,Ref}) ->
  % ?D({"closing file", File,Ref}),
  gen_server:call(File, {close, self(), Ref}).

%%%%%%%%%% Gen server API %%%%%%%%

cache_path(CachePath, URL) ->
  "http://" ++ ClearedName = URL,
  filename:join(CachePath, ClearedName).


start_link(URL, Options) ->
  gen_server:start_link(?MODULE, [URL, Options], []).

init([URL, Options]) ->
  Path = cache_path(proplists:get_value(cache_path, Options), URL),
  TempPath = Path ++ ".tmp",
  filelib:ensure_dir(Path),
  {ok, CacheFile} = file:open(TempPath, [write, read, binary]),
  % ?D({"Storing",URL,to,Path, Options}),
  self() ! start_download,
  {ok, #http_file{url = URL, cache_file = CacheFile, path = Path, temp_path = TempPath, options = Options}}.
  

handle_call({pread, Offset, _Limit}, _From, #http_file{size = Size} = File)
  when Offset >= Size ->
  {reply, eof, File};


handle_call({pread, Offset, Limit}, From, #http_file{size = Size} = File) when Offset + Limit > Size ->
  handle_call({pread, Offset, Size - Offset}, From, File);

handle_call({pread, Offset, Limit}, From, #http_file{streams = Streams} = File) ->
  case is_data_cached(Streams, Offset, Limit) of
    true ->
      % ?D({"Data ok"}),
      {reply, fetch_cached_data(File, Offset, Limit), File};
    false ->
      % ?D({"No data, waiting"}),
      File1 = schedule_request(File, {From, Offset, Limit}),
      {noreply, File1}
  end;    


handle_call({add_client, Opener}, _From, #http_file{clients = Clients} = State) ->
  Ref = erlang:monitor(process, Opener),
  {reply, {ok,Ref}, State#http_file{clients = [{Opener,Ref}|Clients]}};


handle_call({close, Client, Ref}, _From, #http_file{clients = Clients, file_cached = Cached} = State) ->
  case lists:keytake(Ref, 2, Clients) of
    {value, _Entry, []} when Cached == true ->
      % ?D({"No clients left"}),
      {stop, normal, ok, State#http_file{clients = []}};
    {value, {Client,Ref}, NewClients} ->
      % ?D({"closing for", Client, NewClients}),
      erlang:demonitor(Ref, [flush]),
      {reply, ok, State#http_file{clients = NewClients}};
    _ ->  
    {reply, ok, State}
  end;





  
handle_call(Unknown, _From, File) ->
  % ?D({"Unknown call:", Unknown, From, File}),
  {stop, {error, unknown_call, Unknown}, File}.
  

handle_cast(_, State) ->
  {noreply, State}.  



handle_info({bin, Bin, Offset, Stream}, #http_file{cache_file = Cache, streams = Streams, requests = Requests, size = Size} = State) ->
  case lists:keyfind(Stream, #stream.pid, Streams) of
    false ->
      % ?D({"Got message from dead process", {bin, size(Bin), Offset, Stream}}),
      Stream ! stop,
      {noreply, State};
    _ -> 
      ok = file:pwrite(Cache, Offset, Bin),
      % ?D({"Got bin", Offset, size(Bin), Request, Streams}),
      NewStreams1 = update_map(Streams, Stream, Offset, size(Bin)),
      {NewStreams2, Removed} = glue_map(NewStreams1),
      % ?D({"Removing streams", NewStreams2, Removed}),
      lists:foreach(fun({OldStream, Ref}) ->
        erlang:demonitor(Ref, [flush]),
        OldStream ! stop
      end, Removed),
      {NewRequests, Replies} = match_requests(Requests, NewStreams2),
      % ?D({"Matching requests", NewRequests, Replies}),
      lists:foreach(fun({From, Position, Limit}) ->
        {ok, Data} = file:pread(Cache, Position, Limit),
        % ?D({"Replying to", From, Offset, Limit}),
        gen_server:reply(From, {ok, Data})
      end, Replies),

      NewStreams = NewStreams2,

      FileCached = case NewStreams of
        [#stream{offset = 0, size = Size}] ->
          % ?D({"File is fully downloaded", State#http_file.temp_path, State#http_file.path, State#http_file.clients}),
          file:rename(State#http_file.temp_path, State#http_file.path),
          true;
        _ ->
          false
      end,
      State1 = State#http_file{streams = NewStreams, requests = NewRequests, file_cached = FileCached},
      case {FileCached, State#http_file.clients} of
        {true, []} ->
          % ?D({"All is ready, exit"}),
          {stop, normal, State1};
        _ ->
          {noreply, State1}
      end
  end;


handle_info(start_download, #http_file{url = URL} = State) ->
  {ok, Headers} = head_request(URL),
  % ?D(Headers),
  Length = proplists:get_value('Content-Length', Headers),
  {ok, FirstRequest} = http_file_sup:start_request(self(), URL, 0),
  Ref = erlang:monitor(process, FirstRequest),
  {noreply, State#http_file{streams = [#stream{pid = FirstRequest, offset = 0, size = 0, ref = Ref}], size = Length}};

handle_info({'DOWN', _, process, Stream, _Reason}, #http_file{streams = Streams, clients = Clients} = State) ->
  case lists:keytake(Stream, #stream.pid, Streams) of
    {value, _Entry, NewStreams} ->
      stop_if_no_clients(State#http_file{streams = NewStreams});
    _ ->
      case lists:keytake(Stream, 1, Clients) of
        {value, _Entry, NewClients} ->
          stop_if_no_clients(State#http_file{clients = NewClients});
        _ ->
          ?D({unknown_died, Stream, State}),
          {noreply, State}
      end
  end;
  

handle_info(Message, State) ->
  ?D({"Some message:", Message}),
  {noreply, State}.


stop_if_no_clients(#http_file{clients = []} = State) ->
  {stop, normal, State};

stop_if_no_clients(State) ->
  {noreply, State}.
  
terminate(_Reason, #http_file{path = Path, temp_path = TempPath} = _State) ->
  (catch file:delete(Path)),
  (catch file:delete(TempPath)),
  ok.
  
code_change(_Old, State, _Extra) ->
  {ok, State}.
  
%%%----------------------------


head_request(URL) ->
  {http, _UserInfo, Host, Port, _Path, _Query} = http_uri2:parse(URL),
  {ok, Re} = re:compile("http://[^\\/]+/(.+)"),
  {match, [_, Path]} = re:run(URL, Re, [{capture, all, list}]),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active,false}, {packet,http}]),
  Req = lists:flatten(io_lib:format("HEAD /~s HTTP/1.1\r\nHost: ~s\r\n~s\r\n", [Path, Host, auth(URL, "HEAD")])),
  gen_tcp:send(Socket, Req),
  {ok, {http_response, _HTTPVersion, 200, _Status}} = gen_tcp:recv(Socket, 0),
  Headers = read_headers(Socket, []),
  gen_tcp:close(Socket),
  {ok, Headers}.
  
  
read_headers(Sock, Headers) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, {http_header, _, 'Content-Length' = Header, _, Value}} ->
      read_headers(Sock, [{Header,list_to_integer(Value)}|Headers]);
    {ok, {http_header, _, Header, _, Value}} ->
      read_headers(Sock, [{Header,Value}|Headers]);
    {ok, http_eoh} ->
      inet:setopts(Sock, [{packet,raw}]),
      Headers
  end.
  
auth(_URL, _Verb) ->
  "".
  
schedule_request(#http_file{requests = Requests, streams = Streams, url = URL} = File, {_From, Offset, _Limit} = Request) ->
  NewStreams = case has_covering_stream(Streams, Request) of
    true -> Streams;
    false ->
      {ok, Stream} = http_file_sup:start_request(self(), URL, Offset),
      Ref = erlang:monitor(process, Stream),
      % ?D({"Starting new stream for", URL, Offset, Limit, Stream}),
      lists:ukeymerge(#stream.pid, [#stream{pid = Stream, offset = Offset, size = 0, ref = Ref}], Streams)
  end,
  File#http_file{streams = NewStreams, requests = lists:ukeymerge(1, [Request], Requests)}.
  
  
has_covering_stream([#stream{offset = RequestOffset, size = Size} | _], {_Client, Offset, _Limit}) 
  when RequestOffset =< Offset andalso RequestOffset + Size >= Offset - ?COVER_LIMIT ->
  true;
has_covering_stream([_Stream|Streams], Request) -> has_covering_stream(Streams, Request);
has_covering_stream([], _Request) -> false.

  
fetch_cached_data(#http_file{cache_file = Cache}, Offset, Limit) ->
  file:pread(Cache, Offset, Limit).
  
update_map(Streams, Stream, Offset, Size) ->
  {value, #stream{offset = OldOffset, size = OldSize} = Entry, Streams1} = lists:keytake(Stream, #stream.pid, Streams),
  Offset = OldOffset + OldSize, % This is assertion, that we haven't missed not a packet
  NewEntry = Entry#stream{size = OldSize + Size},
  lists:keysort(#stream.offset, lists:ukeymerge(#stream.pid, [NewEntry], Streams1)).
  
  
glue_map(Streams) ->
  Sorted = lists:keysort(#stream.offset, Streams),
  glue_map(Sorted, [], []).

glue_map([Stream], NewStreams, Removed) ->
  {lists:keysort(#stream.offset, [Stream|NewStreams]), lists:keysort(1, Removed)};

glue_map([Stream1, Stream2 | Streams], NewStreams, Removed) ->
  case intersect(Stream1, Stream2) of
    {leave, R1, R2} ->
      glue_map([R2|Streams], [R1 | NewStreams], Removed);
    {remove, R1, Key} ->
      glue_map([R1|Streams], NewStreams, [Key|Removed])
  end.

%%   Start1....End1   Start2...End2  
intersect(#stream{offset = Offset1, size = Size1} = R1, #stream{offset = Offset2} = R2) 
                                       when Offset1 + Size1 < Offset2 ->
  {leave, R1, R2};

%%   Start1....Start2...End1...End2
intersect(#stream{pid = Key1, ref = Ref, offset = Offset1, size = Size1}, #stream{offset = Offset2, size = Size2} = S) 
  when Offset1 + Size1 >= Offset2 andalso Offset1 + Size1 < Offset2 + Size2 ->
  {remove, S#stream{offset = Offset1, size = Offset2 + Size2 - Offset1}, {Key1,Ref}};


%%   Start1....Start2...End2...End1
intersect(#stream{offset = Offset1, size = Size1} = R1, #stream{pid = Key2, offset = Offset2, size = Size2, ref = Ref}) 
  when Offset1 + Size1 >= Offset2 andalso Offset1 + Size1 >= Offset2 + Size2 ->
  {remove, R1, {Key2,Ref}}.


match_requests(Requests, Streams) ->
  match_requests(Requests, Streams, [], []).

match_requests([], _Streams, NewRequests, Replies) ->
  {NewRequests, Replies};
  
match_requests([{From, Offset, Size}|Requests], Streams, NewRequests, Replies) ->
  case is_data_cached(Streams, Offset, Size) of
    true ->
      match_requests(Requests, Streams, NewRequests, [{From, Offset, Size}|Replies]);
    false ->
      match_requests(Requests, Streams, [{From, Offset, Size}|NewRequests], Replies)
  end.


is_data_cached([], _Offset, _Size) ->
  false;
  
is_data_cached([#stream{offset = CurrentOffset, size = CurrentSize} | _], Offset, Size) 
  when Offset >= CurrentOffset andalso Offset + Size =< CurrentOffset + CurrentSize ->
  true;

is_data_cached([_ | Streams], Offset, Size) ->
  is_data_cached(Streams, Offset, Size).
  

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").

update_map1_test() ->
  Map1 = [#stream{pid = a, offset = 0, size = 10}],
  ?assertEqual([#stream{pid = a, offset = 0, size = 20}], update_map(Map1, a, 10, 10)).

update_map2_test() ->
  Map1 = [#stream{pid = a, offset = 0, size = 10}, #stream{pid = b, offset = 15, size = 15}],
  ?assertEqual([#stream{pid = a, offset = 0, size = 20},#stream{pid = b, offset = 15, size = 15}], update_map(Map1, a, 10, 10)).
  

is_data_cached_test() ->
  Map1 = [#stream{pid = a, offset = 0, size = 10}, #stream{pid = b, offset = 15, size = 15}],
  ?assertEqual(true, is_data_cached(Map1, 0, 10)),
  ?assertEqual(true, is_data_cached(Map1, 17, 5)),
  ?assertEqual(false, is_data_cached(Map1, 0, 25)),
  ?assertEqual(false, is_data_cached(Map1, 9, 2)),
  ?assertEqual(true, is_data_cached(Map1, 9, 1)).
 
 
 
intersect_test() ->
  ?assertEqual({remove, #stream{pid = b, offset = 0, size = 30}, {a,ar}}, intersect(#stream{pid=a, offset=0, size=20,ref=ar}, #stream{pid=b, offset=20, size=10})),
  ?assertEqual({leave, #stream{pid=a, offset=0, size=18}, #stream{pid=b, offset=20, size=10}}, intersect(#stream{pid=a, offset=0, size=18}, #stream{pid=b, offset=20, size=10})),
  ?assertEqual({remove, #stream{pid=a, offset=0, size=45}, {b,br}}, intersect(#stream{pid=a, offset=0, size=45}, #stream{pid=b, offset=20, size=10, ref=br})).

glue_map1_test() ->
  Map = [#stream{pid=a, offset=0, size=20, ref=ar}, #stream{pid=b, offset=20, size=10}],
  ?assertEqual({[#stream{pid=b, offset=0, size=30}], [{a,ar}]}, glue_map(Map)).


glue_map2_test() ->
  Map = [#stream{pid=a, offset=0, size=25, ref=ar}, #stream{pid=b, offset=20, size=10}],
  ?assertEqual({[#stream{pid=b, offset=0, size=30}], [{a,ar}]}, glue_map(Map)).


glue_map3_test() ->
  Map = [#stream{pid=a, offset=0, size=45}, #stream{pid=b, ref=br,offset=20, size=10}, #stream{pid=c,ref=cr, offset=32, size=6}],
  ?assertEqual({[#stream{pid=a, offset=0, size=45}], [{b,br},{c,cr}]}, glue_map(Map)).

glue_map4_test() ->
  Map = [#stream{pid=a, offset=0, size=31}, #stream{pid=b, ref=br, offset=20, size=10}, #stream{pid=c, offset=35, size=100}],
  ?assertEqual({[#stream{pid=a, offset=0, size=31}, #stream{pid=c, offset=35, size=100}], [{b,br}]}, glue_map(Map)).










 
  

