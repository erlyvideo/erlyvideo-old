%%
%% How to multipart upload to Amazon
%% http://docs.amazonwebservices.com/AmazonS3/latest/API/index.html?mpUploadInitiate.html
%%
-module(http_file).
-include("log.hrl").

% Application API
-export([start/2, stop/1, config_change/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([open/2, pread/3, close/1, pwrite/3]).

-export([cache_path/2, add_client/2, ems_client_load/0, ems_client_unload/0]).

-export([s3_sign/3, headers/2, head_request/1]).

-export([start/0, stop/0, start_link/2, reload/0, archive/0, rebuild/0]).

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
  clients = [],
  removing_streams = [],
  close_ref
}).


-record(stream, {
  pid,
  offset,
  size
}).


-define(COVER_LIMIT, 100000).
-define(DEFAULT_TIMEOUT, 30000).

start() ->
  application:start(http_file).

stop() ->
  application:stop(http_file),
  application:unload(http_file).
  
  
ems_client_load() ->
  [application:set_env(http_file, Key, Value) || {Key,Value} <- ems:get_var(http_file, [])],
  start().

ems_client_unload() ->
  stop().
  

reload() ->
  case lists:keyfind(http_file, 1, application:loaded_applications()) of
    false -> http_file:start();
    _ -> ok
  end,
  {ok, Modules} = application:get_key(http_file,modules),
  [begin
    code:soft_purge(Module),
    code:load_file(Module)
  end || Module <- Modules].


rebuild() ->
  make:all(),
  reload().

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
  ibrowse:start(),
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
  Timeout = if
    Limit < 100000 -> 10000;
    true -> Limit div 10 %% Counting on 100 kbyte/s at least
  end,
  gen_server:call(File, {pread, Offset, Limit}, Timeout). 

pwrite({http_file, _File, _Ref}, _Location, _Bytes) ->
  erlang:error(pwrite_not_supported).


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
  gen_server_ems:start_link(?MODULE, [URL, Options], []).

init([URL, Options]) when is_binary(URL) ->
  init([binary_to_list(URL), Options]);

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


handle_call({pread, Offset, Limit}, From, #http_file{streams = []} = File) ->
  handle_call({pread, Offset, Limit}, From, start_download(File));

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


handle_call({add_client, Opener}, _From, #http_file{clients = Clients, close_ref = CloseRef} = State) ->
  Ref = erlang:monitor(process, Opener),
  (catch timer:cancel(CloseRef)),
  {reply, {ok,Ref}, State#http_file{clients = [{Opener,Ref}|Clients]}};


handle_call({close, Client, Ref}, _From, #http_file{clients = Clients, file_cached = Cached} = State) ->
  case lists:keytake(Ref, 2, Clients) of
    {value, _Entry, []} when Cached == true ->
      ?D({"No clients left"}),
      {stop, normal, ok, State#http_file{clients = []}};
    % {value, _Entry, []} ->
    %   ?D({"No clients left, waiting"}),
    %   {stop, normal, ok, schedule_closing(State#http_file{clients = []})};
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


handle_info({ibrowse_async_headers, _Stream, _Code, _Headers}, State) ->
  % ?D(Resp),
  {noreply, State};


handle_info({ibrowse_async_response, Stream, Bin}, State) when is_binary(Bin) ->
  ibrowse:stream_next(Stream),
  schedule_closing(handle_incoming_data(Stream, Bin, State));

handle_info({ibrowse_async_response, Stream, {error, _Reason}}, State) ->
  handle_info({ibrowse_async_response_end, Stream}, State);

handle_info({ibrowse_async_response_end, Stream}, #http_file{streams = Streams, removing_streams = Removing} = State) ->
  case lists:keytake(Stream, #stream.pid, Streams) of
    {value, #stream{} = Entry, NewStreams} ->
      schedule_closing(State#http_file{streams = [Entry#stream{pid = undefined}| NewStreams]});
    _ ->
      case lists:member(Stream, Removing) of
        true ->
          schedule_closing(State#http_file{removing_streams = lists:delete(Stream, Removing)});
        false ->
          ?D({"Closed unknown stream", Stream, Streams}),
          {noreply, schedule_closing(State)}
      end
  end;
  
handle_info({'DOWN', _, process, Client, _Reason}, #http_file{clients = Clients} = State) ->
  case lists:keytake(Client, 1, Clients) of
    {value, _Entry, NewClients} ->
      ?D({"Client died", Client}),
      schedule_closing(State#http_file{clients = NewClients});
    _ ->
      ?D({unknown_died, Client, State}),
      {noreply, schedule_closing(State)}
  end;


handle_info(start_download, State) ->
  {noreply, start_download(State)};


handle_info(no_clients, #http_file{clients = []} = State) ->
  ?D({"Close http file due timeout and no clients"}),
  {stop, normal, State};

handle_info(no_clients, #http_file{} = State) ->
  {noreply, State#http_file{close_ref = undefined}};
  
handle_info(Message, State) ->
  ?D({"Some message:", Message}),
  {noreply, State}.


terminate(_Reason, #http_file{path = Path, temp_path = TempPath} = _State) ->
  (catch file:delete(Path)),
  (catch file:delete(TempPath)),
  ok.
  
code_change(_Old, State, _Extra) ->
  {ok, State}.
  
%%%----------------------------


start_download(#http_file{url = URL} = State) ->
  {ok, Headers} = head_request(URL),
  Length = list_to_integer(proplists:get_value("Content-Length", Headers)),
  schedule_request(State#http_file{size = Length}, {undefined, 0, Length}).
  

schedule_closing(#http_file{close_ref = undefined, clients = [], options = Options} = State) ->
  CloseRef = timer:send_after(proplists:get_value(timeout,Options,?DEFAULT_TIMEOUT), no_clients),
  {noreply, State#http_file{close_ref = CloseRef}};
  
schedule_closing(State) ->
  {noreply, State}.
  


s3_sign(get, Key, Path) ->
  s3_sign("GET", Key, Path);

s3_sign(head, Key, Path) ->
  s3_sign("HEAD", Key, Path);

s3_sign(Method, Key, Path) ->
  Date = httpd_util:rfc1123_date(),
  StringToSign = Method++"\n\n\n"++Date++"\n"++Path,
  Sign = binary_to_list(base64:encode(crypto:sha_mac(Key, StringToSign))),
  {Date, Sign}.


headers("http://s3.amazonaws.com"++Path, Method) ->
  Auth = case file:path_consult(["/etc/erlyvideo", "priv", "."], "aws.conf") of
    {ok, Env, _Path} ->
      Secret = proplists:get_value(secret, Env),
      Key = proplists:get_value(key, Env),
      {Date, Sign} = s3_sign(Method, Secret, Path),
      [{'Date', Date}, {'Authorization', "AWS "++Key++":"++Sign}];
    {error, _Error} -> []
  end,
  [{'Content-Type', ""}] ++ Auth;
  
headers(_URL, _Method) ->
  [].



head_request(URL) ->
  RequestHeaders = headers(URL, head),
  {ok, "200", Headers, _Body} = ibrowse:send_req(URL, RequestHeaders, head),
  {ok, Headers}.


  
schedule_request(#http_file{requests = Requests, streams = Streams, url = URL} = File, {_From, Offset, _Limit} = Request) ->
  NewStreams = case has_covering_stream(Streams, Request) of
    true -> Streams;
    false ->
      Range = lists:flatten(io_lib:format("bytes=~p-", [Offset])),
      Headers = headers(URL, get) ++ [{'Range', Range}],
      {ibrowse_req_id, Stream} = ibrowse:send_req(URL, Headers, get, [], [{stream_to,{self(),once}},{response_format,binary},{stream_chunk_size,8192}]),
      % ?D({"Starting new stream for", URL, Offset, _Limit, Stream}),
      lists:ukeymerge(#stream.pid, [#stream{pid = Stream, offset = Offset, size = 0}], Streams)
  end,
  File#http_file{streams = NewStreams, requests = lists:ukeymerge(1, [Request], Requests)}.
  

handle_incoming_data(Stream, Bin, #http_file{cache_file = Cache, streams = Streams, requests = Requests, removing_streams = Removing} = State) ->
  case lists:keyfind(Stream, #stream.pid, Streams) of
    #stream{pid = Stream, offset = BlockOffset, size = CurrentSize} = StreamInfo ->
      % ?D({"Got bin", BlockOffset, CurrentSize, Stream, size(Bin)}),
      ok = save_data(Cache, BlockOffset + CurrentSize, Bin),
      {NewStreams, ToRemove} = register_chunk(Streams, StreamInfo, size(Bin)),
      stop_finished_streams(ToRemove),
      {NewRequests, Replies} = satisfied_requests(Requests, NewStreams),
      reply_requests(Replies, Cache),
      
      FileCached = autorename_cached_file(NewStreams, State),
      % ?D({NewStreams, State#http_file.size}),
      State#http_file{streams = NewStreams, requests = NewRequests, file_cached = FileCached, removing_streams = lists:merge([Removing, ToRemove])};
    false ->
      case lists:member(Stream, Removing) of
        true ->
          ?D({"Got message from removing process", {bin, size(Bin), Stream}}),
          ibrowse:stream_close(Stream),
          State;
        false ->  
          ?D({"Got message from dead process", {bin, size(Bin), Stream}}),
          stop_finished_streams([Stream]),
          State
      end
  end.
  

save_data(Cache, Offset, Bin) ->
  file:pwrite(Cache, Offset, Bin).
  

register_chunk(Streams, #stream{pid = Stream}, ChunkSize) ->
  NewStreams1 = update_map(Streams, Stream, ChunkSize),
  {_NewStreams, _Removed} = glue_map(NewStreams1).
  
stop_finished_streams(Removed) ->
  lists:foreach(fun(OldStream) ->
    ok = ibrowse:stream_close(OldStream),
    flush_stream_data(OldStream)
  end, Removed).
  
flush_stream_data(Stream) ->
  receive
    {ibrowse_async_response, Stream, _Bin} -> flush_stream_data(Stream)
  after
    0 -> ok
  end.
    
  
  
reply_requests(Replies, Cache) ->
  % ?D({"Matching requests", NewRequests, Replies}),
  lists:foreach(fun
    ({undefined, _, _}) ->
      ok;
    ({From, Position, Limit}) ->
    {ok, Data} = file:pread(Cache, Position, Limit),
    % ?D({"Replying to", From, Position, Limit}),
    gen_server:reply(From, {ok, Data})
  end, Replies).


autorename_cached_file([#stream{offset = 0, size = FileSize}], #http_file{size = FileSize, temp_path = TempPath, path = Path}) ->
  ?D({"File is fully downloaded", TempPath, Path}),
  file:rename(TempPath, Path),
  true;
  
autorename_cached_file(_Streams, _File) ->  
  false.
  
  
  
has_covering_stream([#stream{offset = RequestOffset, size = Size} | _], {_Client, Offset, _Limit}) 
  when RequestOffset =< Offset andalso RequestOffset + Size >= Offset - ?COVER_LIMIT ->
  true;
has_covering_stream([_Stream|Streams], Request) -> has_covering_stream(Streams, Request);
has_covering_stream([], _Request) -> false.

  
fetch_cached_data(#http_file{cache_file = Cache}, Offset, Limit) ->
  file:pread(Cache, Offset, Limit).
  
update_map(Streams, Stream, ChunkSize) ->
  {value, #stream{size = CurrentSize} = Entry, Streams1} = lists:keytake(Stream, #stream.pid, Streams),
  NewEntry = Entry#stream{size = CurrentSize + ChunkSize},
  lists:keysort(#stream.offset, lists:ukeymerge(#stream.pid, [NewEntry], Streams1)).
  
  
glue_map(Streams) ->
  Sorted = lists:keysort(#stream.offset, Streams),
  glue_map(Sorted, [], []).

glue_map([Stream], NewStreams, Removed) ->
  {lists:keysort(#stream.offset, [Stream|NewStreams]), lists:dropwhile(fun(F) -> F == undefined end, lists:sort(Removed))};

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
intersect(#stream{pid = Key1, offset = Offset1, size = Size1}, #stream{offset = Offset2, size = Size2} = S) 
  when Offset1 + Size1 >= Offset2 andalso Offset1 + Size1 < Offset2 + Size2 ->
  {remove, S#stream{offset = Offset1, size = Offset2 + Size2 - Offset1}, Key1};


%%   Start1....Start2...End2...End1
intersect(#stream{offset = Offset1, size = Size1} = R1, #stream{pid = Key2, offset = Offset2, size = Size2}) 
  when Offset1 + Size1 >= Offset2 andalso Offset1 + Size1 >= Offset2 + Size2 ->
  {remove, R1, Key2}.


satisfied_requests(Requests, Streams) ->
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
  ?assertEqual([#stream{pid = a, offset = 0, size = 20}], update_map(Map1, a, 10)).

update_map2_test() ->
  Map1 = [#stream{pid = a, offset = 0, size = 10}, #stream{pid = b, offset = 15, size = 15}],
  ?assertEqual([#stream{pid = a, offset = 0, size = 20},#stream{pid = b, offset = 15, size = 15}], update_map(Map1, a, 10)).
  

is_data_cached_test() ->
  Map1 = [#stream{pid = a, offset = 0, size = 10}, #stream{pid = b, offset = 15, size = 15}],
  ?assertEqual(true, is_data_cached(Map1, 0, 10)),
  ?assertEqual(true, is_data_cached(Map1, 17, 5)),
  ?assertEqual(false, is_data_cached(Map1, 0, 25)),
  ?assertEqual(false, is_data_cached(Map1, 9, 2)),
  ?assertEqual(true, is_data_cached(Map1, 9, 1)).
 
 
 
intersect_test() ->
  ?assertEqual({remove, #stream{pid = b, offset = 0, size = 30}, a}, intersect(#stream{pid=a, offset=0, size=20}, #stream{pid=b, offset=20, size=10})),
  ?assertEqual({leave, #stream{pid=a, offset=0, size=18}, #stream{pid=b, offset=20, size=10}}, intersect(#stream{pid=a, offset=0, size=18}, #stream{pid=b, offset=20, size=10})),
  ?assertEqual({remove, #stream{pid=a, offset=0, size=45}, b}, intersect(#stream{pid=a, offset=0, size=45}, #stream{pid=b, offset=20, size=10})).

glue_map1_test() ->
  Map = [#stream{pid=a, offset=0, size=20}, #stream{pid=b, offset=20, size=10}],
  ?assertEqual({[#stream{pid=b, offset=0, size=30}], [a]}, glue_map(Map)).


glue_map2_test() ->
  Map = [#stream{pid=a, offset=0, size=25}, #stream{pid=b, offset=20, size=10}],
  ?assertEqual({[#stream{pid=b, offset=0, size=30}], [a]}, glue_map(Map)).


glue_map3_test() ->
  Map = [#stream{pid=a, offset=0, size=45}, #stream{pid=b,offset=20, size=10}, #stream{pid=c, offset=32, size=6}],
  ?assertEqual({[#stream{pid=a, offset=0, size=45}], [b,c]}, glue_map(Map)).

glue_map4_test() ->
  Map = [#stream{pid=a, offset=0, size=31}, #stream{pid=b, offset=20, size=10}, #stream{pid=c, offset=35, size=100}],
  ?assertEqual({[#stream{pid=a, offset=0, size=31}, #stream{pid=c, offset=35, size=100}], [b]}, glue_map(Map)).










 
  

