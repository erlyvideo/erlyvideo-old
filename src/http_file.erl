-module(http_file).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([open/2, pread/3, close/1]).

-behaviour(gen_server).

-record(http_file, {
  url,
  cache_file,
  request_id,
  options,
  streams = [],
  requests = [],
  size
}).


open(URL, Options) ->
  {ok, Pid} = gen_server:start_link(?MODULE, [URL, Options], []),
  Pid.
  

init([URL, Options]) ->
  CacheName = proplists:get_value(cache_file, Options),
  {ok, CacheFile} = file:open(CacheName, [write, read, binary]),
  self() ! start_download,
  {ok, #http_file{url = URL, cache_file = CacheFile, options = Options}}.
  

handle_call({pread, Offset, Limit}, From, #http_file{streams = Streams} = File) ->
  case is_data_cached(Streams, Offset, Limit) of
    true ->
      ?D({"Data ok"}),
      {reply, fetch_cached_data(File, Offset, Limit), File};
    false ->
      File1 = schedule_request(File, {From, Offset, Limit}),
      {noreply, File1}
  end;    
  
handle_call(Unknown, From, File) ->
  io:format("Unknown call: ~p from ~p (~p)~n", [Unknown, From, File]),
  {stop, {error, unknown_call, Unknown}, File}.
  

handle_cast(close, State) ->
  {stop, normal, State};
  
handle_cast(_, State) ->
  {noreply, State}.  


handle_info(start_download, #http_file{url = URL} = State) ->
  % {ok, {_, Headers, _}} = httpc:request(head, {URL, []}, [], []),
  % Length = proplists:get_value("content-length", Headers),
  Length = "0",
  {ok, FirstRequest} = http_file_request:start(self(), URL, 0),
  erlang:monitor(process, FirstRequest),
  {noreply, State#http_file{streams = [{FirstRequest, 0, 0}], size = list_to_integer(Length)}};

handle_info({bin, Bin, Offset, Request}, #http_file{cache_file = Cache, streams = Streams, requests = Requests} = State) ->
  case lists:keyfind(Request, 1, Streams) of
    false -> 
      ?D({"Got message from dead process", Request}),
      {noreply, State};
    _ -> 
      ok = file:pwrite(Cache, Offset, Bin),
      % ?D({"Got bin", Offset, size(Bin), Request, Streams}),
      NewStreams1 = update_map(Streams, Request, Offset, size(Bin)),
      {NewStreams2, Removed} = glue_map(NewStreams1),
      % ?D({"Removing streams", NewStreams2, Removed}),
      lists:foreach(fun(Stream) ->
        Stream ! stop
      end, Removed),
      {NewRequests, Replies} = match_requests(Requests, NewStreams2),
      % ?D({"Matching requests", NewRequests, Replies}),
      lists:foreach(fun({From, Position, Limit}) ->
        {ok, Data} = file:pread(Cache, Position, Limit),
        ?D({"Replying to", From, Offset, Limit}),
        gen_server:reply(From, {ok, Data})
      end, Replies),
      
      NewStreams = NewStreams2,
      {noreply, State#http_file{streams = NewStreams, requests = NewRequests}}
  end;
  
handle_info({'DOWN', _,process, Stream, _Reason}, #http_file{streams = Streams} = State) ->
  case lists:keytake(Stream, 1, Streams) of
    {value, Stream, NewStreams} ->
      {noreply, State#http_file{streams = NewStreams}};
    _ ->
      {noreply, State}
  end;
  

handle_info(Message, State) ->
  io:format("Some message: ~p~n", [Message]),
  {noreply, State}.
  
  
terminate(_Reason, _State) ->
  ok.
  
code_change(_Old, State, _Extra) ->
  {ok, State}.
  
%%%----------------------------
  
  
schedule_request(#http_file{requests = Requests, streams = Streams, url = URL} = File, {_From, Offset, Limit} = Request) ->
  {ok, Stream} = http_file_request:start(self(), URL, Offset),
  erlang:monitor(process, Stream),
  ?D({"Starting new stream for", URL, Offset, Limit, Stream}),
  File#http_file{streams = lists:ukeymerge(1, [{Stream, Offset, 0}], Streams), requests = lists:ukeymerge(1, [Request], Requests)}.
  
  
fetch_cached_data(#http_file{cache_file = Cache}, Offset, Limit) ->
  file:pread(Cache, Offset, Limit).
  
update_map(Streams, Stream, Offset, Size) ->
  {value, Entry, Streams1} = lists:keytake(Stream, 1, Streams),
  {Stream, OldOffset, OldSize} = Entry,
  Offset = OldOffset + OldSize,
  NewEntry = {Stream, OldOffset, OldSize + Size},
  lists:ukeymerge(1, [NewEntry], Streams1).
  
  
glue_map(Streams) ->
  Sorted = lists:keysort(2, Streams),
  glue_map(Sorted, [], []).

glue_map([Stream], NewStreams, Removed) ->
  {lists:keysort(1, [Stream|NewStreams]), lists:sort(Removed)};

glue_map([Stream1, Stream2 | Streams], NewStreams, Removed) ->
  case intersect(Stream1, Stream2) of
    {leave, R1, R2} ->
      glue_map([R2|Streams], [R1 | NewStreams], Removed);
    {remove, R1, Key} ->
      glue_map([R1|Streams], NewStreams, [Key|Removed])
  end.

%%   Start1....End1   Start2...End2  
intersect({_Key1, Offset1, Size1} = R1, {_Key2, Offset2, _Size2} = R2) 
                                       when Offset1 + Size1 < Offset2 ->
  {leave, R1, R2};

%%   Start1....Start2...End1...End2
intersect({Key1, Offset1, Size1}, {Key2, Offset2, Size2}) 
  when Offset1 + Size1 >= Offset2 andalso Offset1 + Size1 < Offset2 + Size2 ->
  {remove, {Key2, Offset1, Offset2 + Size2 - Offset1}, Key1};


%%   Start1....Start2...End2...End1
intersect({_Key1, Offset1, Size1} = R1, {Key2, Offset2, Size2}) 
  when Offset1 + Size1 >= Offset2 andalso Offset1 + Size1 >= Offset2 + Size2 ->
  {remove, R1, Key2}.


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
  
is_data_cached([{_Request, CurrentOffset, CurrentSize} | _], Offset, Size) 
  when Offset >= CurrentOffset andalso Offset + Size =< CurrentOffset + CurrentSize ->
  true;

is_data_cached([_ | Streams], Offset, Size) ->
  is_data_cached(Streams, Offset, Size).
  
  
pread(File, Offset, Limit) ->
  ?D({"Requesting", Offset, Limit}),
  gen_server:call(File, {pread, Offset, Limit}, infinity).

  

close(File) ->
  gen_server:cast(File, close),
  ok.
  

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").

update_map1_test() ->
  Map1 = [{a, 0, 10}],
  ?assertEqual([{a, 0, 20}], update_map(Map1, a, 10, 10)).

update_map2_test() ->
  Map1 = [{a, 0, 10}, {b, 15, 15}],
  ?assertEqual([{a, 0, 20},{b, 15, 15}], update_map(Map1, a, 10, 10)).
  

is_data_cached_test() ->
  Map1 = [{a, 0, 10}, {b, 15, 15}],
  ?assertEqual(true, is_data_cached(Map1, 0, 10)),
  ?assertEqual(true, is_data_cached(Map1, 17, 5)),
  ?assertEqual(false, is_data_cached(Map1, 0, 25)),
  ?assertEqual(false, is_data_cached(Map1, 9, 2)),
  ?assertEqual(true, is_data_cached(Map1, 9, 1)).
 
 
 
intersect_test() ->
  ?assertEqual({remove, {b, 0, 30}, a}, intersect({a, 0, 20}, {b, 20, 10})),
  ?assertEqual({leave, {a, 0, 18}, {b, 20, 10}}, intersect({a, 0, 18}, {b, 20, 10})),
  ?assertEqual({remove, {a, 0, 45}, b}, intersect({a, 0, 45}, {b, 20, 10})).

glue_map1_test() ->
  Map = [{a, 0, 20}, {b, 20, 10}],
  ?assertEqual({[{b, 0, 30}], [a]}, glue_map(Map)).


glue_map2_test() ->
  Map = [{a, 0, 25}, {b, 20, 10}],
  ?assertEqual({[{b, 0, 30}], [a]}, glue_map(Map)).


glue_map3_test() ->
  Map = [{a, 0, 45}, {b, 20, 10}, {c, 32, 6}],
  ?assertEqual({[{a, 0, 45}], [b,c]}, glue_map(Map)).

glue_map4_test() ->
  Map = [{a, 0, 31}, {b, 20, 10}, {c, 35, 100}],
  ?assertEqual({[{a, 0, 31}, {c, 35, 100}], [b]}, glue_map(Map)).










 
  

