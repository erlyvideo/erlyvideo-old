-module(http_file_request).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/3]).
-behaviour(gen_server).


-record(http_file_request, {
  file,
  url,
  offset,
  request_id
}).


start(File, URL, Offset) ->
  gen_server:start_link(?MODULE, [File, URL, Offset], []).
  % gen_server:call(Pid, {pread, Offset, Limit}).

init([File, URL, Offset]) ->
  self() ! start,
  {ok, #http_file_request{file = File, url = URL, offset = Offset}}.
  
  
handle_call(_Unknown, _From, #http_file_request{} = File) ->
  {reply, unknown, File}.


handle_cast(_Cast, State) ->
  {noreply, State}.  


handle_info(start, #http_file_request{offset = Offset, url = URL} = File) ->
  Range = lists:flatten(io_lib:format("bytes=~p-", [Offset])),
  {_, _, Host, Port, Path, Query} = http_uri:parse(URL),
  Request = "GET "++Path++"?"++Query++" HTTP/1.1\r\nHost: "++Host++"\r\nRange: "++Range++"\r\n\r\n",
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, once}, {packet, http}]),
  gen_tcp:send(Socket, Request),
  ?D({"Started request", URL, Offset, self()}),
  {noreply, File#http_file_request{request_id = Socket}};


  
handle_info({http, Socket, {http_response, _, _Code, _Message}}, File) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, File};

handle_info({http, Socket, {http_header, _, _Key, _, _Value}}, File) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, File};

handle_info({http, Socket, http_eoh}, File) ->
  inet:setopts(Socket, [{active, once}, {packet, raw}]),
  {noreply, File};
  

handle_info({tcp, Socket, Bin}, #http_file_request{offset = Offset, file = Origin} = File) ->
  inet:setopts(Socket, [{active, once}]),
  Origin ! {bin, Bin, Offset, self()},
  {noreply, File#http_file_request{offset = Offset + size(Bin)}};


handle_info(stop, File) ->
  % ?D({"Stopped", File}),
  {stop, normal, File};

handle_info(Message, State) ->
  io:format("Some message: ~p~n", [Message]),
  {noreply, State}.


terminate(_Reason, #http_file_request{request_id = RequestID} = _State) ->
  (catch httpc:cancel_request(RequestID)),
  ok.

code_change(_Old, State, _Extra) ->
  {ok, State}.
  
