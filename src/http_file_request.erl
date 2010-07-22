-module(http_file_request).
-include("log.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/3]).
-behaviour(gen_server).


-record(http_file_request, {
  file,
  url,
  offset,
  socket
}).


start_link(File, URL, Offset) ->
  gen_server:start_link(?MODULE, [File, URL, Offset], []).
  % gen_server:call(Pid, {pread, Offset, Limit}).

init([File, URL, Offset]) ->
  self() ! start,
  erlang:monitor(process, File),
  {ok, #http_file_request{file = File, url = URL, offset = Offset}}.
  
  
handle_call(_Unknown, _From, #http_file_request{} = File) ->
  {reply, unknown, File}.


handle_cast(_Cast, State) ->
  {noreply, State}.  


handle_info(start, #http_file_request{offset = Offset, url = URL} = File) ->
  Range = lists:flatten(io_lib:format("bytes=~p-", [Offset])),
  {_, _, Host, Port, Path, Query} = http_uri:parse(URL),
  Request = "GET "++Path++"?"++Query++" HTTP/1.1\r\nHost: "++Host++"\r\nConnection: close\r\nRange: "++Range++"\r\n\r\n",
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, once}, {packet, http}]),
  gen_tcp:send(Socket, Request),
  ?D({"Started request", URL, Offset, self()}),
  {noreply, File#http_file_request{socket = Socket}};


  
handle_info({http, Socket, {http_response, _, _Code, _Message}}, File) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, File};

handle_info({http, Socket, {http_header, _, _Key, _, _Value}}, File) ->
  ?D({self(), _Key, _Value}),
  inet:setopts(Socket, [{active, once}]),
  {noreply, File};

handle_info({http, Socket, http_eoh}, File) ->
  inet:setopts(Socket, [{active, once}, {packet, raw}]),
  {noreply, File};
  

handle_info({tcp, Socket, Bin}, #http_file_request{offset = Offset, file = Origin} = File) ->
  inet:setopts(Socket, [{active, once}]),
  Origin ! {bin, Bin, Offset, self()},
  receive
    stop -> ?D({self(), stopping}), {stop, normal, File}
  after
    0 -> {noreply, File#http_file_request{offset = Offset + size(Bin)}}
  end;

handle_info({tcp_closed, _Socket}, Request) ->
  {stop, normal, Request};

handle_info({'DOWN', _, process, _File, _Reason}, Request) ->
  {stop, normal, Request};

handle_info(stop, File) ->
  ?D({"Stopped", File}),
  {stop, normal, File};

handle_info(Message, State) ->
  ?D({"Some message:", Message}),
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.

code_change(_Old, State, _Extra) ->
  {ok, State}.
  
