-module(http_file).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([open/2, pread/3, close/1]).


-record(http_file, {
  url,
  request_id,
  options
}).


open(URL, Options) ->
  {ok, Pid} = gen_server:start_link(?MODULE, [URL, Options], []),
  Pid.
  

init([URL, Options]) ->
  {ok, #http_file{url = URL, options = Options}}.
  

handle_call({pread, Offset, Limit}, _From, #http_file{} = File) ->
  {ok, Result} = internal_pread(File, Offset, Limit),
  {reply, {ok, Result}, File};
  
handle_call(Unknown, From, File) ->
  io:format("Unknown call: ~p from ~p (~p)~n", [Unknown, From, File]),
  {stop, {error, unknown_call, Unknown}, File}.
  

handle_cast(close, State) ->
  {stop, normal, State};
  
handle_cast(_, State) ->
  {noreply, State}.  


handle_info(Message, State) ->
  io:format("Some message: ~p~n", [Message]),
  {noreply, State}.
  
  
terminate(_Reason, _State) ->
  ok.
  
code_change(_Old, State, _Extra) ->
  {ok, State}.
  
%%%----------------------------
  
pread(File, Offset, Limit) ->
  gen_server:call(File, {pread, Offset, Limit}).

  
internal_pread(#http_file{url = URL}, Offset, Limit) ->
  Range = lists:flatten(io_lib:format("bytes=~p-~p", [Offset, Offset+Limit])),
  {ok, RequestID} = http:request(get, {URL, [{"Range", Range}]}, [], [{sync, false},{receiver, self()},{body_format,binary},{stream,self}]),
  wait_response(RequestID, <<>>, Limit).
  
  

wait_response(RequestID, Buffer, Limit) when size(Buffer) >= Limit ->
  {Result, _} = split_binary(Buffer, Limit),
  httpc:cancel_request(RequestID),
  receive
    {http, {RequestID, stream_end, _}} -> ok
  end,  
  {ok, Result};

wait_response(RequestID, Buffer, Limit) ->
  receive
    {http, {RequestID, stream, Bin}} ->
      wait_response(RequestID, <<Buffer/binary, Bin/binary>>, Limit);
    {http, {RequestID, stream_end, _}} ->
      Buffer;
    {http, {RequestID, stream_start, _}} ->
      wait_response(RequestID, Buffer, Limit)
  end.

close(File) ->
  gen_server:cast(File, close),
  ok.
  


