-module(http_file).

-export([open/2, init/2, pread/3, close/1, loop/1]).


-record(http_file, {
  url,
  request_id,
  options,
  lala
}).


open(URL, Options) ->
  spawn_link(?MODULE, init, [URL, Options]).
  

init(URL, Options) ->
  ?MODULE:loop(#http_file{url = URL, options = Options}).
  

loop(#http_file{} = File) ->
  receive
    {pread, Offset, Limit, Sender, Ref} ->
      {ok, Result} = internal_pread(File, Offset, Limit),
      Sender ! {ok, Result, Ref},
      ?MODULE:loop(File);
    Else ->
      io:format("Unknown message: ~p~n", [Else]),
      ok
  end.
  
  
pread(File, Offset, Limit) ->
  Ref = erlang:make_ref(),
  File ! {pread, Offset, Limit, self(), Ref},
  receive
    {ok, Result, Ref} -> {ok, Result}
  end.
  
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

close(_) ->
  ok.
  


