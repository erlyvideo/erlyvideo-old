-module(mpegts_media).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(ems_media).


-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-define(MAX_RESTART, 10).
-define(TIMEOUT_RESTART, 1000).

-export([init/1, handle_frame/2, handle_control/2, handle_info/2]).

-record(mpegts, {
  socket,
  options,
  url,
  demuxer,
  restart_count
}).

init(Options) ->
  URL = proplists:get_value(url, Options),
  Socket = case proplists:get_value(make_request, Options, true) of
    true -> connect_http(URL);
    _ -> undefined
  end,
  {ok, Reader} = ems_sup:start_mpegts_reader(self()),
  {ok, #mpegts{socket = Socket, demuxer = Reader, options = Options, url = URL}}.
  
connect_http(URL) ->
  {_, _, Host, Port, Path, Query} = http_uri:parse(URL),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, line}, {active, false}], 4000),
  ?D({Host, Path, Query, "GET "++Path++" HTTP/1.1\r\nHost: "++Host++":"++integer_to_list(Port)++"\r\nAccept: */*\r\n\r\n"}),
  gen_tcp:send(Socket, "GET "++Path++" HTTP/1.1\r\nHost: "++Host++":"++integer_to_list(Port)++"\r\nAccept: */*\r\n\r\n"),
  ok = inet:setopts(Socket, [{active, once}]),
  Socket.

handle_control(_Control, State) ->
  {ok, State}.
  
handle_frame(Frame, State) ->
  {ok, Frame, State}.



handle_info({http, Socket, {http_response, _Version, 200, _Reply}}, State) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, State#mpegts{restart_count = undefined}};

handle_info({http, Socket, {http_header, _, _Header, _, _Value}}, State) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, State};

handle_info({http, Socket, http_eoh}, State) ->
  inet:setopts(Socket, [{active, once}, {packet, raw}]),
  {noreply, State};


handle_info({tcp, Socket, Bin}, #mpegts{demuxer = Reader} = State) when Reader =/= undefined ->
  inet:setopts(Socket, [{active, once}]),
  Reader ! {data, Bin},
  {noreply, State};

handle_info({tcp_closed, Socket}, #mpegts{restart_count = undefined} = State) ->
  handle_info({tcp_closed, Socket}, State#mpegts{restart_count = 0});

handle_info({tcp_closed, _Socket}, #mpegts{url = URL, restart_count = Count} = State) ->
  if
    Count > ?MAX_RESTART ->
      {stop, normal, State};
    true ->  
      % FIXME
      % ems_event:stream_source_lost(Media#media_info.host, Media#media_info.name, self()),
      ?D({"Disconnected MPEG-TS socket in mode", Count}),
      timer:sleep(100),
      Socket = connect_http(URL),
      {noreply, State#mpegts{socket = Socket, restart_count = Count + 1}}
  end;

handle_info(Msg, State) ->
  {stop, {unhandled, Msg}, State}.

