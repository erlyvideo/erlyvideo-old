-module(mpegts_media).
-author('Max Lapshin <max@maxidoors.ru>').
-export([start_link/3]).
-behaviour(gen_server).

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-include("mpegts.hrl").
-include_lib("erlyvideo/include/video_frame.hrl").

% ems_sup:start_ts_lander("http://localhost:8080").




%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(ts_lander, {
  socket,
  url,
  audio_config = undefined,
  video_config = undefined,
  clients = [],
  mpegts_reader,
  byte_counter = 0
}).

% {ok, Socket} = gen_tcp:connect("ya.ru", 80, [binary, {packet, http_bin}, {active, false}], 1000),
% gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
% {ok, Reply} = gen_tcp:recv(Socket, 0, 1000),
% Reply.

% {ok, Pid1} = ems_sup:start_ts_lander("http://localhost:8080").

start_link(URL, Type, Opts) ->
  gen_server:start_link(?MODULE, [URL, Type, Opts], []).

init([undefined, _Type, _]) ->
  {ok, Reader} = mpegts_reader:start_link(self()),
  {ok, #ts_lander{mpegts_reader = Reader}};
  

init([URL, Type, Opts]) when is_binary(URL)->
  init([binary_to_list(URL), Type, Opts]);

init([URL, mpeg_ts_passive, _Opts]) ->
  {ok, Reader} = mpegts_reader:start_link(self()),
  {ok, #ts_lander{url = URL, mpegts_reader = Reader}};
  
init([URL, mpeg_ts, _Opts]) ->
  {_, _, Host, Port, Path, Query} = http_uri:parse(URL),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, http_bin}, {active, false}], 1000),
  gen_tcp:send(Socket, "GET "++Path++"?"++Query++" HTTP/1.0\r\n\r\n"),
  ok = inet:setopts(Socket, [{active, once}]),
  {ok, Reader} = mpegts_reader:start_link(self()),
  {ok, #ts_lander{socket = Socket, url = URL, mpegts_reader = Reader}}.
  
  % io:format("HTTP Request ~p~n", [RequestId]),
  % {ok, #ts_lander{request_id = RequestId, url = URL, pids = [#stream{pid = 0, handler = pat}]}}.
    


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

handle_call({set_socket, Socket}, _From, TSLander) ->
  inet:setopts(Socket, [{active, once}, {packet, raw}]),
  ?D({"MPEG TS received socket"}),
  {reply, ok, TSLander#ts_lander{socket = Socket}};

handle_call({subscribe, Client}, _From, #ts_lander{clients = Clients} = MediaInfo) ->
  erlang:monitor(process, Client),
  {reply, {ok, stream}, MediaInfo#ts_lander{clients = [Client|Clients]}};

handle_call(length, _From, MediaInfo) ->
  {reply, 0, MediaInfo};

handle_call(clients, _From, #ts_lander{clients = Clients} = TSLander) ->
  Entries = lists:map(fun(Pid) -> file_play:client(Pid) end, Clients),
  {reply, Entries, TSLander};

handle_call({set_owner, _}, _From, TSLander) ->
  {reply, ok, TSLander};



handle_call(Request, _From, State) ->
  ?D({"Undefined call", Request, _From}),
  {stop, {unknown_call, Request}, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
  ?D({"Undefined cast", _Msg}),
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

handle_info({http, Socket, {http_response, _Version, 200, _Reply}}, TSLander) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, TSLander};

handle_info({http, Socket, {http_header, _, _Header, _, _Value}}, TSLander) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, TSLander};


handle_info({http, Socket, http_eoh}, TSLander) ->
  inet:setopts(Socket, [{active, once}, {packet, raw}]),
  {noreply, TSLander};

handle_info(#video_frame{decoder_config = true, type = audio} = Frame, TSLander) ->
  {noreply, send_frame(Frame, TSLander#ts_lander{audio_config = Frame})};

handle_info(#video_frame{body = Config, decoder_config = true, type = video} = Frame, TSLander) ->
  Lander = send_frame(Frame, TSLander#ts_lander{video_config = Frame}),
  send_frame(h264:metadata(Config), Lander),
  {noreply, Lander};

handle_info(#video_frame{} = Frame, TSLander) ->
  {noreply, send_frame(Frame, TSLander)};


handle_info({'DOWN', _Ref, process, Client, _Reason}, #ts_lander{clients = Clients} = TSLander) ->
  {noreply, TSLander#ts_lander{clients = lists:delete(Client, Clients)}};

handle_info({tcp, Socket, Bin}, #ts_lander{mpegts_reader = Reader, byte_counter = Counter} = TSLander) ->
  inet:setopts(Socket, [{active, once}]),
  Reader ! {data, Bin},
  {noreply, TSLander#ts_lander{byte_counter = Counter + size(Bin)}};

handle_info({tcp_closed, Socket}, #ts_lander{socket = Socket} = TSLander) ->
  {stop, normal, TSLander#ts_lander{socket = undefined}};
  
handle_info(stop, #ts_lander{socket = Socket} = TSLander) ->
  gen_tcp:close(Socket),
  {stop, normal, TSLander#ts_lander{socket = undefined}};

handle_info(_Info, State) ->
  ?D({"Undefined info", _Info}),
  {noreply, State}.


send_frame(Frame, #ts_lander{clients = Clients} = TSLander) ->
  lists:foreach(fun(Client) -> Client ! Frame end, Clients),
  TSLander.



%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _TSLander) ->
  ?D({"TS Lander terminating", _Reason}),
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
