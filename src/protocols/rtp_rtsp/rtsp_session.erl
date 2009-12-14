-module(rtsp_session).
-author('max@maxidoors.ru').

-behaviour(gen_fsm).
-include("../../../include/ems.hrl").

-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([binarize_header/1, handle_request/1]).

%% FSM States
-export([
  'WAIT_FOR_SOCKET'/2,
	'WAIT_FOR_REQUEST'/2,
	'WAIT_FOR_BINARY'/2,
	'WAIT_FOR_HEADERS'/2,
  'WAIT_FOR_DATA'/2]).

-record(rtsp_session, {
  socket,
  host,
  addr,
  port,
  request_re,
  rtsp_re,
  url_re,
  content_length,
  bytes_read = 0,
  request,
  headers,
  body,
  session_id,
  sequence,
  streams,
  buffer = <<>>,
  media
}).


start_link() ->
  % All possible headers:
  'ANNOUNCE', 'PLAY', 'SETUP', 'OPTIONS', 'DESCRIBE', 'PAUSE', 'RECORD',
  'TEARDOWN', 'GET_PARAMETER', 'SET_PARAMETER',
  'Accept', 'Accept-Encoding', 'Accept-Language', 'Allow', 'Authorization', 'Bandwidth',
  'Blocksize', 'Cache-Control', 'ClientChallenge', 'Conference', 'Connection', 'Content-Base', 'Content-Encoding',
  'Content-Language', 'Content-Length', 'Content-Location', 'Content-Type', 'Cseq',
  'Date', 'Expires', 'From', 'Host', 'If-Match', 'If-Modiﬁed-Since', 'Last-Modiﬁed', 
  'Location', 'Proxy-Authenticate', 'Proxy-Require', 'Public', 'Range', 
  'Referer', 'Retry-After', 'Require', 'RTP-Info', 'Scale', 'Speed', 'Server',
  'Session', 'Timestamp', 'Transport', 'Unsupported', 'User-Agent', 'Vary', 'Via',
  'WWW-Authenticate',
  gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit, true),
  random:seed(now()),
  {ok, RequestRe} = re:compile("([^ ]+) ([^ ]+) (\\w+)/([\\d]+\\.[\\d]+)"),
  {ok, RtspRe} = re:compile("(\\w)=(.*)\\r\\n$"),
  {ok, UrlRe} = re:compile("rtsp://([^/]+)/(.*)$"),
  {ok, 'WAIT_FOR_SOCKET', #rtsp_session{request_re = RequestRe, rtsp_re = RtspRe, url_re = UrlRe}}.



%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) ->
  inet:setopts(Socket, [{active, once}, {packet, line}, binary]),
  {ok, {IP, Port}} = inet:peername(Socket),
  {next_state, 'WAIT_FOR_REQUEST', State#rtsp_session{socket=Socket, addr=IP, port = Port}, ?TIMEOUT}.

'WAIT_FOR_REQUEST'(timeout, State) ->
  {stop, normal, State};

'WAIT_FOR_REQUEST'({data, <<$$, _/binary>> = RTP}, State) ->
  'WAIT_FOR_BINARY'({data, RTP}, State);

'WAIT_FOR_REQUEST'({data, <<>>}, State) ->
  {next_state, 'WAIT_FOR_REQUEST', State};
  
'WAIT_FOR_REQUEST'({data, Request}, #rtsp_session{socket = Socket, url_re = UrlRe, request_re = RequestRe} = State) ->
  {match, [_, Method, URL, <<"RTSP">>, <<"1.0">>]} = re:run(Request, RequestRe, [{capture, all, binary}]),
  MethodName = binary_to_existing_atom(Method, utf8),
  {match, [_, Host, _Path]} = re:run(URL, UrlRe, [{capture, all, binary}]),
  io:format("[RTSP] ~s ~s RTSP/1.0~n", [Method, URL]),
  inet:setopts(Socket, [{packet, line}, {active, once}]),
  {next_state, 'WAIT_FOR_HEADERS', State#rtsp_session{request = [MethodName, URL], headers = [], host = ems:host(Host)}, ?TIMEOUT}.


'WAIT_FOR_BINARY'({data, Bin}, #rtsp_session{buffer = Buffer, socket = Socket} = State) ->
  case <<Buffer/binary, Bin/binary>> of
    <<$$, ChannelId, Length:16, RTP:Length/binary, Request/binary>> ->
      ?D({"RTP tunneled", ChannelId, size(RTP)}),
      'WAIT_FOR_REQUEST'({data, Request}, State#rtsp_session{buffer = <<>>});
    NewBuf ->
      inet:setopts(Socket, [{packet, line}, {active, once}]),
      {next_state, 'WAIT_FOR_BINARY', State#rtsp_session{buffer = NewBuf}}
  end.



'WAIT_FOR_HEADERS'({header, 'Content-Length', LengthBin}, #rtsp_session{socket = Socket} = State) ->
  Length = list_to_integer(binary_to_list(LengthBin)),
  io:format("[RTSP] Content-Length: ~s~n", [LengthBin]),
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_HEADERS', State#rtsp_session{content_length = Length}, ?TIMEOUT};

'WAIT_FOR_HEADERS'({header, 'Cseq', Sequence}, #rtsp_session{socket = Socket} = State) ->
  io:format("[RTSP] Cseq: ~s~n", [Sequence]),
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_HEADERS', State#rtsp_session{sequence = Sequence}, ?TIMEOUT};

'WAIT_FOR_HEADERS'(timeout, State) ->
  {stop, normal, State};

'WAIT_FOR_HEADERS'({header, Name, Value}, #rtsp_session{socket = Socket, headers = Headers} = State) ->
  io:format("[RTSP] ~s: ~s~n", [Name, Value]),
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_HEADERS', State#rtsp_session{headers = [{Name, Value} | Headers]}, ?TIMEOUT};

'WAIT_FOR_HEADERS'(end_of_headers, #rtsp_session{socket = Socket, content_length = undefined} = State) ->
  State1 = handle_request(State),
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_REQUEST', State1};
  

'WAIT_FOR_HEADERS'(end_of_headers, #rtsp_session{socket = Socket} = State) ->
  inet:setopts(Socket, [{packet, line}, {active, once}]),
  {next_state, 'WAIT_FOR_DATA', State#rtsp_session{body = []}, ?TIMEOUT}.


'WAIT_FOR_DATA'(timeout, State) ->
  ?D({"Client timeout"}),
  {stop, normal, State};

'WAIT_FOR_DATA'({data, Message}, #rtsp_session{bytes_read = BytesRead, content_length = ContentLength, socket = Socket} = State) when BytesRead + size(Message) == ContentLength ->
  State1 = decode_line(Message, State),
  State2 = handle_request(State1),
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_REQUEST', State2};

'WAIT_FOR_DATA'({data, Message}, #rtsp_session{bytes_read = BytesRead, socket = Socket} = State) ->
  NewBytesRead = BytesRead + size(Message),
  State1 = decode_line(Message, State),
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_DATA', State1#rtsp_session{bytes_read = NewBytesRead}};


'WAIT_FOR_DATA'(Message, State) ->
  ?D({"Message", Message}),
  {next_state, 'WAIT_FOR_DATA', State}.


% split_params(String) ->
%   lists:map(fun(Opt) -> case string:tokens(Opt, "=") of
%     [Key] -> Key;
%     [Key, Value] -> {Key, Value}
%   end end, string:tokens(binary_to_list(String), ";")).


handle_request(#rtsp_session{request = [_Method, _URL], body = Body} = State) ->
  State1 = run_request(State#rtsp_session{body = lists:reverse(Body)}),
  State1#rtsp_session{request = undefined, content_length = undefined, headers = [], body = [], bytes_read = 0}.
  

run_request(#rtsp_session{request = ['ANNOUNCE', _URL], host = Host, body = Body} = State) ->
  Path = path(State),
  Media = media_provider:open(Host, Path, live),
  Streams = parse_announce(Body),
  ?D(Streams),
  reply(State#rtsp_session{media = Media, streams = Streams}, "200 OK", []);

run_request(#rtsp_session{request = ['OPTIONS', _URL]} = State) ->
  reply(State, "200 OK", [{'Public', "SETUP, TEARDOWN, PLAY, PAUSE, RECORD, OPTIONS, DESCRIBE"}]);

run_request(#rtsp_session{request = ['RECORD', _URL]} = State) ->
  reply(State, "200 OK", []);

run_request(#rtsp_session{request = ['PAUSE', _URL]} = State) ->
  reply(State, "200 OK", []);

run_request(#rtsp_session{request = ['TEARDOWN', _URL]} = State) ->
  reply(State, "200 OK", []);

run_request(#rtsp_session{request = ['SETUP', URL], headers = Headers, streams = Streams, media = Media} = State) ->
  {ok, Re} = re:compile("rtsp://([^/]*)/(.*)/trackid=(\\d+)"),
  {match, [_, HostPort, _Path, TrackIdS]} = re:run(URL, Re, [{capture, all, list}]),
  HostName = hd(string:tokens(HostPort, ":")),
  {ok, {hostent, _, _, inet, _, [HostAddr | _]}} = inet:gethostbyname(HostName),
  {IP1, IP2, IP3, IP4} = HostAddr,
  TrackId = list_to_integer(TrackIdS),
  Transport = proplists:get_value('Transport', Headers),
  
  Stream = hd(lists:filter(fun(S) ->
    lists:member({track_id, TrackId}, S)
  end, Streams)),
  
  TransportOpts = string:tokens(binary_to_list(Transport), ";"),
  Proto = case hd(TransportOpts) of
    "RTP/AVP" -> udp;
    "RTP/AVP/TCP" -> tcp
  end,
  
  {ok, _Listener, {RTP, RTCP}} = ems_sup:start_rtp_server(Media, proplists:get_value(type, Stream), [{proto, Proto} | Stream]),
  Date = httpd_util:rfc1123_date(),
  TransportReply = io_lib:format("~s;server_port=~p-~p;source=~p.~p.~p.~p",[Transport, RTP, RTCP, IP1, IP2, IP3, IP4]),
  % TransportReply = "RTP/AVP;unicast;client_port=5432-5433;server_port=6256-6257",
  ReplyHeaders = [{"Transport", TransportReply},  {'Date', Date}, {'Expires', Date}, {'Cache-Control', "no-cache"}],
  reply(State#rtsp_session{session_id = 42}, "200 OK", ReplyHeaders);

run_request(#rtsp_session{request = [_Method, _URL]} = State) ->
  reply(State, "200 OK", []).
  

decode_line(Message, #rtsp_session{rtsp_re = RtspRe, body = Body} = State) ->
  {match, [_, Key, Value]} = re:run(Message, RtspRe, [{capture, all, binary}]),
  io:format("[RTSP]  ~s: ~s~n", [Key, Value]),
  State#rtsp_session{body = [{binary_to_existing_atom(Key, utf8), Value} | Body]}.


reply(#rtsp_session{socket = Socket, sequence = Sequence, session_id = SessionId} = State, Code, Headers) ->
  Headers1 = [{'Cseq', Sequence} | Headers],
  Headers2 = case SessionId of
    undefined -> Headers1;
    _ -> [{'Session', SessionId} | Headers1]
  end,
  ReplyList = lists:map(fun binarize_header/1, Headers2),
  Reply = iolist_to_binary(["RTSP/1.0 ", Code, <<"\r\n">>, ReplyList, <<"\r\n">>]),
  io:format("[RTSP] ~s", [Reply]),
  gen_tcp:send(Socket, Reply),
  State.
  
binarize_header({Key, Value}) when is_atom(Key) ->
  binarize_header({atom_to_binary(Key, latin1), Value});

binarize_header({Key, Value}) when is_list(Key) ->
  binarize_header({list_to_binary(Key), Value});

binarize_header({Key, Value}) when is_integer(Value) ->
  binarize_header({Key, integer_to_list(Value)});
  
binarize_header({Key, Value}) ->
  [Key, <<": ">>, Value, <<"\r\n">>];

binarize_header([Key, Value]) ->
  [Key, <<" ">>, Value, <<"\r\n">>].
  
  
path(#rtsp_session{url_re = UrlRe, request = [_, URL]}) ->
  {match, [_, _Host, Path]} = re:run(URL, UrlRe, [{capture, all, binary}]),
  Path.
  
parse_announce(Announce) -> 
  sdp:decode(Announce).
  

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
  {stop, {StateName, undefined_event, Event}, StateData}.


%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
   io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, got_sync_request]),
  {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, _Socket, Bin}, 'WAIT_FOR_HEADERS' = StateName, State) ->
  case erlang:decode_packet(httph_bin, <<Bin/binary, "pad">>, []) of
    {ok, {http_header, _, Name, _, Value}, <<"pad">>} -> 
      Key = case Name of
        _ when is_binary(Name) -> binary_to_existing_atom(Name, utf8);
        _ when is_atom(Name) -> Name
      end,
      ?MODULE:StateName({header, Key, Value}, State);
    {ok, http_eoh, <<"pad">>} -> ?MODULE:StateName(end_of_headers, State)
  end;

handle_info({tcp, _Socket, Bin}, StateName, State) ->
  ?MODULE:StateName({data, Bin}, State);

% handle_info({http, _Socket, {http_header, _, Name, _, Value}}, StateName, State) ->
%   ?MODULE:StateName({header, Name, Value}, State);
% 
% handle_info({http, _Socket, http_eoh}, StateName, State) ->
%   ?MODULE:StateName(end_of_headers, State);


handle_info({tcp_closed, Socket}, _StateName, #rtsp_session{socket=Socket, addr=Addr, port = Port} = StateData) ->
  error_logger:info_msg("~p Client ~p:~p disconnected.\n", [self(), Addr, Port]),
  {stop, normal, StateData};

handle_info(timeout, StateName, StateData) ->
  ?D({"Stop due timeout", StateName, StateData}),
  {stop, normal, StateData};


handle_info(_Info, StateName, StateData) ->
  ?D({"Some info handled", _Info, StateName, StateData}),
  {noreply, StateName, StateData}.


%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #rtsp_session{socket=Socket}) ->
  ?D({"RTSP stopping", _StateName, _Reason}),
  (catch gen_tcp:close(Socket)),
  ok.


%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

