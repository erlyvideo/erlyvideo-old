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
	'WAIT_FOR_HEADERS'/2,
  'WAIT_FOR_DATA'/2]).

-record(rtsp_session, {
  socket,
  addr,
  port,
  request_re,
  rtsp_re,
  content_length,
  bytes_read = 0,
  request,
  headers,
  body,
  session_id,
  sequence
}).


start_link() ->
  % All possible headers:
  'ANNOUNCE', 'PLAY', 'SETUP', 'OPTIONS', 'DESCRIBE', 'PAUSE', 'RECORD',
  'TEARDOWN', 'GET_PARAMETER', 'SET_PARAMETER',
  'Accept', 'Accept-Encoding', 'Accept-Language', 'Allow', 'Authorization', 'Bandwidth',
  'Blocksize', 'Cache-Control', 'Conference', 'Connection', 'Content-Base', 'Content-Encoding',
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
  {ok, 'WAIT_FOR_SOCKET', #rtsp_session{request_re = RequestRe, rtsp_re = RtspRe}}.



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

'WAIT_FOR_REQUEST'({data, Request}, #rtsp_session{socket = Socket, request_re = RequestRe} = State) ->
  {match, [_, Method, URL, "RTSP", "1.0"]} = re:run(Request, RequestRe, [{capture, all, list}]),
  % {_, _, Host, Port, Path, Query} = http_uri:parse(URL),
  MethodName = binary_to_existing_atom(list_to_binary(Method), latin1),
  io:format("[RTSP] ~s ~s RTSP/1.0~n", [Method, URL]),
  inet:setopts(Socket, [{packet, line}, {active, once}]),
  {next_state, 'WAIT_FOR_HEADERS', State#rtsp_session{request = [MethodName, URL], headers = []}, ?TIMEOUT}.

'WAIT_FOR_HEADERS'({header, 'Content-Length', LengthBin}, #rtsp_session{socket = Socket} = State) ->
  Length = list_to_integer(binary_to_list(LengthBin)),
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_HEADERS', State#rtsp_session{content_length = Length}, ?TIMEOUT};

'WAIT_FOR_HEADERS'({header, 'Cseq', Sequence}, #rtsp_session{socket = Socket} = State) ->
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
  {next_state, 'WAIT_FOR_REQUEST', State1, ?TIMEOUT};
  

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
  {next_state, 'WAIT_FOR_REQUEST', State2, ?TIMEOUT};

'WAIT_FOR_DATA'({data, Message}, #rtsp_session{bytes_read = BytesRead, socket = Socket} = State) ->
  NewBytesRead = BytesRead + size(Message),
  State1 = decode_line(Message, State),
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_DATA', State1#rtsp_session{bytes_read = NewBytesRead}, ?TIMEOUT};


'WAIT_FOR_DATA'(Message, State) ->
  ?D({"Message", Message}),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.


split_params(String) ->
  lists:map(fun(Opt) -> case string:tokens(Opt, "=") of
    [Key] -> Key;
    [Key, Value] -> {Key, Value}
  end end, string:tokens(binary_to_list(String), ";")).


handle_request(#rtsp_session{request = [_Method, _URL], body = Body} = State) ->
  State1 = run_request(State#rtsp_session{body = lists:reverse(Body)}),
  State1#rtsp_session{request = undefined, content_length = undefined, headers = [], body = [], bytes_read = 0}.
  

run_request(#rtsp_session{request = ['OPTIONS', _URL]} = State) ->
  reply(State, "200 OK", [{'Public', "SETUP, TEARDOWN, PLAY, PAUSE, RECORD, OPTIONS, DESCRIBE"}]),
  State;

run_request(#rtsp_session{request = ['SETUP', _URL], headers = Headers} = State) ->
  Transport = proplists:get_value('Transport', Headers),
  TransportOpts = split_params(Transport),
  ?D({"Transport", TransportOpts}),
  % ClientPorts = proplists:get_value("client_port", TransportOpts),
  {ok, {RTCP, RTP}} = rtp_server:port(),
  ReplyHeaders = [{"Session", "42"}, {"Transport", io_lib:format("~s;server_port=~p-~p",[Transport, RTCP, RTP])}],
  reply(State, "200 OK", ReplyHeaders),
  State;

run_request(#rtsp_session{request = [_Method, _URL]} = State) ->
  reply(State, "200 OK", []),
  State.
  

decode_line(Message, #rtsp_session{rtsp_re = RtspRe, body = Body} = State) ->
  {match, [_, Key, Value]} = re:run(Message, RtspRe, [{capture, all, list}]),
  io:format("[RTSP]  ~s: ~s~n", [Key, Value]),
  State#rtsp_session{body = [{Key, Value} | Body]}.


reply(#rtsp_session{socket = Socket, sequence = Sequence}, Code, Headers) ->
  ReplyList = lists:map(fun binarize_header/1, [["RTSP/1.0", Code] | [{'Cseq', Sequence} | Headers]]),
  Reply = iolist_to_binary([ReplyList, <<"\r\n">>]),
  io:format("[RTSP] ~s", [Reply]),
  gen_tcp:send(Socket, Reply).
  
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

