-module(rtsp_client).
-author('max@maxidoors.ru').

-behaviour(gen_fsm).
-include("../include/ems.hrl").

-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
  'WAIT_FOR_SOCKET'/2,
	'WAIT_FOR_REQUEST'/2,
	'WAIT_FOR_HEADERS'/2,
  'WAIT_FOR_DATA'/2,
  handle_request/1]).

-record(rtsp_client, {
  socket,
  addr,
  port,
  request_re,
  content_length,
  bytes_read = 0,
  request,
  headers,
  body,
  session_id
}).


start_link() ->
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
  {ok, 'WAIT_FOR_SOCKET', #rtsp_client{request_re = RequestRe}}.



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
  {next_state, 'WAIT_FOR_REQUEST', State#rtsp_client{socket=Socket, addr=IP, port = Port}, ?TIMEOUT}.

'WAIT_FOR_REQUEST'(timeout, State) ->
  {stop, normal, State};

'WAIT_FOR_REQUEST'({data, Request}, #rtsp_client{socket = Socket, request_re = RequestRe} = State) ->
  ?D({"Request", Request}),
  {match, [_, Method, Url, "RTSP", "1.0"]} = re:run(Request, RequestRe, [{capture, all, list}]),
  inet:setopts(Socket, [{packet, line}, {active, once}]),
  {next_state, 'WAIT_FOR_HEADERS', State#rtsp_client{request = [Method, Url], headers = []}, ?TIMEOUT}.

'WAIT_FOR_HEADERS'({header, 'Content-Length', LengthBin}, #rtsp_client{socket = Socket} = State) ->
  Length = list_to_integer(binary_to_list(LengthBin)),
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_HEADERS', State#rtsp_client{content_length = Length}, ?TIMEOUT};

'WAIT_FOR_HEADERS'({header, <<"Cseq">>, SessionId}, #rtsp_client{socket = Socket} = State) ->
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_HEADERS', State#rtsp_client{session_id = SessionId}, ?TIMEOUT};

'WAIT_FOR_HEADERS'(timeout, State) ->
  {stop, normal, State};

'WAIT_FOR_HEADERS'({header, Name, Value}, #rtsp_client{socket = Socket, headers = Headers} = State) ->
  ?D({"Header", Name, Value}),
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_HEADERS', State#rtsp_client{headers = [{Name, Value} | Headers]}, ?TIMEOUT};

'WAIT_FOR_HEADERS'(end_of_headers, #rtsp_client{socket = Socket} = State) ->
  ?D("Headers finished"),
  inet:setopts(Socket, [{packet, line}, {active, once}]),
  {next_state, 'WAIT_FOR_DATA', State#rtsp_client{body = []}, ?TIMEOUT}.


'WAIT_FOR_DATA'(timeout, State) ->
  ?D({"Client timeout"}),
  {stop, normal, State};

'WAIT_FOR_DATA'({data, Message}, #rtsp_client{bytes_read = BytesRead, content_length = ContentLength, body = Body, socket = Socket} = State) when BytesRead + size(Message) == ContentLength ->
  ?D({"Message", Message}),
  State1 = handle_request(State#rtsp_client{body = lists:reverse([Message | Body])}),
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_REQUEST', State1#rtsp_client{request = undefined, headers = [], body = [], bytes_read = 0}, ?TIMEOUT};

'WAIT_FOR_DATA'({data, Message}, #rtsp_client{bytes_read = BytesRead, content_length = ContentLength, body = Body, socket = Socket} = State) ->
  NewBytesRead = BytesRead + size(Message),
  ?D({"Message", Message, NewBytesRead, ContentLength}),
  
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_DATA', State#rtsp_client{bytes_read = NewBytesRead, body = [Message | Body]}, ?TIMEOUT};


'WAIT_FOR_DATA'(Message, State) ->
  ?D({"Message", Message}),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.


handle_request(#rtsp_client{request = ["ANNOUNCE" = Method, Url], socket = Socket, session_id = SessionId} = State) ->
  ?D({"Request", Method, Url}),
  gen_tcp:send(Socket, <<"RTSP/1.0 200 OK\r\nCseq: ", SessionId/binary, "\r\n">>),
  State;

handle_request(#rtsp_client{request = [Method, Url]} = State) ->
  ?D({"Request", Method, Url}),
  
  State.


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
    {ok, {http_header, _, Name, _, Value}, <<"pad">>} -> ?MODULE:StateName({header, Name, Value}, State);
    {ok, http_eoh, <<"pad">>} -> ?MODULE:StateName(end_of_headers, State)
  end;

handle_info({tcp, _Socket, Bin}, StateName, State) ->
  ?MODULE:StateName({data, Bin}, State);

handle_info({http, _Socket, {http_header, _, Name, _, Value}}, StateName, State) ->
  ?MODULE:StateName({header, Name, Value}, State);

handle_info({http, _Socket, http_eoh}, StateName, State) ->
  ?MODULE:StateName(end_of_headers, State);


handle_info({tcp_closed, Socket}, _StateName, #rtsp_client{socket=Socket, addr=Addr, port = Port} = StateData) ->
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
terminate(_Reason, _StateName, #rtsp_client{socket=Socket}) ->
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

