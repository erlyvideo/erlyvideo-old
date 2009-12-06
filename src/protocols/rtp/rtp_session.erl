-module(rtp_session).
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
  'WAIT_FOR_DATA'/2]).

-record(rtp_session, {
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
  session_id
}).


start_link() ->
  % All possible headers:
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
  {ok, 'WAIT_FOR_SOCKET', #rtp_session{request_re = RequestRe, rtsp_re = RtspRe}}.



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
  {next_state, 'WAIT_FOR_REQUEST', State#rtp_session{socket=Socket, addr=IP, port = Port}, ?TIMEOUT}.

'WAIT_FOR_REQUEST'(timeout, State) ->
  {stop, normal, State};

'WAIT_FOR_REQUEST'({data, Request}, #rtp_session{socket = Socket, request_re = RequestRe} = State) ->
  ?D({"Request", Request}),
  {match, [_, Method, URL, "RTSP", "1.0"]} = re:run(Request, RequestRe, [{capture, all, list}]),
  % {_, _, Host, Port, Path, Query} = http_uri:parse(URL),
  MethodName = binary_to_existing_atom(list_to_binary(Method), utf8),
  inet:setopts(Socket, [{packet, line}, {active, once}]),
  {next_state, 'WAIT_FOR_HEADERS', State#rtp_session{request = [MethodName, URL], headers = []}, ?TIMEOUT}.

'WAIT_FOR_HEADERS'({header, 'Content-Length', LengthBin}, #rtp_session{socket = Socket} = State) ->
  Length = list_to_integer(binary_to_list(LengthBin)),
  ?D({"Content length", Length}),
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_HEADERS', State#rtp_session{content_length = Length}, ?TIMEOUT};

'WAIT_FOR_HEADERS'({header, 'Cseq', SessionId}, #rtp_session{socket = Socket} = State) ->
  ?D({"SessionId", SessionId}),
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_HEADERS', State#rtp_session{session_id = SessionId}, ?TIMEOUT};

'WAIT_FOR_HEADERS'(timeout, State) ->
  {stop, normal, State};

'WAIT_FOR_HEADERS'({header, Name, Value}, #rtp_session{socket = Socket, headers = Headers} = State) ->
  ?D({"Header", Name, Value}),
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_HEADERS', State#rtp_session{headers = [{Name, Value} | Headers]}, ?TIMEOUT};

'WAIT_FOR_HEADERS'(end_of_headers, #rtp_session{socket = Socket, content_length = undefined} = State) ->
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_REQUEST', State, ?TIMEOUT};
  

'WAIT_FOR_HEADERS'(end_of_headers, #rtp_session{socket = Socket} = State) ->
  ?D({"Headers finished", State#rtp_session.content_length}),
  inet:setopts(Socket, [{packet, line}, {active, once}]),
  {next_state, 'WAIT_FOR_DATA', State#rtp_session{body = []}, ?TIMEOUT}.


'WAIT_FOR_DATA'(timeout, State) ->
  ?D({"Client timeout"}),
  {stop, normal, State};

'WAIT_FOR_DATA'({data, Message}, #rtp_session{bytes_read = BytesRead, socket = Socket} = State) ->
  NewBytesRead = BytesRead + size(Message),
  inet:setopts(Socket, [{active, once}]),
  {next_state, 'WAIT_FOR_DATA', State#rtp_session{bytes_read = NewBytesRead}, ?TIMEOUT};


'WAIT_FOR_DATA'(Message, State) ->
  ?D({"Message", Message}),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.


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
handle_info({tcp, _Socket, Bin}, StateName, State) ->
  ?MODULE:StateName({data, Bin}, State);

% handle_info({http, _Socket, {http_header, _, Name, _, Value}}, StateName, State) ->
%   ?MODULE:StateName({header, Name, Value}, State);
% 
% handle_info({http, _Socket, http_eoh}, StateName, State) ->
%   ?MODULE:StateName(end_of_headers, State);


handle_info({tcp_closed, Socket}, _StateName, #rtp_session{socket=Socket, addr=Addr, port = Port} = StateData) ->
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
terminate(_Reason, _StateName, #rtp_session{socket=Socket}) ->
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

