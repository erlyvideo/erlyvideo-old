%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
%%% @doc        RTMP finite state behavior module
%%% @reference  See <a href="http://erlyvideo.googlecode.com" target="_top">http://erlyvideo.googlecode.com</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtmp_session).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-author('max@maxidoors.ru').
-include("../../../include/ems.hrl").

-behaviour(gen_fsm).

-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
  'WAIT_FOR_SOCKET'/3,
  'WAIT_FOR_SOCKET'/2,
	'WAIT_FOR_HANDSHAKE'/2,
	'WAIT_FOR_HS_ACK'/2,
  'WAIT_FOR_DATA'/2,
  'WAIT_FOR_DATA'/3]).




%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

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
    {ok, 'WAIT_FOR_SOCKET', #rtmp_session{channels = array:new(), streams = array:new()}}.



%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, _From, State) when is_pid(Socket) ->
    {reply, ok, 'WAIT_FOR_HANDSHAKE', State#rtmp_session{socket=Socket}, ?TIMEOUT}.


'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
    % Now we own the socket
    inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
    {ok, {IP, Port}} = inet:peername(Socket),
    {next_state, 'WAIT_FOR_HANDSHAKE', State#rtmp_session{socket=Socket, addr=IP, port = Port}, ?TIMEOUT};

    
'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_HANDSHAKE'({data, Data}, #rtmp_session{buff = Buff} = State) when size(Buff) + size(Data) < ?HS_BODY_LEN + 1 -> 
	Data2 = <<Buff/binary,Data/binary>>,
	{next_state, 'WAIT_FOR_HANDSHAKE', State#rtmp_session{buff=Data2}, ?TIMEOUT};

'WAIT_FOR_HANDSHAKE'({data, Data}, #rtmp_session{buff = Buff} = State) when size(Buff) + size(Data) >= ?HS_BODY_LEN + 1 ->
	case <<Buff/binary,Data/binary>> of
		<<?HS_HEADER,HandShake:?HS_BODY_LEN/binary, Rest/binary>> ->
			Reply = rtmp:handshake(HandShake),
			send_data(State, [?HS_HEADER, Reply]),
			{next_state, 'WAIT_FOR_HS_ACK', State#rtmp_session{buff = Rest}, ?TIMEOUT};
		_ -> ?D("Handshake Failed"), {stop, normal, State}
	end;

'WAIT_FOR_HANDSHAKE'(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout during handshake.\n", [self()]),
    {stop, normal, State};

'WAIT_FOR_HANDSHAKE'(Other, State) ->
    ?D({"Ignoring unexpected data:", Other}),
    {next_state, 'WAIT_FOR_HANDSHAKE', State, ?TIMEOUT}.


%% Notification event coming from client
'WAIT_FOR_HS_ACK'({data, Data}, #rtmp_session{buff = Buff} = State) when size(Buff) + size(Data) < ?HS_BODY_LEN -> 
	{next_state, 'WAIT_FOR_HS_ACK', State#rtmp_session{buff = <<Buff/binary,Data/binary>>}, ?TIMEOUT};

'WAIT_FOR_HS_ACK'({data, Data}, #rtmp_session{buff = Buff} = State) when size(Buff) + size(Data) >= ?HS_BODY_LEN -> 
	case <<Buff/binary,Data/binary>> of
		<<_HS:?HS_BODY_LEN/binary,Rest/binary>> ->
			NewState = rtmp:decode(State#rtmp_session{buff = Rest}),
			{next_state, 'WAIT_FOR_DATA', NewState, ?TIMEOUT};
		_ -> ?D("Handshake Failed"), {stop, normal, State}
	end;

'WAIT_FOR_HS_ACK'(Other, State) ->
  ?D({"Ignoring unecpected data:", Other}),
  {next_state, 'WAIT_FOR_HANDSHAKE', State, ?TIMEOUT}.


%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, #rtmp_session{buff = Buff} = State) ->
  {next_state, 'WAIT_FOR_DATA', rtmp:decode(State#rtmp_session{buff = <<Buff/binary, Data/binary>>}), ?TIMEOUT};

'WAIT_FOR_DATA'({send, {#channel{type = ?RTMP_TYPE_CHUNK_SIZE} = Channel, ChunkSize}}, #rtmp_session{server_chunk_size = OldChunkSize} = State) ->
	Packet = rtmp:encode(Channel#channel{chunk_size = OldChunkSize}, <<ChunkSize:32/big-integer>>),
	send_data(State, Packet),
  % ?D({"Set chunk size from", OldChunkSize, "to", ChunkSize}),
  {next_state, 'WAIT_FOR_DATA', State#rtmp_session{server_chunk_size = ChunkSize}, ?TIMEOUT};

'WAIT_FOR_DATA'({send, {#channel{} = Channel, Data}}, #rtmp_session{server_chunk_size = ChunkSize} = State) ->
	Packet = rtmp:encode(Channel#channel{chunk_size = ChunkSize}, Data),
  % ?D({"Channel", Channel#channel.type, Channel#channel.timestamp, Channel#channel.length}),
	send_data(State, Packet),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};

'WAIT_FOR_DATA'({send, Packet}, State) when is_binary(Packet) ->
	send_data(State, Packet),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};


'WAIT_FOR_DATA'({exit}, State) ->
  {stop, normal, State};


'WAIT_FOR_DATA'(timeout, #rtmp_session{pinged = false} = State) ->
  gen_fsm:send_event(self(), {control, ?RTMP_CONTROL_STREAM_PING, 0}),
  {next_state, 'WAIT_FOR_DATA', State#rtmp_session{pinged = true}, ?TIMEOUT};    

'WAIT_FOR_DATA'(timeout, State) ->
  error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
  {stop, normal, State};    
        
'WAIT_FOR_DATA'(Message, State) ->
  case ems:try_method_chain('WAIT_FOR_DATA', [Message, State]) of
    {unhandled} ->
    	case Message of
    		{record,Channel} when is_record(Channel,channel) -> 
    			?D({"Ignoring channel message:", Channel#channel{msg = <<>>}});
    		Data -> 
    		  ?D({"Ignoring message:", Data})
    	end,
      {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};
    Reply -> Reply
  end.

'WAIT_FOR_DATA'(info, _From, #rtmp_session{addr = {IP1, IP2, IP3, IP4}, port = Port} = State) ->
  {reply, {io_lib:format("~p.~p.~p.~p", [IP1, IP2, IP3, IP4]), Port, self()}, 'WAIT_FOR_DATA', State, ?TIMEOUT};
        

'WAIT_FOR_DATA'(Data, _From, State) ->
	io:format("~p Ignoring data: ~p\n", [self(), Data]),
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
   io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, got_sync_request2]),
  {stop, {StateName, undefined_event, Event}, StateData}.

send_data(#rtmp_session{socket = Socket}, Data) when is_port(Socket) ->
  gen_tcp:send(Socket, Data);

send_data(#rtmp_session{socket = Socket}, Data) when is_pid(Socket) ->
  gen_fsm:send_event(Socket, {server_data, Data}).

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #rtmp_session{socket=Socket} = State) ->
    % Flow control: enable forwarding of next TCP message
%	?D({"TCP",size(Bin)}),
%	file:write_file("/sfe/temp/packet.txt",Bin),
  inet:setopts(Socket, [{active, once}]),
  ?MODULE:StateName({data, Bin}, State);

handle_info({tcp_closed, Socket}, _StateName,
            #rtmp_session{socket=Socket, addr=Addr, port = Port} = StateData) ->
    error_logger:info_msg("~p Client ~p:~p disconnected.\n", [self(), Addr, Port]),
    {stop, normal, StateData};

handle_info({'EXIT', PlayerPid, _Reason}, StateName, #rtmp_session{streams = _Streams} = StateData) ->
  % FIXME: proper lookup of dead player between Streams and notify right stream
  % ?D({"Player died", PlayerPid, _Reason}),
  gen_fsm:send_event(self(), {status, ?NS_PLAY_COMPLETE}),
  {next_state, StateName, StateData, ?TIMEOUT};

handle_info({'EXIT', Pid, _Reason}, StateName, StateData) ->
  ?D({"Died child", Pid, _Reason}),
  {next_state, StateName, StateData, ?TIMEOUT};

handle_info({Port, {data, _Line}}, StateName, State) when is_port(Port) ->
  % No-op. Just child program
  {next_state, StateName, State, ?TIMEOUT};


handle_info(_Info, StateName, StateData) ->
  ?D({"Some info handled", _Info, StateName, StateData}),
  {noreply, StateName, StateData}.


%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #rtmp_session{socket=Socket, streams = Streams} = State) ->
  lists:foreach(fun(Player) when is_pid(Player) -> Player ! exit;
                   (_) -> ok end, array:sparse_to_list(Streams)),

  lists:foreach(fun(Player) when is_pid(Player) -> (catch erlang:exit(Player));
                   (_) -> ok end, array:sparse_to_list(Streams)),
  ems:call_modules(logout, [State]),
  rtmp_listener:logout(),
  (catch gen_tcp:close(Socket)),
  ok.


%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(OldVersion, StateName, State, Extra) ->
  plugins_code_change(OldVersion, StateName, State, Extra, ems:get_var(applications, [])).

plugins_code_change(_OldVersion, StateName, State, _Extra, []) -> {ok, StateName, State};

plugins_code_change(OldVersion, StateName, State, Extra, [Module | Modules]) -> 
  case ems:respond_to(Module, code_change, 4) of
    true ->
      error_logger:info_msg("Code change in module ~p~n", [Module]),
      {ok, NewStateName, NewState} = Module:code_change(OldVersion, StateName, State, Extra);
    _ ->
      {NewStateName, NewState} = {StateName, State}
  end,
  plugins_code_change(OldVersion, NewStateName, NewState, Extra, Modules).






