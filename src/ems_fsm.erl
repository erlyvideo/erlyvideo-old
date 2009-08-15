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
-module(ems_fsm).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-author('max@maxidoors.ru').
-include("../include/ems.hrl").

-behaviour(gen_fsm).

-export([start_link/0, set_socket/2]).
-import(ems_flv).
-import(ems_play).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
  'WAIT_FOR_SOCKET'/2,
	'WAIT_FOR_HANDSHAKE'/2,
	'WAIT_FOR_HS_ACK'/2,
  'WAIT_FOR_DATA'/2,
  'WAIT_FOR_DATA'/3]).


%% rsacon: TODOD move this to ems.hrl ist her only for testing purpose
%% or make it an application confiuration environment variable
-define(VIDEO_WRITE_BUFFER, 20000). 


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
    {ok, 'WAIT_FOR_SOCKET', #ems_fsm{}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
    % Now we own the socket
    inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, 'WAIT_FOR_HANDSHAKE', State#ems_fsm{socket=Socket, addr=IP}, ?TIMEOUT};
    
'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_HANDSHAKE'({data, Data}, #ems_fsm{buff = Buff} = State) when size(Buff) + size(Data) < ?HS_BODY_LEN + 1 -> 
	Data2 = <<Buff/binary,Data/binary>>,
	{next_state, 'WAIT_FOR_HANDSHAKE', State#ems_fsm{buff=Data2}, ?TIMEOUT};

'WAIT_FOR_HANDSHAKE'({data, Data}, #ems_fsm{buff = Buff} = State) when size(Buff) + size(Data) == ?HS_BODY_LEN + 1 ->
	case <<Buff/binary,Data/binary>> of
		<<?HS_HEADER,HandShake:?HS_BODY_LEN/binary>> ->
			Reply = ems_rtmp:handshake(HandShake),
			gen_tcp:send(State#ems_fsm.socket, <<?HS_HEADER, Reply/binary>>),
			{next_state, 'WAIT_FOR_HS_ACK', State#ems_fsm{buff = <<>>}, ?TIMEOUT};
		_ -> ?D("Handshake Failed"), {stop, normal, State}
	end;

'WAIT_FOR_HANDSHAKE'(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout.\n", [self()]),
    {stop, normal, State};

'WAIT_FOR_HANDSHAKE'(Other, State) ->
    ?D({"Ignoring unecpected data:", Other}),
    {next_state, 'WAIT_FOR_HANDSHAKE', State, ?TIMEOUT}.


%% Notification event coming from client
'WAIT_FOR_HS_ACK'({data, Data}, #ems_fsm{buff = Buff} = State) when size(Buff) + size(Data) < ?HS_BODY_LEN -> 
	{next_state, 'WAIT_FOR_HS_ACK', State#ems_fsm{buff = <<Buff/binary,Data/binary>>}, ?TIMEOUT};

'WAIT_FOR_HS_ACK'({data, Data}, #ems_fsm{buff = Buff} = State) when size(Buff) + size(Data) > ?HS_BODY_LEN -> 
	case <<Buff/binary,Data/binary>> of
		<<_HS:?HS_BODY_LEN/binary,Rest/binary>> ->
			NewState = ems_rtmp:decode(Rest,State),
			{next_state, 'WAIT_FOR_DATA', NewState#ems_fsm{buff = <<>>}, ?TIMEOUT};
		_ -> ?D("Handshake Failed"), {stop, normal, State}
	end;

'WAIT_FOR_HS_ACK'(Other, State) ->
  ?D({"Ignoring unecpected data:", Other}),
  {next_state, 'WAIT_FOR_HANDSHAKE', State, ?TIMEOUT}.


%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, State) ->
	NewState = ems_rtmp:decode(Data,State),
  {next_state, 'WAIT_FOR_DATA', NewState, ?TIMEOUT};

'WAIT_FOR_DATA'({send, {Channel, AMF}}, State) when is_record(Channel,channel), is_record(AMF,amf) ->
	Packet = ems_rtmp:encode(Channel,AMF),
	gen_tcp:send(State#ems_fsm.socket,Packet),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};

'WAIT_FOR_DATA'({send, {Channel, Data}}, State) when is_record(Channel,channel), is_binary(Data) ->
	Packet = ems_rtmp:encode(Channel,Data),
%	?D({"Sending Packet",size(Packet),Channel#channel.id}),
%	file:write_file("/sfe/temp/packet.txt",Packet),
%	?D({"Packet: ",Packet}),
	gen_tcp:send(State#ems_fsm.socket, Packet),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};

'WAIT_FOR_DATA'({send, Packet}, State) when is_binary(Packet) ->
	gen_tcp:send(State#ems_fsm.socket,Packet),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};
        
'WAIT_FOR_DATA'({play, Name, StreamId}, State) ->
  case ems_cluster:is_live_stream(Name) of
    true ->
      ems_cluster:subscribe(self(), Name),
      NextState = State#ems_fsm{type  = live},
      {next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};
    _ ->
      FileName = filename:join([ems_play:file_dir(), ems_play:normalize_filename(Name)]),  
      case filelib:is_regular(FileName) of
        true ->
          case ems_play:play(FileName, StreamId, State#ems_fsm{type = vod}) of
            {ok, PlayerPid} ->
              NextState = State#ems_fsm{type  = vod, video_player = PlayerPid},
              gen_fsm:send_event(PlayerPid, {timeout, timer, play}),
              {next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};
            Reason -> 
              ?D({"Failed to start video player", Reason}),
              {error, Reason}
            end;
        _ ->
          ems_cluster:subscribe(self(), Name),
          NextState = State#ems_fsm{type  = wait},
          {next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT}
        end
    end;

'WAIT_FOR_DATA'({stop}, #ems_fsm{video_device = IoDev, video_buffer = Buffer, video_timer_ref = TimerRef, type = _Type} = State) ->
	case Buffer of
		undefined -> ok;
		_ -> file:write(IoDev, lists:reverse(Buffer))
	end,
    case IoDev of
        undefined -> ok;
        _ -> file:close(IoDev)
    end,
    case TimerRef of
        undefined -> ok;
        _ -> gen_fsm:cancel_timer(TimerRef)
    end,
    case type of
        live -> 
            ems_cluster:unsubscribe(State#ems_fsm.video_file_name, self());
        wait -> 
            ems_cluster:unsubscribe(State#ems_fsm.video_file_name, self());
        broadcast ->
            emscluster:stop_broadcast(State#ems_fsm.video_file_name);
        _ -> 
            ok
    end,
	NextState = State#ems_fsm{video_device=undefined,video_buffer=[],
							  video_timer_ref=undefined,video_pos = 0},
    {next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};

'WAIT_FOR_DATA'({publish, record, Name}, State) when is_list(Name) ->
	FileName = filename:join([ems_play:file_dir(), Name]),
	Header = ems_flv:header(#flv_header{version = 1, audio = 1, video = 1}),
	case file:open(FileName, [write, append]) of
		{ok, IoDev} ->
			NextState = State#ems_fsm{video_buffer=[Header],type=record,video_device=IoDev,video_file_name=FileName,video_ts_prev=0},
			{next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};
		_ ->
			{next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}
	end;

%% rsaccon: TODO get last timstamp of the exisiting FLV file, without that it won't playback propperly 	
'WAIT_FOR_DATA'({publish, append, Name}, State) when is_list(Name) ->
	FileName = filename:join([ems_play:file_dir(), Name]),
	case file:open(FileName, [write, append]) of
		{ok, IoDev} ->
			NextState = State#ems_fsm{type=record_append,video_device=IoDev,video_file_name=FileName,video_ts_prev=0},
			{next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};
	    _ ->
    		{next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}
    end;	
    
'WAIT_FOR_DATA'({publish, live, Name}, State) when is_list(Name) ->        
    ems_cluster:broadcast(Name),
    %% hack - empty audio
    StreamId=1,
    Id=ems_play:channel_id(?RTMP_TYPE_AUDIO, StreamId),
    TimeStamp=0,
    Length=0,
    Type=?RTMP_TYPE_AUDIO,
    Rest = <<>>,
    FirstPacket = <<?RTMP_HDR_NEW:2,Id:6,TimeStamp:24,Length:24,Type:8,StreamId:32/little,Rest/binary>>,    
    ems_cluster:broadcast(Name, FirstPacket),
    %% hack end
    NextState = State#ems_fsm{type=broadcast,video_file_name=Name, video_ts_prev = 0},
	{next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};

'WAIT_FOR_DATA'({publish,Channel}, #ems_fsm{type = broadcast, 
                                            video_ts_prev = PrevTs,
                                            video_file_name = Name} = State) when is_record(Channel,channel) ->
	NextTimeStamp = PrevTs + Channel#channel.timestamp,    
%	?D({"Broadcast",Channel#channel.id,Channel#channel.type,size(Channel#channel.msg),NextTimeStamp}),
	Packet = ems_rtmp:encode(Channel#channel{id = ems_play:channel_id(Channel#channel.type,1), timestamp = NextTimeStamp}),        
    ems_cluster:broadcast(Name, Packet),
    NextState = State#ems_fsm{video_ts_prev=NextTimeStamp},
    {next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};
    	
'WAIT_FOR_DATA'({publish,Channel}, #ems_fsm{video_ts_prev = PrevTs, 
                                            video_device = IoDev, 
                                            video_buffer = Buffer} = State) when is_record(Channel,channel) ->
	?D({"Record",Channel#channel.type,size(Channel#channel.msg),Channel#channel.timestamp,PrevTs}),
	{Tag,NextTimeStamp} = ems_flv:to_tag(Channel,PrevTs),
	FlvChunk = [Tag | Buffer],	
	Size = size(list_to_binary(FlvChunk)),
	NextState = if
		(Size > ?VIDEO_WRITE_BUFFER) ->
			file:write(IoDev, lists:reverse(Buffer)),
			State#ems_fsm{video_buffer=[Tag],video_ts_prev=NextTimeStamp};
		true ->
			State#ems_fsm{video_buffer=FlvChunk,video_ts_prev=NextTimeStamp}
	end,
	{next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};	

'WAIT_FOR_DATA'(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
    {stop, normal, State};    
        
'WAIT_FOR_DATA'(Data, State) ->
	case Data of
		{record,Channel} when is_record(Channel,channel) -> 
			io:format("~p Ignoring data: ~p\n", [self(), Channel#channel{msg = <<>>}]);
		Data -> 
			io:format("~p Ignoring data: ~p\n", [self(), Data])
	end,
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.
        

'WAIT_FOR_DATA'(next_stream_id, _From, #ems_fsm{next_stream_id = Id} = State) ->
    io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, got_next_stream_id_request]),
    {reply, Id, 'WAIT_FOR_DATA', State#ems_fsm{next_stream_id = Id + 1}};   
    
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


%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #ems_fsm{socket=Socket} = State) ->
    % Flow control: enable forwarding of next TCP message
%	?D({"TCP",size(Bin)}),
%	file:write_file("/sfe/temp/packet.txt",Bin),
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, State);

handle_info({tcp_closed, Socket}, _StateName,
            #ems_fsm{socket=Socket, addr=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.


%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #ems_fsm{socket=Socket}) ->
    (catch gen_tcp:close(Socket)),
    ok.


%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, #ems_fsm{video_player = PlayerPid} = StateData, _Extra) ->
  erlang:kill(PlayerPid),
  {ok, StateName, StateData}.







