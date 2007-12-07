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
-include("../include/ems.hrl").

-behaviour(gen_fsm).

-export([start_link/0, set_socket/2]).

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
-define(FLV_WRITE_BUFFER, 20000). 


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
	?D({"Packet: ",Packet}),
	gen_tcp:send(State#ems_fsm.socket,Packet),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};

'WAIT_FOR_DATA'({send, {Channel, Data}}, State) when is_record(Channel,channel), is_binary(Data) ->
	Packet = ems_rtmp:encode(Channel,Data),
%	?D({"Sending Packet",size(Packet),Channel#channel.id}),
%	file:write_file("/sfe/temp/packet.txt",Packet),
%	?D({"Packet: ",Packet}),
	gen_tcp:send(State#ems_fsm.socket,Packet),
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
            FileName = filename:join([flv_dir(), normalize_fileame(Name)]),  
        	case filelib:is_regular(FileName) of
        		true ->
        		    play_vod(FileName, StreamId, State#ems_fsm{type = vod});
        		_ ->
        		    ems_cluster:subscribe(self(), Name),
        		    NextState = State#ems_fsm{type  = wait},
                    {next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT}
        	end
    end;    

'WAIT_FOR_DATA'({timeout, Timer, play}, #ems_fsm{flv_timer_ref = Timer, flv_device = IoDev, flv_pos = Pos, flv_stream_id = StreamId} = State) ->
	case ems_flv:read_tag(IoDev, Pos) of
		{ok, done} ->
			file:close(IoDev),
			{next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};
		{ok, Tag} when is_record(Tag,flv_tag) ->
			TimeStamp = Tag#flv_tag.timestamp_abs - State#ems_fsm.flv_ts_prev,			
			send(Tag#flv_tag{timestamp=TimeStamp, streamid = StreamId}),
 			Timeout = timeout(Tag#flv_tag.timestamp_abs, 
			                  State#ems_fsm.flv_timer_start, 
    		                  State#ems_fsm.client_buffer),
			NewTimer = gen_fsm:start_timer(Timeout, play),
			NextState = State#ems_fsm{flv_timer_ref  = NewTimer,
									  flv_ts_prev = Tag#flv_tag.timestamp_abs,
									  flv_pos = Tag#flv_tag.nextpos},
			{next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};
		{error,_Reason} -> 
			file:close(IoDev),
			{stop, normal, State}
	end;

'WAIT_FOR_DATA'({stop}, #ems_fsm{flv_device = IoDev, flv_buffer = Buffer, flv_timer_ref = TimerRef, type = Type} = State) ->
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
            ems_cluster:unsubscribe(State#ems_fsm.flv_file_name, self());
        wait -> 
            ems_cluster:unsubscribe(State#ems_fsm.flv_file_name, self());
        broadcast ->
            emscluster:stop_broadcast(State#ems_fsm.flv_file_name);
        _ -> 
            ok
    end,
	NextState = State#ems_fsm{flv_device=undefined,flv_buffer=[],
							  flv_timer_ref=undefined,flv_pos = 0},
    {next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};

'WAIT_FOR_DATA'({publish, record, Name}, State) when is_list(Name) ->
	FileName = filename:join([flv_dir(), Name]),
	Header = ems_flv:header(#flv_header{version = 1, audio = 1, video = 1}),
	case file:open(FileName, [write, append]) of
		{ok, IoDev} ->
			NextState = State#ems_fsm{flv_buffer=[Header],type=record,flv_device=IoDev,flv_file_name=FileName,flv_ts_prev=0},
			{next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};
		_ ->
			{next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}
	end;

%% rsaccon: TODO get last timstamp of the exisiting FLV file, without that it won't playback propperly 	
'WAIT_FOR_DATA'({publish, append, Name}, State) when is_list(Name) ->
	FileName = filename:join([flv_dir(), Name]),
	case file:open(FileName, [write, append]) of
		{ok, IoDev} ->
			NextState = State#ems_fsm{type=record_append,flv_device=IoDev,flv_file_name=FileName,flv_ts_prev=0},
			{next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};
	    _ ->
    		{next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}
    end;	
    
'WAIT_FOR_DATA'({publish, live, Name}, State) when is_list(Name) ->        
    ems_cluster:broadcast(Name),
    %% hack - empty audio
    StreamId=1,
    Id=channel_id(?RTMP_TYPE_AUDIO, StreamId),
    TimeStamp=0,
    Length=0,
    Type=?RTMP_TYPE_AUDIO,
    Rest = <<>>,
    FirstPacket = <<?RTMP_HDR_NEW:2,Id:6,TimeStamp:24,Length:24,Type:8,StreamId:32/little,Rest/binary>>,    
    ems_cluster:broadcast(Name, FirstPacket),
    %% hack end
    NextState = State#ems_fsm{type=broadcast,flv_file_name=Name, flv_ts_prev = 0},
	{next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};

'WAIT_FOR_DATA'({publish,Channel}, #ems_fsm{type = broadcast, 
                                            flv_ts_prev = PrevTs,
                                            flv_file_name = Name} = State) when is_record(Channel,channel) ->
	NextTimeStamp = PrevTs + Channel#channel.timestamp,    
%	?D({"Broadcast",Channel#channel.id,Channel#channel.type,size(Channel#channel.msg),NextTimeStamp}),
	Packet = ems_rtmp:encode(Channel#channel{id = channel_id(Channel#channel.type,1), timestamp = NextTimeStamp}),        
    ems_cluster:broadcast(Name, Packet),
    NextState = State#ems_fsm{flv_ts_prev=NextTimeStamp},
    {next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};
    	
'WAIT_FOR_DATA'({publish,Channel}, #ems_fsm{flv_ts_prev = PrevTs, 
                                            flv_device = IoDev, 
                                            flv_buffer = Buffer} = State) when is_record(Channel,channel) ->
	?D({"Record",Channel#channel.type,size(Channel#channel.msg),Channel#channel.timestamp,PrevTs}),
	{Tag,NextTimeStamp} = ems_flv:to_tag(Channel,PrevTs),
	FlvChunk = [Tag | Buffer],	
	Size = size(list_to_binary(FlvChunk)),
	NextState = if
		(Size > ?FLV_WRITE_BUFFER) ->
			file:write(IoDev, lists:reverse(Buffer)),
			State#ems_fsm{flv_buffer=[Tag],flv_ts_prev=NextTimeStamp};
		true ->
			State#ems_fsm{flv_buffer=FlvChunk,flv_ts_prev=NextTimeStamp}
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
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%%-------------------------------------------------------------------------
%% @spec (FLV_TAG::tuple()) -> any()
%% @doc Convert FLV_Tag into Channel then transmit the Channel and Body
%% @end
%%-------------------------------------------------------------------------
send(#flv_tag{type = Type, streamid=StreamId,timestamp_abs = TimeStamp,body=Body} = FLVTag) when is_record(FLVTag,flv_tag) ->
	Channel = #channel{id=channel_id(Type, StreamId),timestamp=TimeStamp,length=size(Body),type=Type,stream=StreamId},
	gen_fsm:send_event(self(), {send, {Channel,Body}}).


%%-------------------------------------------------------------------------
%% @spec () -> FileName::string()
%% @doc retrieves FLV video file folder from application environment
%% @end
%%-------------------------------------------------------------------------	
flv_dir() ->
    case application:get_env(ems, flv_dir) of
        {ok, Val} ->
            Val;
        _ ->
            exit(flv_dir_not_defined)
    end.


%%-------------------------------------------------------------------------
%% @spec (AbsTime::integer(), TimerStart::integer(), ClientBuffer::integer()) -> [TimeOut::integer() | 0]
%% @doc calculates timeout to playback of next FLV Tag 
%% @end
%%-------------------------------------------------------------------------	
timeout(AbsTime, TimerStart, ClientBuffer) ->
    Timeout = AbsTime - ClientBuffer - trunc(timer:now_diff(now(), TimerStart) / 1000),
    if 
		(Timeout > 0) -> 
            Timeout; 
        true -> 
            0 
    end.


normalize_fileame(Name) ->
    case filename:extension(Name) of
        ".flv" -> Name;
        ".FLV" -> Name;
        _      -> Name ++ ".flv"
    end.
 
 
play_vod(FileName, StreamId, State) ->
	?D({"Found It",FileName}),
	{ok, IoDev} = file:open(FileName, [read, read_ahead]),
	case ems_flv:read_header(IoDev) of
		{ok, Pos, _FLVHeader} -> 
			case ems_flv:read_tag(IoDev, Pos) of
				{ok, done} ->
					file:close(IoDev),
					{stop, normal, State};
				{ok, Tag} when is_record(Tag,flv_tag) -> 
					Now = erlang:now(),
					send(Tag#flv_tag{streamid = StreamId}),
					Timeout = timeout(Tag#flv_tag.timestamp_abs, 
					                  Now, 
					                  State#ems_fsm.client_buffer),
					Timer = gen_fsm:start_timer(Timeout, play),
					NextState = State#ems_fsm{flv_device = IoDev,
											  flv_stream_id = StreamId, 
											  flv_timer_start = Now,
											  flv_timer_ref  = Timer,
											  flv_ts_prev = Tag#flv_tag.timestamp_abs,
											  flv_pos = Tag#flv_tag.nextpos},
					{next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};
				{error, _Reason} ->
					?D(_Reason),
					file:close(IoDev),
					{stop, normal, State}
			end;
		_HdrError -> 
		    ?D(_HdrError)
	end.
        	    

% rsaccon: TODO: streams per connections need to be stored and channelId retrieved from stream
% idea: a  process per stream, mnesia RAM table (with streamid as key) contains stream process PID
channel_id(?FLV_TAG_TYPE_META, _StreamId) -> 4;
channel_id(?FLV_TAG_TYPE_VIDEO, _StreamId) -> 5;
channel_id(?FLV_TAG_TYPE_AUDIO, _StreamId) -> 6.







