%%% Copyright (c) 2007 Roberto Saccon <rsaccon@gmail.com>
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

%%% Non-blocking OTP TCP Server derived from:
%%% http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles
 
-module(erlyvideo_fsm).
-author('rsaccon@gmail.com').

-include("erlyvideo.hrl").

-behaviour(gen_fsm).

-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export(['WAIT_FOR_SOCKET'/2,
         'WAIT_FOR_HANDSHAKE'/2,
         'WAIT_FOR_HANDSHAKE_ACKN'/2,
         'WAIT_FOR_DATA'/2]).

%% --------------------------------------------------------------------
%% global definitions
%% --------------------------------------------------------------------
-define(TIMEOUT, 120000).
-define(DEFAULT_VIDEO_DIR, "/tmp").

% delay time in milliseconds, before clip starts to play
-define(MIN_CLIENT_BUFFER, 100).    


-record(state, {socket,             % client socket
                addr,               % client address
                buf = <<>>,         % reentrant parser
                ch = undefined,     % current/next channel
                chs = [],           % list of channels
                client_buffer = ?MIN_CLIENT_BUFFER,
                chunk_size = ?RTMP_DEFAULT_CHUNK_SIZE,
                flv_read_file = undefined,
                flv_write_file = undefined,
                flv_timer_ref = undefined,
                flv_timer_start,
                flv_pos = 0,
                flv_stream_id,
                flv_ts_prev}).


%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @spec (Socket) -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%      If init/1 fails with Reason, the function returns {error,Reason}.
%%      If init/1 returns {stop,Reason} or ignore, the process is
%%      terminated and the function returns {error,Reason} or ignore,
%%      respectively.
%% @end
%%-------------------------------------------------------------------------
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
    {ok, 'WAIT_FOR_SOCKET', #state{}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
    %% Now we own the socket
    inet:setopts(Socket, [{active, once}, 
                          {packet, raw}, 
                          binary]),
    {ok, {Ip, _Port}} = inet:peername(Socket),
    {next_state, 'WAIT_FOR_HANDSHAKE', 
     State#state{socket=Socket, addr=Ip}, ?TIMEOUT};

'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", 
                           [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_HANDSHAKE'({data, Data}, State)  ->
    Buf = State#state.buf,
    Data2 = <<Buf/binary, Data/binary>>,
    case Data2 of 
        <<?RTMP_HANDSHAKE_FIRST_BYTE, 
         Handshake:?RTMP_HANDSHAKE_BLOCK_LENGTH/binary>> ->
            Resp = erlyvideo_rtmp:handshake(Handshake),
            gen_tcp:send(State#state.socket, <<?RTMP_HANDSHAKE_FIRST_BYTE, 
                                              Resp/binary>>),
            {next_state, 'WAIT_FOR_HANDSHAKE_ACKN', 
             State#state{buf = <<>>}, ?TIMEOUT};
        <<?RTMP_HANDSHAKE_FIRST_BYTE, Rest/binary>> 
        when size(Rest) < ?RTMP_HANDSHAKE_BLOCK_LENGTH ->
            {next_state, 'WAIT_FOR_HANDSHAKE', 
             State#state{buf=Data2}, ?TIMEOUT};
        _ ->
            ?TRACE("Handshake failed"),
            {stop, normal, State}
    end;

'WAIT_FOR_HANDSHAKE'(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout.\n", [self()]),
    {stop, normal, State};

'WAIT_FOR_HANDSHAKE'(Other, State) ->
    ?TRACE({"Ignoring unecpected data:", Other}),
    {next_state, 'WAIT_FOR_HANDSHAKE', State, ?TIMEOUT}.

%% Notification event coming from client
'WAIT_FOR_HANDSHAKE_ACKN'({data, Data}, State) -> 
    Buf0 = State#state.buf,
    Data2 = <<Buf0/binary, Data/binary>>,
    case Data2 of 
        <<_Ackn:?RTMP_HANDSHAKE_BLOCK_LENGTH/binary, Rest/binary>> ->
            {Ch, Chs, Buf} =  handle(State#state.ch, State#state.chs,
                                     State#state.chunk_size, Rest),
            {next_state, 'WAIT_FOR_DATA', 
             State#state{ch=Ch, chs=Chs, buf=Buf}, ?TIMEOUT};
        <<Data/binary>> when size(Data) < ?RTMP_HANDSHAKE_BLOCK_LENGTH ->
            {next_state, 'WAIT_FOR_HANDSHAKE_ACKN', 
             State#state{buf=Data}, ?TIMEOUT};
        _ ->
            ?TRACE("Handshake failed"),
            {stop, normal, State}
    end;

'WAIT_FOR_HANDSHAKE_ACKN'(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout.\n", [self()]),
    {stop, normal, State};

'WAIT_FOR_HANDSHAKE_ACKN'(Other, State) ->
    ?TRACE({"Ignoring unecpected data:", Other}),
    {next_state, 'WAIT_FOR_HANDSHAKE_ACKN', State, ?TIMEOUT}.


%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, State) ->
    Buf0 = State#state.buf,
    Data2 = <<Buf0/binary, Data/binary>>,
    {Ch, Chs, Buf} =  handle(State#state.ch, State#state.chs, 
                             State#state.chunk_size, Data2),
    {next_state, 'WAIT_FOR_DATA', 
     State#state{ch=Ch, chs=Chs, buf=Buf}, ?TIMEOUT};

'WAIT_FOR_DATA'({send, What, Data}, State) ->
    {next_state, 'WAIT_FOR_DATA', send(What, Data, State), ?TIMEOUT};

'WAIT_FOR_DATA'({record, Name}, State) ->
    Dir = erlyvideo_app:get_app_env(video_dir, ?DEFAULT_VIDEO_DIR),
    FileName = filename:join([Dir, Name]),
    case file:open(FileName, [write, append]) of
        {ok, IoDev} ->
            case erlyvideo_flv:write_header(IoDev) of
                ok ->
                    {next_state, 'WAIT_FOR_DATA', 
                     State#state{flv_write_file = IoDev}, ?TIMEOUT};
                _ ->
                    ?TRACE("ERR writng header"),
                    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}            
            end;
        _ ->
            ?TRACE("ERR opeing file"),
            {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}      
    end;

'WAIT_FOR_DATA'({record, Ts, Type, StId, Data}, State) ->  
    {IoDev, Ts2} = case State#state.flv_write_file of
                       {IoDev0, PrevTs} ->
                           {IoDev0, PrevTs + Ts};
                       IoDev0 ->
                           {IoDev0, Ts}
                   end,
    erlyvideo_flv:write_tag(IoDev, Type, Ts2, StId, Data), 
    {next_state, 'WAIT_FOR_DATA', 
     State#state{flv_write_file = {IoDev, Ts2}}, ?TIMEOUT};

'WAIT_FOR_DATA'({play, Name, StId}, State) ->
    Dir = erlyvideo_app:get_app_env(video_dir, "/tmp"),
    FileName = filename:join([Dir, Name]),
    try
        {ok, IoDev} = file:open(FileName, [read, read_ahead]),
        {ok, Pos} = erlyvideo_flv:read_header(IoDev),
        case erlyvideo_flv:read_tag(IoDev, Pos) of
            {ok, done} ->
                file:close(IoDev),
                {stop, normal, State};
            {ok, TagType, TsAbs, Data, Pos2} ->
                Now = erlang:now(),
                State2 = send({TsAbs, TagType, StId}, Data, State),
                Timeout = calc_timeout(TsAbs, Now, 
                                       State2#state.client_buffer),
                Timer = gen_fsm:start_timer(Timeout, play),
                {next_state, 'WAIT_FOR_DATA', 
                 State2#state{flv_read_file = IoDev,
                              flv_stream_id = StId,
                              flv_timer_start = Now,
                              flv_timer_ref  = Timer,
                              flv_ts_prev = TsAbs,
                              flv_pos = Pos2}, ?TIMEOUT};          
            {error, _Reason} ->
                file:close(IoDev),
                {stop, normal, State}
        end
    catch 
        Err ->
            error_logger:error_msg("Reading FlV file failed: ~p\n", [Err]),
            {stop, normal, State}
    end;

'WAIT_FOR_DATA'({stop}, State) ->
    case State#state.flv_read_file of
        undefined -> ok;
        _ -> file:close(State#state.flv_read_file)
    end,
    case State#state.flv_timer_ref of
        undefined -> ok;
        _ -> gen_fsm:cancel_timer(State#state.flv_timer_ref)
    end,
    {next_state, 'WAIT_FOR_DATA', 
     State#state{flv_read_file = undefined, flv_timer_ref = undefined, 
                 flv_pos = 0}, ?TIMEOUT};

'WAIT_FOR_DATA'({timeout, _Timer, play}, State) ->
    case erlyvideo_flv:read_tag(State#state.flv_read_file,
                                State#state.flv_pos) of
        {ok, done} ->
            file:close(State#state.flv_read_file),
            {stop, normal, State};
        {ok, TagType, TsAbs, Data, Pos} ->
            Ts = TsAbs - State#state.flv_ts_prev,
            State2 = send({Ts, TagType, State#state.flv_stream_id}, 
                          Data, State),
            Timeout = calc_timeout(TsAbs, 
                                   State2#state.flv_timer_start,
                                   State2#state.client_buffer),
            Timer = gen_fsm:start_timer(Timeout, play),
            {next_state, 
             'WAIT_FOR_DATA', 
             State2#state{flv_timer_ref = Timer, 
                          flv_ts_prev = TsAbs,
                          flv_pos = Pos}, ?TIMEOUT};             
        {error, _Reason} ->
            file:close(State#state.flv_read_file),
            {stop, normal, State}
    end;

'WAIT_FOR_DATA'({update, client_buffer, ClientBuffer}, State) ->  
    State2 = if
                 (ClientBuffer > ?MIN_CLIENT_BUFFER) ->
                     State#state{client_buffer = ClientBuffer};
                 true ->
                     State
             end,
    {next_state, 'WAIT_FOR_DATA', State2, ?TIMEOUT}; 

'WAIT_FOR_DATA'({update, chunk_size, ChunkSize}, State) ->  
    {next_state, 'WAIT_FOR_DATA', 
     State#state{chunk_size = ChunkSize}, ?TIMEOUT};
             
'WAIT_FOR_DATA'(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout.\n", [self()]),
    {stop, normal, State};

'WAIT_FOR_DATA'(Other, State) ->
    ?TRACE({"Ignoring unecpected data:", Other}),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.


%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, State) ->
    {stop, {StateName, undefined_event, Event}, State}.

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
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
    %% Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName,
            #state{socket=Socket, addr=Addr} = StateData) ->
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
terminate(_Reason, _StateName, #state{socket=Socket}) ->
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


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

handle(undefined, Chs, ChunkSize, Data) ->
    {Ch, Data2} = erlyvideo_rtmp:header(Data),
    {Ch2, Chs2} = channels(Ch, Chs),
    handle(Ch2, Chs2, ChunkSize, Data2);

handle({ChId, Ts, Length, Type, StId, Buf}=Ch, Chs, ChunkSize, Data) ->
    case Data of
        <<Data2:ChunkSize/binary, _ChType:2, ChIdNext:6, Rest/binary>>
        when ChId =:= ChIdNext ->
            Buf2 = <<Buf/binary, Data2/binary>>,
            Ch2 = {ChId, Ts, Length, Type, StId, Buf2},
            Chs2 = lists:keyreplace(ChId, 1, Chs, Ch2),
            handle(Ch2, Chs2, ChunkSize, Rest); 
        <<Data2:ChunkSize/binary, _ChType:2, _ChIdNext:6, Rest/binary>> ->
            Buf2 = <<Buf/binary, Data2/binary>>,
            Ch2 = {ChId, Ts, Length, Type, StId, Buf2},
            Chs2 = lists:keyreplace(ChId, 1, Chs, Ch2),
            handle(undefined, Chs2, ChunkSize, Rest);
        _Val when Length == size(Buf) + size(Data) ->
            Buf2 = <<Buf/binary, Data/binary>>,
            command({ChId, Ts, Type, StId}, Buf2),
            Ch2 = {ChId, Ts, Length, Type, StId, <<>>},
            Chs2 = lists:keyreplace(ChId, 1, Chs, Ch2),
            {undefined, Chs2, <<>>}; 
        _Val when Length < size(Data) ->
            <<Data3:Length/binary, Rest2/binary>> = Data,
            command({ChId, Ts, Type, StId}, Data3),
            handle(undefined, Chs, ChunkSize, Rest2);
        _ ->           
            {Ch, Chs, Data}
    end.


channels({ChId, Ts, Length, Type, StId, Buf} = Ch, Chs) ->
    case lists:keysearch(ChId, 1, Chs) of     
        {value, {_, Ts2, Length2, Type2, StId2, Buf2}} ->
            Ts3     = case Ts     of undefined -> Ts2;     _ -> Ts end,
            Length3 = case Length of undefined -> Length2; _ -> Length end,
            Type3   = case Type   of undefined -> Type2;   _ -> Type end,
            StId3   = case StId   of undefined -> StId2;   _ -> StId end,  
            Buf3    = case Buf    of undefined -> Buf2; _  ->   Buf end,
            Ch2 = {ChId, Ts3, Length3, Type3, StId3, Buf3},
            {Ch2, lists:keyreplace(ChId, 1, Chs, Ch2)};                                                                                                        
        false ->
            {Ch, [Ch | Chs]}  
    end.
 

command({_, _, ?TYPE_CHUNK_SIZE, _}=_Ch, Data) ->
    gen_fsm:send_event(self(), {update, chunk_size, Data});
                                                                                                
command({_, _, ?TYPE_STREAM_BYTES_READ, _}=_CH, <<Length:32/integer>>) ->
    ?TRACE({"TYPE_STREAM_BYTES_READ: ~p~n", Length});

command({_, _, _, ?TYPE_PING, _}, <<?PING_CLIENT_BUFFER:16/integer, 
                                   StreamId:32/integer, 
                                   ClientBuffer:32/integer>>) -> 
    ?TRACE({"PING_CLIENT_BUFFER: ~p~n", StreamId, ClientBuffer}),
    gen_fsm:send_event(self(), {update, client_buffer, ClientBuffer});

command({_, _, ?TYPE_PING, _}=_Ch, Data) ->
    ?TRACE({"PING: ~p~n", Data}); 

command({_, _, ?TYPE_SERVER_BANDWIDTH, _}=_Ch, _Data) ->
    ?TRACE("TYPE_SERVER_BANDWIDTH: (not implemented yet)");
           
command({_, _, ?TYPE_CLIENT_BANDWIDTH, _}=_Ch, _Data) ->
    ?TRACE("TYPE_CLIENT_BANDWIDTH: (not implemented yet)");

command({_, Ts, Type, StId}=Ch, Data)  when (Type =:= ?TYPE_META_DATA) or 
                                        (Type =:= ?TYPE_AUDIO_DATA) or
                                        (Type =:= ?TYPE_VIDEO_DATA) ->
    ?TRACE("TYPE_Media_DATA:"),
    gen_fsm:send_event(self(), {record, Ts, Type, StId, Data});

command({_, _, ?TYPE_INVOKE, _}=Ch, Data) ->
    try
       {Cmd, Iid, Args} = erlyvideo_amf:parse_signature(Data),
       erlyvideo_rtmp:Cmd(self(), Iid, Args, Ch)   
    catch 
        Err -> error_logger:error_msg("Calling RTMP command failed~p\n", 
                                      [Err])
    end;

command(Other, _Data) ->
    ?TRACE({"igonoring exec other (Chanel):", Other}).

 
calc_timeout(AbsTime, TimerStart, ClientBuffer) ->
    Timeout = AbsTime - ClientBuffer - trunc(timer:now_diff(now(), TimerStart) / 1000),
    if (Timeout > 0) -> 
            Timeout; 
       true -> 
            0 
    end.


send({Ts, Type, StId}, Data, State) -> 
    %% TODO: clean up this mess, streams and their channels need to 
    %% need to be indexed by thier streamid at least I think so ...
    ChId = case Type of
               ?TYPE_META_DATA -> 6;   % provisory
               ?TYPE_AUDIO_DATA -> 7;  % provisory
               ?TYPE_VIDEO_DATA -> 9   % provisory
               end,
    {Chs, Packet} = erlyvideo_rtmp:do_channels_packet({ChId, Ts, Type, StId}, Data, 
                                                      State#state.chs,
                                                      State#state.chunk_size),
    gen_tcp:send(State#state.socket, Packet),      
    State#state{chs = Chs};

send(Ch, Data, State) ->
    Chs = State#state.chs, 
    ChunkSz = State#state.chunk_size,
    {Chs2, Packet} = erlyvideo_rtmp:do_channels_packet(Ch, Data, Chs, ChunkSz),
    gen_tcp:send(State#state.socket, Packet), 
    State#state{chs = Chs2}.


