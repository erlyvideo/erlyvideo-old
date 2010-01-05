%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        RTMPT interface. Used from web-server.
%%% RTMPT protocol is a tunneling protocol for RTMP. Technically it is implemented in following way:
%%% Flash client connects to HTTP server several times a second and sends POST /idle requests with session id,
%%% given in first POST /open request. When player needs to send data is sens POST /send request.
%%% If server needs to send data to client, it buffers these bytes somewhere, associated with session id.
%%% Usually you may not bother about POST /close request, because I have never seen it.
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information.
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
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
-module(rtmpt).
-author('Max Lapshin <max@maxidoors.ru>').
-version(1.0).

-behaviour(gen_server).
-define(RTMPT_TIMEOUT, 12000).

-record(rtmpt, {
	consumer = undefined,   % backend process
	session_id = undefined,
	ip,
	buffer = <<>>,
	bytes_count = 0,
	sequence_number = 0
	}).


-export([open/2, idle/3, send/4, close/2, write/2]).


-export([start_link/2, set_consumer/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

%% @spec(IP::{}, Callback::atom()) -> {ok, RTMP::pid(), SessionID::string()}
%% @doc Opens RTMPT session. Creates internal buffer, that will register under SessionID
%% and will serve as a input/output buffer for RTMP socket. You must pass Callback module that
%% will start client for you. See overview for details.
%% @end
open(IP, Callback) ->
  {ok, RTMPT, SessionID} = rtmpt_sessions:create(IP),
  % {ok, RTMP} = rtmp_socket:start_socket(Consumer, accept, RTMPT),
  {ok, RTMP} = rtmp_sup:start_rtmp_socket(undefined, accept),
  {ok, Pid} = Callback:create_client(RTMP),
  rtmp_socket:setopts(RTMP, [{consumer, Pid}]),
  rtmpt:set_consumer(RTMPT, RTMP),
  gen_fsm:send_event(RTMP, {socket, RTMPT}),
  {ok, Pid, SessionID}.


%% @spec(SessionID::string(), IP::{}, Sequence::integer()) -> {ok, Data::binary()} | {error, Reason}
%% @doc Asks RTMPT buffer for any data, need to be received to client. Usually flash client calls is several times per second.
%% Don't forget to add magic 33 in front of your data:
%% <code>
%%  {ok, Data} = rtmpt:idle(SessionId, Req:get(peer_addr), list_to_int(SequenceNumber)),
%%  Req:ok([{'Content-Type', "application/x-fcs"}, {"Server", "RTMPT/1.0"}], [33, Data]);
%% </code>
%% @end
idle(SessionID, IP, Sequence) ->
  case rtmpt_sessions:find(SessionID, IP) of
    {error, Reason} -> {error, Reason};
    {ok, RTMPT} -> gen_server:call(RTMPT, {recv, Sequence})
  end.

%% @spec(SessionID::string(), IP::{}, Sequence::integer(), Data::binary()) -> {ok, Reply::binary()} | {error, Reason}
%% @doc Push data to RTMP socket and asks RTMPT buffer for any data, need to be received to client. Usually flash client calls is several times per second.
%% Don't forget to add magic 33 in front of your data:
%% <code>
%%  {ok, Data} = rtmpt:send(SessionId, Req:get(peer_addr), list_to_int(SequenceNumber), Req:get(body)),
%%  Req:ok([{'Content-Type', "application/x-fcs"}, {"Server", "RTMPT/1.0"}], [33, Data]);
%% </code>
%% @end
send(SessionID, IP, Sequence, Data) ->
  case rtmpt_sessions:find(SessionID, IP) of
    {error, Reason} -> {error, Reason};
    {ok, RTMPT} -> 
      gen_server:call(RTMPT, {client_data, Data}),
      gen_server:call(RTMPT, {recv, Sequence})
  end.

%% @spec(SessionID::string(), IP::{}) -> noreply | {error, Reason}
%% @doc Closes RTMPT session
%% @end
close(SessionID, IP) ->
  case rtmpt_sessions:find(SessionID, IP) of
    {error, Reason} -> {error, Reason};
    {ok, RTMPT} -> gen_server:cast(RTMPT, close)
  end.

%% @private
write(RTMPT, Data) ->
  gen_server:call(RTMPT, {server_data, Data}).

%% @private
start_link(SessionId, IP) ->
  gen_server:start_link(?MODULE, [SessionId, IP], []).

%% @private
set_consumer(RTMPT, Consumer) ->
  gen_server:call(RTMPT, {set_consumer, Consumer}).

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
init([SessionId, IP]) ->
  % process_flag(trap_exit, true),
  {ok, #rtmpt{session_id = SessionId, ip = IP}, ?RTMPT_TIMEOUT}.
        

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

handle_call({server_data, Bin}, _From, #rtmpt{buffer = Buffer, bytes_count = BytesCount} = State) ->
  Data = iolist_to_binary(Bin),
  {reply, ok, State#rtmpt{buffer = <<Buffer/binary, Data/binary>>, bytes_count = BytesCount + size(Data)}, ?RTMPT_TIMEOUT};


handle_call({client_data, Bin}, _From, #rtmpt{consumer = Upstream} = State) when is_pid(Upstream)  ->
  Upstream ! {rtmpt, self(), Bin},
  {reply, ok, State, ?RTMPT_TIMEOUT};

handle_call({set_consumer, Upstream}, _From, #rtmpt{consumer = undefined} = State) ->
  {reply, ok, State#rtmpt{consumer = Upstream}, ?RTMPT_TIMEOUT};

handle_call(timeout, _From, #rtmpt{consumer = _Consumer} = State) ->
  % gen_fsm:send_event(Consumer, timeout),
  % {stop, normal, State};
  {reply, ok, State, ?RTMPT_TIMEOUT};


handle_call({info}, _From, #rtmpt{sequence_number = SequenceNumber, session_id = SessionId, buffer = Buffer, bytes_count = BytesCount} = State) ->
  Info = {self(), [{session_id, SessionId}, {sequence_number, SequenceNumber}, {total_bytes, BytesCount}, {unread_data, size(Buffer)} | process_info(self(), [message_queue_len, heap_size])]},
  {reply, Info, State, ?RTMPT_TIMEOUT};

handle_call({recv, SequenceNumber}, _From, #rtmpt{buffer = Buffer, consumer = Consumer} = State) ->
  Consumer ! {rtmpt, self(), alive},
  {reply, {ok, Buffer}, State#rtmpt{buffer = <<>>, sequence_number = SequenceNumber}, ?RTMPT_TIMEOUT}.



%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(close, #rtmpt{} = State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
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

handle_info(timeout, State) ->
  {noreply, State};

handle_info(_Info, State) ->
  io:format("RTMPT unknown message: ~p~n", [_Info]),
  {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


