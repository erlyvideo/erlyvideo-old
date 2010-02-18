-module(rtsp_socket).
-author('Max Lapshin <max@maxidoors.ru>').


-export([start_link/2]).
-behaviour(gen_server).

-include("../include/rtsp.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(rtsp_socket, {
  buffer = <<>>,
  module,
  state
}).

-export([behaviour_info/1]).
behaviour_info(callbacks) -> [{init, 1}, {handle_call, 3}, {handle_cast, 2}, {handle_info, 2}, 
                              {handle_rtp_packet, 2}, {handle_rtsp_response, 2}, {handle_rtsp_request, 2}];
behaviour_info(_Other) -> undefined.


start_link(Module, Args) ->
  gen_server:start_link(?MODULE, [Module, Args], []).


init([Module, Args]) ->
  {ok, State} = Module:init(Args),
  {ok, #rtsp_socket{module = Module, state = State}}.

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

handle_call(Request, From, #rtsp_socket{module = Module, state = State} = Socket) ->
  case Module:handle_call(Request, From, State) of
    {reply, Reply, State1} ->
      {reply, Reply, Socket#rtsp_socket{state = State1}};
    {reply, Reply, State1, Timeout} ->
      {reply, Reply, Socket#rtsp_socket{state = State1}, Timeout};
    {noreply, State1} ->
      {noreply, Socket#rtsp_socket{state = State1}};
    {noreply, State1, Timeout} ->
      {noreply, Socket#rtsp_socket{state = State1}, Timeout};
    {stop, Reason, Reply, State1} ->
      {stop, Reason, Reply, Socket#rtsp_socket{state = State1}};
    {stop, Reason, State1} ->
      {stop, Reason, Socket#rtsp_socket{state = State1}}
  end.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(Request, #rtsp_socket{module = Module, state = State} = Socket) ->
  case Module:handle_cast(Request, State) of
    {noreply, State1} ->
      {noreply, Socket#rtsp_socket{state = State1}};
    {noreply, State1, Timeout} ->
      {noreply, Socket#rtsp_socket{state = State1}, Timeout};
    {stop, Reason, State1} ->
      {stop, Reason, Socket#rtsp_socket{state = State1}}
  end.


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


handle_info({tcp_closed, Socket}, Socket) ->
  {stop, normal, Socket};
  
handle_info({tcp, Socket, Bin}, #rtsp_socket{buffer = Buf} = RTSPSocket) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, handle_packet(RTSPSocket#rtsp_socket{buffer = <<Buf/binary, Bin/binary>>})};

handle_info(Message, #rtsp_socket{module = Module, state = State} = Socket) ->
  case Module:handle_info(Message, State) of
    {noreply, State1} ->
      {noreply, Socket#rtsp_socket{state = State1}};
    {noreply, State1, Timeout} ->
      {noreply, Socket#rtsp_socket{state = State1}, Timeout};
    {stop, Reason, State1} ->
      {stop, Reason, Socket#rtsp_socket{state = State1}}
  end.




handle_packet(#rtsp_socket{buffer = Data, state = State, module = Module} = Socket) ->
  case rtsp:decode(Data) of
    {more, Data} ->
      Socket;
    {ok, {rtp, _Channel, _} = RTP, Rest} ->
      NewState = Module:handle_rtp_packet(State, RTP),
      handle_packet(Socket#rtsp_socket{buffer = Rest, state = NewState});
    {ok, {response, _Code, _, _, _} = Response, Rest} ->
      NewState = Module:handle_rtsp_response(State, Response),
      handle_packet(Socket#rtsp_socket{buffer = Rest, state = NewState});
    {ok, {request, _Method, _, _, _} = Request, Rest} ->
      NewState = Module:handle_rtsp_request(State, Request),
      handle_packet(Socket#rtsp_socket{buffer = Rest, state = NewState})
  end.


%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ?D({"RTSP stopping"}),
  ok.


%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.











