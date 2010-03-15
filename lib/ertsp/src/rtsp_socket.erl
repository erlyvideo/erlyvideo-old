-module(rtsp_socket).
-author('Max Lapshin <max@maxidoors.ru>').


-export([start_link/2]).
-behaviour(gen_server).

-include("../include/rtsp.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([connect/2, describe/1, setup/1, play/1]).

-record(rtsp_socket, {
  buffer = <<>>,
  url,
  socket,
  sdp_config = [],
  options,
  rtp_streams = {undefined,undefined,undefined,undefined},
  consumer,
  state,
  pending,
  seq,
  session
}).


connect(URL, Options) ->
  {ok, RTSP} = start_link(URL, Options),
  rtsp_socket:describe(RTSP),
  rtsp_socket:setup(RTSP),
  rtsp_socket:play(RTSP),
  {ok, RTSP}.


describe(RTSP) ->
  gen_server:call(RTSP, {request, describe}, 5000).

setup(RTSP) ->
  gen_server:call(RTSP, {request, setup}, 5000).
  
play(RTSP) ->
  gen_server:call(RTSP, {request, play}, 5000).


start_link(URL, Options) ->
  gen_server:start_link(?MODULE, [URL, Options], []).



init([URL, Options]) ->
  Consumer = proplists:get_value(consumer, Options),
  erlang:monitor(process, Consumer),
  {ok, Re} = re:compile("rtsp://([^/]*):(\\d+)/(.*)"),
  {match, [_, Host, Port, _Path]} = re:run(URL, Re, [{capture, all, list}]),
  {ok, Socket} = gen_tcp:connect(Host, list_to_integer(Port), [binary, {packet, raw}, {active, once}], 1000),
  ?D({"Connect", URL}),
  {ok, #rtsp_socket{url = URL, options = Options, consumer = Consumer, socket = Socket}}.

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

handle_call({request, describe}, From, #rtsp_socket{socket = Socket, url = URL} = RTSP) ->
  gen_tcp:send(Socket, io_lib:format("DESCRIBE ~s RTSP/1.0\r\nCSeq: 1\r\n\r\n", [URL])),
  % ems_log:access(default, "DESCRIBE ~s RTSP/1.0", [URL]),
  io:format("DESCRIBE ~s RTSP/1.0~n", [URL]),
  {noreply, RTSP#rtsp_socket{pending = From, state = describe, seq = 1}};

handle_call({request, setup}, From, #rtsp_socket{socket = Socket, url = URL, seq = Seq} = RTSP) ->
  gen_tcp:send(Socket, io_lib:format("SETUP ~s/trackID=1 RTSP/1.0\r\nCSeq: ~p\r\nTransport: RTP/AVP/TCP;unicast;interleaved=0-1\r\n\r\n", [URL, Seq + 1])),
  % ems_log:access(default, "DESCRIBE ~s RTSP/1.0", [URL]),
  io:format("SETUP ~s/trackID=1 RTSP/1.0~n", [URL]),
  {noreply, RTSP#rtsp_socket{pending = From, seq = Seq + 1}};

handle_call({request, play}, From, #rtsp_socket{socket = Socket, url = URL, seq = Seq, session = Session} = RTSP) ->
  gen_tcp:send(Socket, io_lib:format("PLAY ~s RTSP/1.0\r\nCSeq: ~pr\r\nSession: ~s\r\n\r\n", [URL, Seq + 1, Session])),
  io:format("PLAY ~s RTSP/1.0~n", [URL]),
  {noreply, RTSP#rtsp_socket{pending = From, seq = Seq + 1}};

handle_call(Request, _From, #rtsp_socket{} = RTSP) ->
  {stop, {unknown_call, Request}, RTSP}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(Request, #rtsp_socket{} = Socket) ->
  {stop, {unknown_cast, Request}, Socket}.


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

handle_info(Message, #rtsp_socket{} = Socket) ->
  ?D({"Unknown message", Message}),
  {noreply, Socket}.



handle_packet(#rtsp_socket{buffer = Data} = Socket) ->
  case rtsp:decode(Data) of
    {more, Data} ->
      Socket;
    {ok, {rtp, _Channel, _} = RTP, Rest} ->
      Socket1 = handle_rtp(Socket#rtsp_socket{buffer = Rest}, RTP),
      handle_packet(Socket1);
    {ok, {response, _Code, _Message, Headers, Body} = _Response, Rest} ->
      % ?D({"ZZ", _Code, _Message, Headers}),
      Socket1 = configure_rtp(Socket#rtsp_socket{buffer = Rest}, Headers, Body),
      Socket2 = extract_session(Socket1, Headers),
      Socket3 = sync_rtp(Socket2, Headers),
      Socket4 = reply_pending(Socket3),
      handle_packet(Socket4);
    {ok, {request, _Method, _URL, Headers, Body} = _Request, Rest} ->
      Socket1 = configure_rtp(Socket#rtsp_socket{buffer = Rest}, Headers, Body),
      handle_packet(Socket1)
  end.


reply_pending(#rtsp_socket{pending = undefined} = Socket) ->
  Socket;

reply_pending(#rtsp_socket{state = {Method, Count}} = Socket) when Count > 1 ->
  Socket#rtsp_socket{state = {Method, Count - 1}};

reply_pending(#rtsp_socket{pending = From} = Socket) ->
  gen_server:reply(From, ok),
  Socket#rtsp_socket{pending = undefined}.

configure_rtp(Socket, _Headers, undefined) ->
  Socket;
  
configure_rtp(#rtsp_socket{rtp_streams = RTPStreams, consumer = Consumer} = Socket, Headers, Body) ->
  case proplists:get_value('Content-Type', Headers) of
    <<"application/sdp">> ->
      {SDPConfig, RtpStreams1, Frames} = rtp_server:configure(Body, RTPStreams, Consumer),
      ?D({"Autoconfiguring RTP"}),

      lists:foreach(fun(Frame) ->
        Consumer ! Frame
      end, Frames),
      
      Socket#rtsp_socket{sdp_config = SDPConfig, rtp_streams = RtpStreams1};
    undefined ->
      Socket;
    Else ->
      ?D({"Unknown body type", Else}),
      Socket
  end.
  
extract_session(Socket, Headers) ->
  case proplists:get_value('Session', Headers) of
    undefined -> 
      Socket;
    FullSession ->
      ?D({"Session", FullSession}),
      Socket#rtsp_socket{session = hd(string:tokens(binary_to_list(FullSession), ";"))}
  end.
  
sync_rtp(#rtsp_socket{rtp_streams = Streams} = Socket, Headers) ->
  case proplists:get_value(<<"Rtp-Info">>, Headers) of
    undefined -> 
      Socket;
    Info ->
      {ok, Re} = re:compile("([^=]+)=(.*)"),
      F = fun(S) ->
        {match, [_, K, V]} = re:run(S, Re, [{capture, all, list}]),
        {K, V}
      end,
      RtpInfo = [[F(S1) || S1 <- string:tokens(S, ";")] || S <- string:tokens(binary_to_list(Info), ",")],
      ?D({"Rtp", RtpInfo}),
      Streams1 = rtp_server:presync(Streams, RtpInfo),
      Socket#rtsp_socket{rtp_streams = Streams1}
  end.

handle_rtp(#rtsp_socket{rtp_streams = Streams, consumer = Consumer} = Socket, {rtp, Channel, Packet}) ->
  Streams1 = case element(Channel+1, Streams) of
    {rtcp, RTPNum} ->
      {Type, RtpState} = element(RTPNum+1, Streams),
      {RtpState1, _} = rtp_server:decode(rtcp, RtpState, Packet),
      setelement(RTPNum+1, Streams, {Type, RtpState1});
    {Type, RtpState} ->
      % ?D({"Decode rtp on", Channel, Type}),
      {RtpState1, Frames} = rtp_server:decode(Type, RtpState, Packet),
      % ?D({"Frame", Frames}),
      lists:foreach(fun(Frame) ->
        Consumer ! Frame
      end, Frames),
      setelement(Channel+1, Streams, {Type, RtpState1});
    undefined ->
      Streams
  end,
  Socket#rtsp_socket{rtp_streams = Streams1}.
      

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











