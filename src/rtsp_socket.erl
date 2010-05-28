-module(rtsp_socket).
-author('Max Lapshin <max@maxidoors.ru>').


-export([start_link/0]).
-behaviour(gen_server).

-include("../include/rtsp.hrl").
-include_lib("erlmedia/include/video_frame.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([read/2, connect/3, describe/2, setup/2, play/2, accept/3]).

-record(rtsp_socket, {
  buffer = <<>>,
  url,
  auth = "",
  socket,
  sdp_config = [],
  options,
  rtp_streams = {undefined,undefined,undefined,undefined},
  nal_size = 32,
  consumer,
  consumer_ref,
  acceptor,
  state,
  pending,
  seq,
  session
}).


read(URL, Options) ->
  {ok, RTSP} = start_link(),
  rtsp_socket:connect(RTSP, URL, Options),
  rtsp_socket:describe(RTSP, Options),
  rtsp_socket:setup(RTSP, Options),
  rtsp_socket:play(RTSP, Options),
  {ok, RTSP}.


describe(RTSP, Options) ->
  Timeout = proplists:get_value(timeout, Options, 5000),
  gen_server:call(RTSP, {request, describe}, Timeout).

setup(RTSP, Options) ->
  Timeout = proplists:get_value(timeout, Options, 5000),
  gen_server:call(RTSP, {request, setup}, Timeout).
  
play(RTSP, Options) ->
  Timeout = proplists:get_value(timeout, Options, 5000),
  gen_server:call(RTSP, {request, play}, Timeout).

connect(RTSP, URL, Options) ->
  Timeout = proplists:get_value(timeout, Options, 5000),
  gen_server:call(RTSP, {connect, URL, Options}, Timeout).

accept(RTSP, Socket, Acceptor) ->
  gen_tcp:controlling_process(Socket, RTSP),
  gen_server:call(RTSP, {accept, Socket, Acceptor}).

start_link() ->
  gen_server:start_link(?MODULE, [], []).


init([]) ->
  {ok, #rtsp_socket{}}.

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

handle_call({accept, Socket, Acceptor}, _From, State) ->
  Ref = erlang:monitor(process, Acceptor),
  inet:setopts(Socket, [{active, once}, binary]),
  {reply, ok, State#rtsp_socket{socket = Socket, acceptor = Acceptor, consumer = Acceptor, consumer_ref = Ref}};


handle_call({connect, URL, Options}, _From, RTSP) ->
  Consumer = proplists:get_value(consumer, Options),
  Ref = erlang:monitor(process, Consumer),
  {rtsp, UserInfo, Host, Port, _Path, _Query} = http_uri2:parse(URL),
  ?D({"Connecting to", Host, Port}),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, raw}, {active, once}], 1000),
  ?D({"Connect", URL}),
  Auth = case UserInfo of
    [] -> "";
    _ -> "Authorization: Basic "++binary_to_list(base64:encode(UserInfo))++"\r\n"
  end,
  {reply, ok, RTSP#rtsp_socket{url = URL, options = Options, consumer = Consumer, consumer_ref = Ref, socket = Socket, auth = Auth}};
  
  
handle_call({consume, Consumer}, _From, #rtsp_socket{consumer_ref = OldRef} = RTSP) ->
  (catch erlang:demonitor(OldRef)),
  Ref = erlang:monitor(process, Consumer),
  {reply, ok, RTSP#rtsp_socket{consumer = Consumer, consumer_ref = Ref}};
  

handle_call({request, describe}, From, #rtsp_socket{socket = Socket, url = URL, auth = Auth} = RTSP) ->
  Call = io_lib:format("DESCRIBE ~s RTSP/1.0\r\nCSeq: 1\r\n"++Auth++"\r\n", [URL]),
  gen_tcp:send(Socket, Call),
  io:format("~s~n", [Call]),
  {noreply, RTSP#rtsp_socket{pending = From, state = describe, seq = 1}};

handle_call({request, setup}, From, #rtsp_socket{socket = Socket, url = URL, seq = Seq, auth = Auth} = RTSP) ->
  Call = io_lib:format("SETUP ~s RTSP/1.0\r\nCSeq: ~p\r\nTransport: RTP/AVP/TCP;unicast;interleaved=0-1\r\n"++Auth++"\r\n", [append_trackid(URL,1), Seq + 1]),
  gen_tcp:send(Socket, Call),
  io:format("~s~n", [Call]),
  {noreply, RTSP#rtsp_socket{pending = From, seq = Seq + 1}};

handle_call({request, play}, From, #rtsp_socket{socket = Socket, url = URL, seq = Seq, session = Session, auth = Auth} = RTSP) ->
  Call = io_lib:format("PLAY ~s RTSP/1.0\r\nCSeq: ~pr\r\nSession: ~s\r\n"++Auth++"\r\n", [URL, Seq + 1, Session]),
  gen_tcp:send(Socket, Call),
  io:format("~s~n", [Call]),
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


handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
  
handle_info({tcp, Socket, Bin}, #rtsp_socket{buffer = Buf} = RTSPSocket) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, handle_packet(RTSPSocket#rtsp_socket{buffer = <<Buf/binary, Bin/binary>>})};

handle_info({'DOWN', _, process, Consumer, _Reason}, #rtsp_socket{consumer = Consumer} = Socket) ->
  {stop, normal, Socket};

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
      io:format("--------------------------~n[RTSP] ~p ~s~n~p~n", [_Code, _Message, Headers]),
      Socket1 = configure_rtp(Socket#rtsp_socket{buffer = Rest}, Headers, Body),
      Socket2 = extract_session(Socket1, Headers),
      Socket3 = sync_rtp(Socket2, Headers),
      Socket4 = reply_pending(Socket3),
      handle_packet(Socket4);
    {ok, {request, _Method, _URL, Headers, Body} = Request, Rest} ->
      io:format("--------------------------~n[RTSP] ~p ~p~n~p~n", [_Method, _URL, Headers]),
      Socket1 = configure_rtp(Socket#rtsp_socket{buffer = Rest}, Headers, Body),
      Socket2 = handle_request(Request, Socket1),
      handle_packet(Socket2)
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
      ?D({"Autoconfiguring RTP", Frames, RtpStreams1}),

      lists:foreach(fun(Frame) ->
        Consumer ! Frame
      end, Frames),
      
      Socket1 = case Frames of
        [#video_frame{body = Config, content = video, flavor = config} |_] ->
          {NalSize, _} = h264:unpack_config(Config),
          Socket#rtsp_socket{nal_size = NalSize};
        _ ->
          Socket
      end,
      Socket1#rtsp_socket{sdp_config = SDPConfig, rtp_streams = RtpStreams1};
    undefined ->
      Socket;
    Else ->
      ?D({"Unknown body type", Else}),
      Socket
  end.
  

seq(Headers) ->
  proplists:get_value('Cseq', Headers, 1).
  
handle_request({request, 'OPTIONS', _URL, Headers, _Body}, State) ->
  reply(State, "200 OK", [{'Cseq', seq(Headers)}, {'Public', "SETUP, TEARDOWN, PLAY, PAUSE, RECORD, OPTIONS, DESCRIBE"}]);

handle_request({request, 'ANNOUNCE', URL, Headers, Body}, #rtsp_socket{acceptor = Acceptor} = State) ->
  case gen_server:call(Acceptor, {announce, URL, Headers, Body}) of
    ok ->
      reply(State#rtsp_socket{session = "42"}, "200 OK", [{'Cseq', seq(Headers)}]);
    {error, authentication} ->
      reply(State, "401 Unauthorized", [{"WWW-Authenticate", "Basic realm=\"Erlyvideo Streaming Server\""}, {'Cseq', seq(Headers)}])
  end;

handle_request({request, 'PAUSE', _URL, Headers, _Body}, State) ->
  reply(State, "200 OK", [{'Cseq', seq(Headers)}]);

handle_request({request, 'RECORD', URL, Headers, _}, #rtsp_socket{acceptor = Acceptor} = State) ->
  case gen_server:call(Acceptor, {record, URL, Headers}) of
    {ok, Consumer} ->
      erlang:monitor(process, Consumer),
      reply(State#rtsp_socket{consumer = Consumer}, "200 OK", [{'Cseq', seq(Headers)}]);
    {error, authentication} ->
      reply(State, "401 Unauthorized", [{"WWW-Authenticate", "Basic realm=\"Erlyvideo Streaming Server\""}, {'Cseq', seq(Headers)}])
  end;
      
handle_request({request, 'SETUP', _URL, Headers, _}, State) ->
  Transport = proplists:get_value('Transport', Headers),
  Date = httpd_util:rfc1123_date(),
  ReplyHeaders = [{"Transport", Transport},{'Cseq', seq(Headers)}, {'Date', Date}, {'Expires', Date}, {'Cache-Control', "no-cache"}],
  reply(State, "200 OK", ReplyHeaders);

handle_request({request, 'TEARDOWN', _URL, Headers, _Body}, #rtsp_socket{consumer = Consumer} = State) ->
  Consumer ! stop,
  reply(State, "200 OK", [{'Cseq', seq(Headers)}]).
  

reply(#rtsp_socket{socket = Socket, session = SessionId} = State, Code, Headers) ->
  Headers2 = case SessionId of
    undefined -> Headers;
    _ -> [{'Session', SessionId} | Headers]
  end,
  ReplyList = lists:map(fun binarize_header/1, Headers2),
  Reply = iolist_to_binary(["RTSP/1.0 ", Code, <<"\r\n">>, ReplyList, <<"\r\n">>]),
  io:format("[RTSP] ~s", [Reply]),
  gen_tcp:send(Socket, Reply),
  State.


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


  
extract_session(Socket, Headers) ->
  case proplists:get_value('Session', Headers) of
    undefined -> 
      Socket;
    FullSession ->
      % ?D({"Session", FullSession}),
      Socket#rtsp_socket{session = hd(string:tokens(binary_to_list(FullSession), ";"))}
  end.
  
sync_rtp(#rtsp_socket{rtp_streams = Streams} = Socket, Headers) ->
  case proplists:get_value(<<"Rtp-Info">>, Headers) of
    undefined -> 
      ?D({"No Rtp-Info on play command"}),
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


append_trackid(URL, TrackID) ->
  Track = "/trackID="++integer_to_list(TrackID),
  case string:tokens(URL, "?") of
    [URL1, URL2] -> URL1 ++ Track ++ "?" ++ URL2;
    [URL] -> URL ++ Track
  end.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").


append_trackid1_test() ->
  ?assertEqual("rtsp://cam1:554/h264.sdp/trackID=1", append_trackid("rtsp://cam1:554/h264.sdp", 1)).

append_trackid2_test() ->
  ?assertEqual("rtsp://cam1:554/h264.sdp/trackID=1?res=half&x0=0", append_trackid("rtsp://cam1:554/h264.sdp?res=half&x0=0", 1)).




