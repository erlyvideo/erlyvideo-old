-module(rtsp_media).
-author('Max Lapshin <max@maxidoors.ru>').
-export([start_link/3]).
-behaviour(gen_server).

-include("../include/rtsp.hrl").
-include_lib("erlyvideo/include/video_frame.hrl").

% ems_sup:start_rtsp_client("http://localhost:8080").


-record(rtsp_client, {
  socket,
  url,
  seq = 1,
  state = request,
  method,
  audio_config = undefined,
  video_config = undefined,
  content_length = 0,
  buffer = <<>>,
  streams,
  rtp_streams = {undefined, undefined, undefined, undefined},
  media,
  session,
  clients = []
}).



%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


% {ok, Socket} = gen_tcp:connect("ya.ru", 80, [binary, {packet, http_bin}, {active, false}], 1000),
% gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
% {ok, Reply} = gen_tcp:recv(Socket, 0, 1000),
% Reply.

% {ok, Pid1} = ems_sup:start_rtsp_client("http://localhost:8080").

start_link(URL, Type, Opts) ->
  gen_server:start_link(?MODULE, [URL, Type, Opts], []).

init([URL, rtsp, Opts]) ->
  process_flag(trap_exit, true),
  {ok, Re} = re:compile("rtsp://([^/]*):(\\d+)/(.*)"),
  {match, [_, RTSPHost, Port, _Path]} = re:run(URL, Re, [{capture, all, list}]),
  
  {ok, Socket} = gen_tcp:connect(RTSPHost, list_to_integer(Port), [binary, {packet, line}, {active, false}], 1000),
  ok = inet:setopts(Socket, [{active, once}]),
  
  Host = proplists:get_value(host, Opts),
  Name = proplists:get_value(name, Opts),
  Media = media_provider:open(Host, Name, live),
  
  gen_tcp:send(Socket, io_lib:format("DESCRIBE ~s RTSP/1.0\r\nCSeq: 1\r\n\r\n", [URL])),
  {ok, #rtsp_client{socket = Socket, url = URL, method = describe, media = Media}}.
      


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

handle_call({create_player, Options}, _From, #rtsp_client{url = URL, clients = Clients} = Lander) ->
  {ok, Pid} = ems_sup:start_stream_play(self(), Options),
  link(Pid),
  ?D({"Creating media player for", URL, "client", proplists:get_value(consumer, Options), Pid}),
  case Lander#rtsp_client.video_config of
    undefined -> ok;
    VideoConfig -> 
      Pid ! VideoConfig,
      Pid ! h264:metadata(VideoConfig#video_frame.body)
  end,
  case Lander#rtsp_client.audio_config of
    undefined -> ok;
    AudioConfig -> Pid ! AudioConfig
  end,
  {reply, {ok, Pid}, Lander#rtsp_client{clients = [Pid | Clients]}};

handle_call(length, _From, MediaInfo) ->
  {reply, 0, MediaInfo};

handle_call(clients, _From, #rtsp_client{clients = Clients} = TSLander) ->
  Entries = lists:map(fun(Pid) -> file_play:client(Pid) end, Clients),
  {reply, Entries, TSLander};

handle_call({set_owner, _}, _From, TSLander) ->
  {reply, ok, TSLander};



handle_call(Request, _From, State) ->
  ?D({"Undefined call", Request, _From}),
  {stop, {unknown_call, Request}, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
  ?D({"Undefined cast", _Msg}),
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

handle_info({'EXIT', Client, _Reason}, #rtsp_client{clients = Clients} = TSLander) ->
  {noreply, TSLander#rtsp_client{clients = lists:delete(Client, Clients)}};


handle_info({tcp_closed, Socket}, #rtsp_client{socket = Socket} = TSLander) ->
  {stop, normal, TSLander#rtsp_client{socket = undefined}};
  
handle_info({tcp, Socket, <<$$, ChannelId, Length:16, RTP:Length/binary, Request/binary>>}, #rtsp_client{rtp_streams = Streams, state = binary, buffer = <<>>} = State) ->
  % ?D({"RTP", ChannelId, Length, Streams}),
  Streams1 = case element(ChannelId+1, Streams) of
    {rtcp, RTPNum} ->
      {Type, RtpState} = element(RTPNum+1, Streams),
      RtpState1 = rtp_server:decode(rtcp, RtpState, RTP),
      setelement(RTPNum+1, Streams, {Type, RtpState1});
    {Type, RtpState} ->
      RtpState1 = rtp_server:decode(Type, RtpState, RTP),
      setelement(ChannelId+1, Streams, {Type, RtpState1});
    undefined ->
      Streams
  end,
  inet:setopts(Socket, [{active, once}]),
  handle_info({tcp, Socket, Request}, State#rtsp_client{rtp_streams = Streams1});

handle_info({tcp, Socket, <<$$, _/binary>> = Bin}, #rtsp_client{state = binary, buffer = <<>>} = State) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, State#rtsp_client{buffer = Bin}};

handle_info({tcp, Socket, <<>>}, #rtsp_client{state = binary, buffer = <<>>} = State) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, State};

handle_info({tcp, Socket, Bin}, #rtsp_client{state = binary, buffer = Buf} = State) ->
  inet:setopts(Socket, [{active, once}]),
  handle_info({tcp, Socket, <<Buf/binary, Bin/binary>>}, State#rtsp_client{buffer = <<>>});
  
  
handle_info({tcp, Socket, <<"RTSP/1.0 200 OK\r\n">>}, #rtsp_client{state = request} = RTSP) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, RTSP#rtsp_client{state = headers, buffer = <<>>}};

handle_info({tcp, Socket, Header}, #rtsp_client{buffer = Buf, state = headers, method = Method, url = URL, seq = Seq, session = Session} = RTSP) ->
  Packet = <<Buf/binary, Header/binary>>,
  State = case erlang:decode_packet(httph_bin, Packet, []) of
    {ok, {http_header, _, Name, _, Value}, More} -> 
      Key = case Name of
        _ when is_binary(Name) -> binary_to_atom(Name, utf8);
        _ when is_atom(Name) -> Name
      end,
      RTSP1 = read_header(RTSP#rtsp_client{buffer = More}, Key, Value),
      case {More, Method} of
        {<<"\r\n">>, setup} ->
          gen_tcp:send(Socket, io_lib:format("PLAY ~s RTSP/1.0\r\nCSeq: ~pr\r\nSession: ~s\r\n\r\n", [URL, Seq + 1, Session])),
          RTSP1#rtsp_client{buffer = More, state = request, seq = Seq + 1, method = play};
        _ ->
          RTSP1
      end;  
    {ok, http_eoh, More} when Method == describe -> 
      RTSP#rtsp_client{buffer = More, state = body};
    {ok, http_eoh, Bin} when Method == play ->
      inet:setopts(Socket, [{packet, raw}]),
      ?D({"Turn on streaming"}),
      RTSP#rtsp_client{buffer = Bin, state = binary};
    {more, undefined} ->
      RTSP#rtsp_client{buffer = Packet}
  end,
  inet:setopts(Socket, [{active, once}]),
  {noreply, State};
  
handle_info({tcp, Socket, Bin}, #rtsp_client{buffer = Buf, state = body, content_length = Length} = RTSP) ->
  Body = <<Buf/binary, Bin/binary>>,
  State = case size(Body) of
    Length ->
      read_body(RTSP#rtsp_client{buffer = Body});
    _ ->
      RTSP#rtsp_client{buffer = Body}
  end,
  inet:setopts(Socket, [{active, once}]),
  {noreply, State};
  
handle_info(stop, #rtsp_client{socket = Socket} = TSLander) ->
  gen_tcp:close(Socket),
  ?D({"RTSP stopped"}),
  {stop, normal, TSLander#rtsp_client{socket = undefined}};

handle_info(_Info, State) ->
  ?D({"Undefined info", _Info, State}),
  {noreply, State}.

read_header(RTSP, 'Content-Length', Length) ->
  RTSP#rtsp_client{content_length = list_to_integer(binary_to_list(Length))};
  
read_header(RTSP, 'Session', Session) ->
  RTSP#rtsp_client{session = hd(string:tokens(binary_to_list(Session), ";"))};
  
read_header(RTSP, Key, Value) ->
  ?D({Key, Value}),
  RTSP.


decode_body(Body) ->
  {ok, Re} = re:compile("(\\w)=(.*)\\r\\n$"),
  Split = split_body(Body, []),
  % ?D({"Split", Split}),
  decode_body(Split, [], Re).
  
decode_body([], List, _Re) ->
  lists:reverse(List);
  
decode_body([Message | Body], List, Re) ->
  % ?D({"SDP", Message}),
  {match, [_, Key, Value]} = re:run(Message, Re, [{capture, all, binary}]),
  decode_body(Body, [{Key, Value} | List], Re).

split_body(<<>>, List) ->
  lists:reverse(List);
  
split_body(Body, List) ->
  case erlang:decode_packet(line, Body, []) of
    {ok, Line, More} ->
      split_body(More, [Line | List]);
    {more, undefined} ->
      lists:reverse([Body | List])
  end.

read_body(#rtsp_client{buffer = Body, socket = Socket, url = URL, seq = Seq, media = Media} = RTSP) ->
  Decoded = decode_body(Body),
  % ?D({"Decoded", Decoded}),
  Streams = sdp:decode(Decoded),
  
  RTP = 0,
  RTCP = 1,
  Streams1 = ems_rtsp:config_media(Media, Streams),
  Stream = hd(Streams1),
  RtpConfig = rtp_server:init(Stream, Media),
  RtpStreams = setelement(RTP+1, RTSP#rtsp_client.rtp_streams, {Stream#rtsp_stream.type, RtpConfig}),
  RtpStreams1 = setelement(RTCP+1, RtpStreams, {rtcp, RTP}),  
  
  gen_tcp:send(Socket, io_lib:format("SETUP ~s RTSP/1.0\r\nCSeq: ~p\r\nTransport: RTP/AVP/TCP;unicast\r\n\r\n", [URL, Seq + 1])),
  ?D({"Parsed streams", RtpStreams1}),
  RTSP#rtsp_client{buffer = <<>>, streams = Streams1, state = request, method = setup, seq = Seq + 1, rtp_streams = RtpStreams1}.

send_frame(Frame, #rtsp_client{clients = Clients} = TSLander) ->
  lists:foreach(fun(Client) -> Client ! Frame end, Clients),
  TSLander.

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
