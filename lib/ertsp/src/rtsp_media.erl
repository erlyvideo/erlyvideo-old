-module(rtsp_media).
-author('Max Lapshin <max@maxidoors.ru>').

-export([start_link/3]).
-behaviour(rtsp_socket).

-include("../include/rtsp.hrl").
-include_lib("erlyvideo/include/video_frame.hrl").

% ems_sup:start_rtsp_client("http://localhost:8080").


-record(rtsp_client, {
  socket,
  url,
  options,
  seq = 1,
  method,
  audio_config = undefined,
  video_config = undefined,
  content_length = 0,
  buffer = <<>>,
  streams,
  rtp_streams = {undefined, undefined, undefined, undefined},
  session,
  clients = []
}).



%% rtsp_socket callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([handle_rtsp_response/2, handle_rtp_packet/2, handle_rtsp_request/2]).

% {ok, Socket} = gen_tcp:connect("ya.ru", 80, [binary, {packet, http_bin}, {active, false}], 1000),
% gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
% {ok, Reply} = gen_tcp:recv(Socket, 0, 1000),
% Reply.

% {ok, Pid1} = ems_sup:start_rtsp_client("http://localhost:8080").

start_link(URL, Type, Opts) ->
  rtsp_socket:start_link(?MODULE, [URL, Type, Opts]).

init([URL, rtsp, Opts]) ->
  process_flag(trap_exit, true),
  {ok, Re} = re:compile("rtsp://([^/]*):(\\d+)/(.*)"),
  {match, [_, RTSPHost, Port, _Path]} = re:run(URL, Re, [{capture, all, list}]),
  
  {ok, Socket} = gen_tcp:connect(RTSPHost, list_to_integer(Port), [binary, {packet, raw}, {active, once}], 1000),
  
  self() ! describe,
  {ok, #rtsp_client{socket = Socket, url = URL, options = Opts, method = describe}};
  
init([_, rtsp_server, _]) ->
  {ok, #rtsp_client{}}.
  
      


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

  
handle_info(describe, #rtsp_client{socket = Socket, url = URL} = RTSP) ->
  gen_tcp:send(Socket, io_lib:format("DESCRIBE ~s RTSP/1.0\r\nCSeq: 1\r\n\r\n", [URL])),
  {noreply, RTSP#rtsp_client{method = describe}};
  


handle_info({'EXIT', Client, _Reason}, #rtsp_client{clients = Clients} = RTSP) ->
  case length(Clients) of
    Length when Length =< 1 ->
      {stop, normal, RTSP#rtsp_client{clients =[]}};
    _ ->
      {noreply, RTSP#rtsp_client{clients = lists:delete(Client, Clients)}}
  end;

  
  
handle_info(stop, #rtsp_client{socket = Socket} = TSLander) ->
  gen_tcp:close(Socket),
  ?D({"RTSP stopped"}),
  {stop, normal, TSLander#rtsp_client{socket = undefined}};

handle_info(#video_frame{} = Frame, RTSP) ->
  send_frame(Frame, RTSP),
  {noreply, RTSP};

handle_info(_Info, State) ->
  ?D({"Undefined info", _Info, State}),
  {noreply, State}.


handle_rtp_packet(#rtsp_client{rtp_streams = Streams} = State, {rtp, Channel, Packet}) ->
  Streams1 = case element(Channel+1, Streams) of
    {rtcp, RTPNum} ->
      {Type, RtpState} = element(RTPNum+1, Streams),
      RtpState1 = rtp_server:decode(rtcp, RtpState, Packet),
      setelement(RTPNum+1, Streams, {Type, RtpState1});
    {Type, RtpState} ->
      RtpState1 = rtp_server:decode(Type, RtpState, Packet),
      setelement(Channel+1, Streams, {Type, RtpState1});
    undefined ->
      Streams
  end,
  State#rtsp_client{rtp_streams = Streams1}.


handle_rtsp_response(#rtsp_client{socket = Socket, url = URL, seq = Seq, method = describe} = RTSP, 
               {response, 200, _, _Headers, Body}) ->
  Streams = sdp:decode(Body),
  
  RTP = 0,
  RTCP = 1,
  Streams1 = ems_rtsp:config_media(self(), Streams),
  Stream = hd(Streams1),
  RtpConfig = rtp_server:init(Stream, self()),
  RtpStreams = setelement(RTP+1, RTSP#rtsp_client.rtp_streams, {Stream#rtsp_stream.type, RtpConfig}),
  RtpStreams1 = setelement(RTCP+1, RtpStreams, {rtcp, RTP}),  
  
  gen_tcp:send(Socket, io_lib:format("SETUP ~s RTSP/1.0\r\nCSeq: ~p\r\nTransport: RTP/AVP/TCP;unicast\r\n\r\n", [URL, Seq + 1])),
  % ?D({"Parsed streams", RtpStreams1}),
  RTSP#rtsp_client{streams = Streams1, method = setup, seq = Seq + 1, rtp_streams = RtpStreams1};
  
  
handle_rtsp_response(#rtsp_client{url = URL, method = setup, socket = Socket, seq = Seq}=RTSP, {response, 200, _, Headers, _}) ->
  % ?D({"Setup done"}),
  FullSession = proplists:get_value('Session', Headers, <<"111">>),
  Session = hd(string:tokens(binary_to_list(FullSession), ";")),
  gen_tcp:send(Socket, io_lib:format("PLAY ~s RTSP/1.0\r\nCSeq: ~pr\r\nSession: ~s\r\n\r\n", [URL, Seq + 1, Session])),
  ?D({"Send play command"}),
  RTSP#rtsp_client{seq = Seq + 1, method = play};

handle_rtsp_response(#rtsp_client{method = play} = RTSP, {response, 200, _, _, _}) ->
  ?D({"PLay started"}),
  RTSP;
  
handle_rtsp_response(RTSP, {response, 200, _, _, _} = R) ->
  ?D({"Unknown response", R, RTSP}),
  RTSP.

handle_rtsp_request(RTSP, _Request) ->
  RTSP.

send_frame(Frame, #rtsp_client{clients = Clients} = TSLander) ->
  lists:foreach(fun(Client) -> Client ! Frame end, Clients),
  TSLander.

