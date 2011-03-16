%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTSP socket module
%%%
%%% 
%%% 1. connect
%%% 2. describe
%%% 3. each setup
%%% 4. play, possible Rtp-Sync
%%% 5. get each packet
%%% 6. decode
%%% 
%%% 
%%% @end
%%% @reference  See <a href="http://erlyvideo.org/rtsp" target="_top">http://erlyvideo.org</a> for common information.
%%% @end
%%%
%%% This file is part of erlang-rtsp.
%%%
%%% erlang-rtsp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtsp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtsp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtsp_socket).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).

-include("../include/rtsp.hrl").
-include("log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("erlmedia/include/sdp.hrl").

-export([start_link/1, set_socket/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([read/2, connect/3, describe/2, setup/3, play/2]).


-define(FRAMES_BUFFER, 15).
-define(REORDER_FRAMES, 10).
-define(DEFAULT_TIMEOUT, 30000).

-define(SERVER_NAME, "Erlyvideo").

-record(rtsp_socket, {
  callback,
  direction,
  buffer = <<>>,
  addr,
  port,
  url,
  auth = "",
  frames = [],
  socket,
  options,
  rtp_streams = {},
  control_map,
  media         :: pid(),
  media_info,
  rtp           :: pid(),
  rtp_ref       :: reference(),
  sent_audio_config = false,
  audio_rtp_stream,
  video_rtp_stream,
  state,
  pending,
  pending_reply = ok,
  seq = 0,
  timeout = ?DEFAULT_TIMEOUT,
  session
}).

read(URL, Options) when is_binary(URL) ->
  read(binary_to_list(URL), Options);

read(URL, Options) ->
  try read_raw(URL, Options) of
    {ok, RTSP, MediaInfo} -> {ok, RTSP, MediaInfo}
  catch
    _Class:{error,Reason} -> {error, Reason};
    exit:Reason -> {error, Reason};
    Class:Reason -> {Class, Reason}
  end.

read_raw(URL, Options) ->
  {ok, RTSP} = rtsp_sup:start_rtsp_socket(undefined),
  ConnectResult = rtsp_socket:connect(RTSP, URL, Options),
  ok == ConnectResult orelse erlang:error(ConnectResult),
  {ok, MediaInfo, Streams} = rtsp_socket:describe(RTSP, Options),
  [ok = rtsp_socket:setup(RTSP, Stream, Options) || Stream <- Streams],
  ok = rtsp_socket:play(RTSP, Options),
  {ok, RTSP, MediaInfo}.


describe(RTSP, Options) ->
  Timeout = proplists:get_value(timeout, Options, 5000)*2,
  gen_server:call(RTSP, {request, describe}, Timeout).

setup(RTSP, Stream, Options) ->
  Timeout = proplists:get_value(timeout, Options, 5000)*2,
  gen_server:call(RTSP, {request, setup, Stream}, Timeout).

play(RTSP, Options) ->
  Timeout = proplists:get_value(timeout, Options, 5000)*2,
  gen_server:call(RTSP, {request, play}, Timeout).

connect(RTSP, URL, Options) ->
  Timeout = proplists:get_value(timeout, Options, 10000)*2,
  gen_server:call(RTSP, {connect, URL, Options}, Timeout).

start_link(Callback) ->
  gen_server:start_link(?MODULE, [Callback], []).


set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
  gen_tcp:controlling_process(Socket, Pid),
  gen_server:cast(Pid, {socket_ready, Socket}).


init([Callback]) ->
  {ok, #rtsp_socket{callback = Callback, timeout = ?DEFAULT_TIMEOUT}}.


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


handle_call({connect, URL, Options}, _From, RTSP) ->
  Consumer = proplists:get_value(consumer, Options),
  Ref = erlang:monitor(process, Consumer),
  Timeout = proplists:get_value(timeout, Options, ?DEFAULT_TIMEOUT),
  {rtsp, UserInfo, Host, Port, _Path, _Query} = http_uri2:parse(URL),

  Auth = case UserInfo of
    [] -> "";
    _ -> "Authorization: Basic "++binary_to_list(base64:encode(UserInfo))++"\r\n"
  end,
  RTSP1 = RTSP#rtsp_socket{url = URL, options = Options, media = Consumer, rtp_ref = Ref, auth = Auth, timeout = Timeout},

  ConnectOptions = [binary, {packet, raw}, {active, once}, {keepalive, true}, {send_timeout, Timeout}, {send_timeout_close, true}],
  case gen_tcp:connect(Host, Port, ConnectOptions, Timeout) of
    {ok, Socket} ->
      ?D({"RTSP Connected", URL}),
      {reply, ok, RTSP1#rtsp_socket{socket = Socket}, Timeout};
    Else ->
      {stop, normal, Else, RTSP1}
  end;

handle_call({consume, Consumer}, _From, #rtsp_socket{rtp_ref = OldRef, timeout = Timeout} = RTSP) ->
  (catch erlang:demonitor(OldRef)),
  Ref = erlang:monitor(process, Consumer),
  {reply, ok, RTSP#rtsp_socket{rtp = Consumer, rtp_ref = Ref}, Timeout};


handle_call({request, describe}, From, #rtsp_socket{socket = Socket, url = URL, auth = Auth, seq = Seq, timeout = Timeout} = RTSP) ->
  Call = io_lib:format("DESCRIBE ~s RTSP/1.0\r\nCSeq: ~p\r\n"++Auth++"\r\n", [URL, Seq+1]),
  gen_tcp:send(Socket, Call),
  io:format("~s~n", [Call]),
  {noreply, RTSP#rtsp_socket{pending = From, state = describe, seq = Seq+1}, Timeout};

handle_call({request, setup, Num}, From, #rtsp_socket{socket = Socket, rtp_streams = Streams, url = URL, seq = Seq, auth = Auth, timeout = Timeout} = RTSP) ->
  % ?D({"Setup", Num, Streams}),
  Stream = #stream_info{options = Options} = element(Num, Streams),
  Control = proplists:get_value(control, Options),

  Sess = case RTSP#rtsp_socket.session of
    undefined -> "";
    Session -> "Session: "++Session++"\r\n"
  end,
  Call = io_lib:format("SETUP ~s RTSP/1.0\r\nCSeq: ~p\r\n"++Sess++
        "Transport: RTP/AVP/TCP;unicast;interleaved=~p-~p\r\n"++Auth++"\r\n",
        [append_trackid(URL, Control), Seq + 1, Num*2 - 2, Num*2-1]),
  gen_tcp:send(Socket, Call),
  io:format("~s~n", [Call]),
  {noreply, RTSP#rtsp_socket{pending = From, rtp_streams = setelement(Num, Streams, rtp_decoder:init(Stream)), seq = Seq+1}, Timeout};

handle_call({request, play}, From, #rtsp_socket{socket = Socket, url = URL, seq = Seq, session = Session, auth = Auth, timeout = Timeout} = RTSP) ->
  Call = io_lib:format("PLAY ~s RTSP/1.0\r\nCSeq: ~p\r\nSession: ~s\r\n"++Auth++"\r\n", [URL, Seq + 1, Session]),
  gen_tcp:send(Socket, Call),
  io:format("~s~n", [Call]),
  {noreply, RTSP#rtsp_socket{pending = From, state = play, seq = Seq + 1}, Timeout};

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
handle_cast({socket_ready, Socket}, #rtsp_socket{timeout = Timeout} = State) ->
  {ok, {IP, Port}} = inet:peername(Socket),
  inet:setopts(Socket, [{active, once}]),
  {noreply, State#rtsp_socket{socket = Socket, addr = IP, port = Port}, Timeout};

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
  ?D({"RTSP socket closed"}),
  {stop, normal, State};

handle_info({tcp, Socket, Bin}, #rtsp_socket{buffer = Buf, timeout = Timeout} = RTSPSocket) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, handle_packet(RTSPSocket#rtsp_socket{buffer = <<Buf/binary, Bin/binary>>}), Timeout};

handle_info({'DOWN', _, process, Consumer, _Reason}, #rtsp_socket{rtp = Consumer} = Socket) ->
  ?D({"RTSP RTP process died", Consumer}),
  {stop, normal, Socket};

handle_info({'DOWN', _, process, Consumer, _Reason}, #rtsp_socket{media = Consumer} = Socket) ->
  ?D({"RTSP consumer died", Consumer}),
  {stop, normal, Socket};

handle_info(#video_frame{content = Content} = _Frame, #rtsp_socket{socket = _Sock, timeout = Timeout} = Socket) ->
  ?DBG("Media Frame (~p): ~p", [self(), Content]),
  {noreply, Socket, Timeout};

%% Here Type is rtp|rtcp
handle_info({interleaved, Channel, {Type, RTP}}, #rtsp_socket{socket = Sock, timeout = Timeout} = Socket) ->
  Data = packet_codec:encode({Type, Channel, RTP}),
  gen_tcp:send(Sock, Data),
  {noreply, Socket, Timeout};

handle_info(timeout, #rtsp_socket{frames = Frames, media = Consumer} = Socket) ->
  lists:foreach(fun(Frame) ->
    % ?D({Frame#video_frame.content, Frame#video_frame.flavor, round(Frame#video_frame.dts)}),
    Consumer ! Frame
  end, Frames),
  {stop, timeout, Socket#rtsp_socket{frames = []}};

handle_info(Message, #rtsp_socket{} = Socket) ->
  {stop, {uknown_message, Message}, Socket}.

dump_io({request, Method, URL, Headers, undefined}) ->
  HeaderS = lists:flatten([io_lib:format("~p: ~p~n", [K, V]) || {K,V} <- Headers]),
  io:format("~s ~s RTSP/1.0~n~s~n", [Method, URL, HeaderS]);

dump_io({request, Method, URL, Headers, Body}) ->
  HeaderS = lists:flatten([io_lib:format("~p: ~p~n", [K, V]) || {K,V} <- Headers]),
  io:format("~s ~s RTSP/1.0~n~s~n~s~n", [Method, URL, HeaderS, Body]);

dump_io({response, Code, Message, Headers, undefined}) ->
  HeaderS = lists:flatten([io_lib:format("~p: ~p~n", [K, V]) || {K,V} <- Headers]),
  io:format("RTSP/1.0 ~p ~s~n~s~n", [Code, Message, HeaderS]);

dump_io({response, Code, Message, Headers, Body}) ->
  HeaderS = lists:flatten([io_lib:format("~p: ~p~n", [K, V]) || {K,V} <- Headers]),
  io:format("RTSP/1.0 ~p ~s~n~s~n~s~n", [Code, Message, HeaderS, Body]).
  
-define(DUMP_REQUEST(X), dump_io(X)).
-define(DUMP_RESPONSE(X), dump_io(X)).

handle_packet(#rtsp_socket{buffer = Data} = Socket) ->
  case packet_codec:decode(Data) of
    {more, Data} ->
      Socket;
    {ok, {rtp, _Channel, _} = RTP, Rest} ->
      Socket1 = handle_rtp(Socket#rtsp_socket{buffer = Rest}, RTP),
      handle_packet(Socket1);
    {ok, {response, _Code, _Message, Headers, _Body} = Response, Rest} ->
      ?DUMP_RESPONSE(Response),
      Socket1 = handle_response(extract_session(Socket#rtsp_socket{buffer = Rest}, Headers), Response),
      handle_packet(Socket1);
    {ok, {request, _Method, _URL, Headers, Body} = Request, Rest} ->
      ?DUMP_REQUEST(Request),
      Socket1 = handle_request(Request, Socket#rtsp_socket{buffer = Rest}),
      handle_packet(Socket1)
  end.


handle_response(#rtsp_socket{state = describe} = Socket, {response, _Code, _Message, Headers, Body}) ->
  Socket1 = handle_sdp(Socket, Headers, Body),
  reply_pending(Socket1#rtsp_socket{state = undefined});

handle_response(#rtsp_socket{state = play} = Socket, {response, _Code, _Message, Headers, _Body}) ->
  Socket1 = sync_rtp(Socket, Headers),
  reply_pending(Socket1#rtsp_socket{state = undefined});

handle_response(Socket, {response, _Code, _Message, Headers, Body} = Response) ->
  reply_pending(Socket).
  

reply_pending(#rtsp_socket{pending = undefined} = Socket) ->
  Socket;

reply_pending(#rtsp_socket{state = {Method, Count}} = Socket) when Count > 1 ->
  Socket#rtsp_socket{state = {Method, Count - 1}};

reply_pending(#rtsp_socket{pending = From, pending_reply = Reply} = Socket) ->
  gen_server:reply(From, Reply),
  Socket#rtsp_socket{pending = undefined, pending_reply = ok}.

handle_sdp(#rtsp_socket{} = Socket, Headers, Body) ->
  <<"application/sdp">> = proplists:get_value('Content-Type', Headers),
  MediaInfo = #media_info{audio = Audio, video = Video} = sdp:decode(Body),
      
  StreamNums = lists:seq(1, length(Audio)+length(Video)),
  % TODO: Отрефакторить это уродство
  
  {StreamInfos, AudioNum, VideoNum, ControlMap} = case {Audio, Video} of
    {[A], [V]} -> {{A, V}, 1, 2, [{proplists:get_value(control, A#stream_info.options),1}, {proplists:get_value(control, V#stream_info.options),2}]};
    {[], [V]} -> {{V}, undefined, 1, [{proplists:get_value(control, V#stream_info.options),1}]};
    {[A], []} -> {{A}, undefined, 1, [{proplists:get_value(control, A#stream_info.options),1}]}
  end,  
  ?D({"Streams", StreamInfos, StreamNums, ControlMap}),
  Socket#rtsp_socket{rtp_streams = StreamInfos, control_map = ControlMap, pending_reply = {ok, MediaInfo, StreamNums}, audio_rtp_stream = AudioNum, video_rtp_stream = VideoNum}.



sync_rtp(#rtsp_socket{rtp_streams = Streams} = Socket, RtpHeaders) ->
  case proplists:get_value(<<"Rtp-Info">>, RtpHeaders) of
    undefined ->
      Socket;
    Info ->
      {ok, Re} = re:compile("([^=]+)=(.*)"),
      F = fun(S) ->
        {match, [_, K, V]} = re:run(S, Re, [{capture, all, list}]),
        {K, V}
      end,
      RtpInfo = [[F(S1) || S1 <- string:tokens(S, ";")] || S <- string:tokens(binary_to_list(Info), ",")],
      % ?D({"Rtp", RtpInfo}),
      Streams1 = lists:zipwith(fun(Stream, Headers) ->
        rtp_decoder:sync(Stream, Headers)
      end, tuple_to_list(Streams), RtpInfo),
      Socket#rtsp_socket{rtp_streams = list_to_tuple(Streams1)}
  end.



seq(Headers) ->
  proplists:get_value('Cseq', Headers, 1).
  
%
% Wirecast goes:
% 
% ANNOUNCE with SDP
% OPTIONS
% SETUP  
  


handle_request({request, 'DESCRIBE', URL, Headers, Body}, #rtsp_socket{callback = Callback} = Socket) ->
  handle_describe_request(Socket, URL, Headers, Body);


handle_request({request, 'RECORD', URL, Headers, Body}, #rtsp_socket{callback = Callback} = State) ->
  case Callback:record(URL, Headers, Body) of
    ok ->
      reply(State, "200 OK", [{'Cseq', seq(Headers)}]);
    {error, authentication} ->
      reply(State, "401 Unauthorized", [{"WWW-Authenticate", "Basic realm=\"Erlyvideo Streaming Server\""}, {'Cseq', seq(Headers)}])
  end;


handle_request({request, 'PLAY', URL, Headers, Body}, #rtsp_socket{direction = in} = State) ->
  handle_request({request, 'RECORD', URL, Headers, Body}, State);

handle_request({request, 'PLAY', URL, Headers, Body}, #rtsp_socket{} = Socket) ->
  handle_play_request(Socket, URL, Headers, Body);

handle_request({request, 'OPTIONS', _URL, Headers, _Body}, State) ->
  reply(State, "200 OK", [{'Server', ?SERVER_NAME}, {'Cseq', seq(Headers)}, {'Public', "SETUP, TEARDOWN, PLAY, PAUSE, DESCRIBE"}]);

handle_request({request, 'ANNOUNCE', URL, Headers, Body}, #rtsp_socket{callback = Callback} = Socket) ->
  handle_announce_request(Socket, URL, Headers, Body);

handle_request({request, 'PAUSE', _URL, Headers, _Body}, #rtsp_socket{rtp = undefined} = State) ->
  reply(State, "200 OK", [{'Cseq', seq(Headers)}]);

handle_request({request, 'PAUSE', _URL, Headers, _Body}, #rtsp_socket{rtp = Consumer} = State) ->
  gen_server:call(Consumer, {pause, self()}),
  reply(State, "200 OK", [{'Cseq', seq(Headers)}]);

handle_request({request, 'SETUP', URL, Headers, Body}, #rtsp_socket{} = Socket) ->
  Transport = proplists:get_value('Transport', Headers),
  case proplists:get_value(mode, Transport) of
    'receive' ->
      handle_receive_setup(Socket, URL, Headers, Body)
  end;
  

handle_request({request, 'TEARDOWN', _URL, Headers, _Body},
               #rtsp_socket{rtp = RTPProc} = State) ->
  if is_pid(RTPProc) ->
      ?DBG("Stop RTP Proc ~p", [RTPProc]),
          rtp_server:stop(RTPProc);
     true -> pass
  end,
  reply(State, "200 OK", [{'Cseq', seq(Headers)}]).


handle_describe_request(#rtsp_socket{callback = Callback} = Socket, URL, Headers, Body) ->
  case Callback:describe(URL, Headers, Body) of
    {error, authentication} ->
      reply(Socket, "401 Unauthorized", [{"WWW-Authenticate", "Basic realm=\"Erlyvideo Streaming Server\""}, {'Cseq', seq(Headers)}]);
    {ok, Media} ->
      handle_authorized_describe(Socket, URL, Headers, Media)
  end.
  
handle_authorized_describe(Socket, URL, Headers, Media) ->  
  MediaInfo = ems_media:media_info(Media),
  ?DBG("Describe INFO (~p): ~p", [self(), MediaInfo]),

  SessionDesc =
    #sdp_session{version = "0",
                  originator = #sdp_o{username = "-",
                                      sessionid = "1275067839203788",
                                      version = "1",
                                      netaddrtype = inet4,
                                      address = "0.0.0.0"},
                  name = "Test",
                  connect = {inet4, "0.0.0.0"},
                  attrs = [
                           {tool, "LIVE555 Streaming Media v2008.04.09"},
                           recvonly,
                           {type, "broadcast"},
                           {control, "*"},
                           {charset, "UTF-8"},
                           {range, " npt=0-"}
                          ]},
  Opts = [{video, <<URL/binary, "/trackID=0">>},{audio, <<URL/binary, "/trackID=1">>}],
  MediaConfig = [sdp:prep_media_config(F, Opts) || F <- MediaInfo],
  ?DBG("MediaConfig:~n~p", [MediaConfig]),
  SDP = sdp:encode(SessionDesc, MediaConfig),
  %%?DBG("SDP:~n~p", [SDP]),
  reply(Socket#rtsp_socket{media = Media, direction = out}, "200 OK",
        [
         {'Server', ?SERVER_NAME},
         {'Cseq', seq(Headers)},
         {'Cache-Control', "no-cache"}
        ], SDP).
  
handle_play_request(#rtsp_socket{callback = Callback, session = _Session, media = Media, rtp = ProducerCtl} = Socket, URL, Headers, Body) ->
  %% Callback:play sets up self() as consumer of #video_frame-s:
  %% Callback:play -> media_provider:play -> ems_media:play
  %%case Callback:play(URL, Headers, Body) of
  ?DBG("PLAY: ~p", [ProducerCtl]),
  case rtp_server:play(ProducerCtl,
                       fun() -> Callback:play(URL, Headers, Body) end, Media) of
    {ok, Info} ->
      %%erlang:monitor(process, Media),
      %% Save Pid of producer here or in SETUP?
      Infos = [Track
               ++ ";seq=" ++ integer_to_list(Seq)
               ++ ";rtptime=" ++ integer_to_list(RtpTime) ||
                {Track, Seq, RtpTime} <- Info],
      reply(Socket, "200 OK",
            [
             {'Cseq', seq(Headers)},
             {'Cache-control', "no-cache"},
             {'RTP-Info', string:join(Infos, ",")}
            ]);
    {error, authentication} ->
      reply(Socket, "401 Unauthorized", [{"WWW-Authenticate", "Basic realm=\"Erlyvideo Streaming Server\""}, {'Cseq', seq(Headers)}])
  end.
  

handle_announce_request(#rtsp_socket{callback = Callback} = Socket, URL, Headers, Body) ->
  Socket1 = #rtsp_socket{pending_reply = {ok, MediaInfo, _}} = handle_sdp(Socket, Headers, Body),
  case Callback:announce(URL, Headers, MediaInfo) of
    {ok, Media} ->
      ?D({"Announced to", Media}),
      erlang:monitor(process, Media),
      reply(Socket1#rtsp_socket{session = 42, media = Media, direction = in}, "200 OK", [{'Cseq', seq(Headers)}]);
    {error, authentication} ->
      reply(Socket1, "401 Unauthorized", [{"WWW-Authenticate", "Basic realm=\"Erlyvideo Streaming Server\""}, {'Cseq', seq(Headers)}])
  end.
  
handle_receive_setup(#rtsp_socket{} = Socket, URL, Headers, Body) ->
  {match, [Control]} = re:run(URL, "/([^/]+)$", [{capture, all_but_first, list}]),
  StreamNum = proplists:get_value(Control, Socket#rtsp_socket.control_map),
  StreamInfo = element(StreamNum, Socket#rtsp_socket.rtp_streams),
  Streams = setelement(StreamNum, Socket#rtsp_socket.rtp_streams, rtp_decoder:init(StreamInfo)),
  reply(Socket#rtsp_socket{rtp_streams = Streams}, "200 OK", [{'Cseq', seq(Headers)}, {'Session', 42}, {'Transport', proplists:get_value('Transport', Headers)}]).

reply(State, Code, Headers) ->
  reply(State, Code, Headers, undefined).

reply(#rtsp_socket{socket = Socket, session = SessionId, timeout = TimeOut} = State, Code, Headers, Body) ->
  Headers2 =
    case SessionId of
      undefined -> Headers;
      _ ->
        if is_integer(TimeOut) ->
            TO = ";timeout=" ++ integer_to_list(TimeOut);
           true -> TO = ""
        end,
        [{'Session', integer_to_list(SessionId) ++ TO} | lists:keydelete('Session', 1, Headers)]
    end,
  Headers3 = case Body of
    undefined -> [{'Content-Length', 0} | Headers2];
    _ -> [{'Content-Length', iolist_size(Body)}, {'Content-Type', <<"application/sdp">>}|Headers2]
  end,
  Reply = iolist_to_binary(["RTSP/1.0 ", Code, <<"\r\n">>, packet_codec:encode_headers(Headers3), <<"\r\n">>,
  case Body of
    undefined -> <<>>;
    _ -> Body
  end]),
  io:format("[RTSP Response to Client]~n~s", [Reply]),
  gen_tcp:send(Socket, Reply),
  State.




extract_session(Socket, Headers) ->
  case proplists:get_value('Session', Headers) of
    undefined ->
      Socket;
    FullSession ->
      % ?D({"Session", FullSession}),
      Socket#rtsp_socket{session = hd(string:tokens(binary_to_list(FullSession), ";"))}
  end.


audio_stream(#rtsp_socket{audio_rtp_stream = undefined}) -> undefined;
audio_stream(#rtsp_socket{audio_rtp_stream = AudioNum, rtp_streams = Streams}) -> element(AudioNum, Streams).

video_stream(#rtsp_socket{video_rtp_stream = undefined}) -> undefined;
video_stream(#rtsp_socket{video_rtp_stream = VideoNum, rtp_streams = Streams}) -> element(VideoNum, Streams).

handle_rtp(#rtsp_socket{socket = Sock, rtp_streams = Streams, frames = Frames} = Socket, {rtp, Channel, Packet}) ->
  % ?D({rtp,Channel}),
  {Streams1, NewFrames} = case Channel rem 2 of
    0 ->
      RtpNum = Channel div 2 + 1,
      RtpState = element(RtpNum, Streams),
      {ok, RtpState1, RtpFrames} = rtp_decoder:decode(Packet, RtpState),
      {setelement(RtpNum, Streams, RtpState1), RtpFrames};
    1 ->
      RtpNum = (Channel - 1) div 2 + 1,
      RtpState = element(RtpNum, Streams),
      RtpState1 = rtp_decoder:rtcp(Packet, RtpState),
      RtpState2 = RtpState1,
      % {RtpState2, RtcpData} = rtp_rtsp:encode(receiver_report, RtpState1),
      % RTCP_RR = packet_codec:encode({rtcp, RTPNum, RtcpData}),
      % gen_tcp:send(Sock, RTCP_RR),
      {setelement(RtpNum, Streams, RtpState2), []}
  end,
  reorder_frames(Socket#rtsp_socket{rtp_streams = Streams1, frames = Frames ++ NewFrames}).

reorder_frames(#rtsp_socket{frames = Frames} = Socket) when length(Frames) < ?FRAMES_BUFFER ->
  Socket;

reorder_frames(#rtsp_socket{frames = Frames, media = Consumer, sent_audio_config = SentAC} = Socket) ->
  Ordered = lists:sort(fun frame_sort/2, Frames),
  {ToSend, NewFrames} = lists:split(?REORDER_FRAMES, Ordered),
  lists:foreach(fun
    (#video_frame{codec = aac, dts = DTS} = Frame) when SentAC == false -> 
      Consumer ! (rtp_decoder:config_frame(audio_stream(Socket)))#video_frame{dts = DTS, pts = DTS},
      Consumer ! Frame;
    (#video_frame{codec = h264, flavor = keyframe, dts = DTS} = Frame) ->
      Consumer ! (rtp_decoder:config_frame(video_stream(Socket)))#video_frame{dts = DTS, pts = DTS},
      Consumer ! Frame;
    (Frame) -> Consumer ! Frame
  end, ToSend),
  Socket#rtsp_socket{frames = NewFrames, sent_audio_config = true}.

frame_sort(#video_frame{dts = DTS1}, #video_frame{dts = DTS2}) -> DTS1 =< DTS2.


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

append_trackid(_URL, ("rtsp://"++ _) = TrackID) ->
  TrackID;

append_trackid(URL, TrackID) ->
  case string:tokens(URL, "?") of
    [URL1, URL2] -> URL1 ++ "/" ++ TrackID ++ "?" ++ URL2;
    [URL] -> URL ++ "/" ++ TrackID
  end.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").


append_trackid_test_() ->
  [?_assertEqual("rtsp://cam1:554/h264.sdp/trackID=1", append_trackid("rtsp://cam1:554/h264.sdp", "trackID=1")),
   ?_assertEqual("rtsp://cam1:554/h264.sdp/trackID=1?res=half&x0=0", append_trackid("rtsp://cam1:554/h264.sdp?res=half&x0=0", "trackID=1")),
   ?_assertEqual("rtsp://cam1:554/h264.sdp/track1?res=half&x0=0", append_trackid("rtsp://cam1:554/h264.sdp?res=half&x0=0", "track1")),
   ?_assertEqual("rtsp://cam1:554/h264.sdp/track1?res=half&x0=0", append_trackid("rtsp://cam1:554/h264.sdp?res=half&x0=0", "rtsp://cam1:554/h264.sdp/track1?res=half&x0=0"))
  ].



%%%%%%%



handle_reques1t({request, 'SETUP', URL, Headers, _},
               #rtsp_socket{addr = Addr, port = OPort,
                            session = Session,
                            rtp = ProducerCtl,
                            media = Media} = State) ->
  ?DBG("Addr: ~p, OPort: ~p", [Addr, OPort]),
  OldTransport = proplists:get_value('Transport', Headers),

  %% Date = httpd_util:rfc1123_date(),
  %% {ok, Re} = re:compile("trackID=(\\d+)"),
  %% Transport =
  %%   case re:run(URL, Re, [{capture, all, list}]) of
  %%     {match, [_, TrackID_S]} ->
  %%       ?DBG("SDP:~n~p", [SDP]),
  %%       %% Extract media_desc from SDP
  %%       case lists:keyfind("trackID="++TrackID_S, #media_desc.track_control, SDP) of
  %%         #media_desc{} = Stream ->
  %%           Media = self(),
  %%           rtp_server:start_link(Media, Stream);
  %%         false ->
  %%           fail
  %%       end,
  %%       TrackID = (list_to_integer(TrackID_S) - 1)*2,
  %%       list_to_binary("RTP/AVP/TCP;unicast;interleaved="++integer_to_list(TrackID)++"-"++integer_to_list(TrackID+1));
  %%     _ -> OldTransport
  %%   end,
  %% Transport = OldTransport,

  % case re:run(OldTransport, "RTP/AVP(/(TCP|UDP))?;(.*)", [{capture, [2, 3], list}]) of
  %   {match, [LowerTransport, Parameters]}
  %     when LowerTransport =:= "";
  %          LowerTransport =:= "UDP" ->
  %     Proto = udp;
  %   {match, ["TCP", Parameters]} ->
  %     Proto = tcp;
  %   _ ->
  %     Parameters = "",
  %     Proto = undefined
  % end,
  % case re:run(Parameters, "(client_port|interleaved)=(\\d+)-(\\d+)", [{capture, [1,2,3], list}]) of
  %   {match, ["client_port", Val0s, Val1s]} ->
  %     TagVal = ports;
  %   {match, ["interleaved", Val0s, Val1s]} ->
  %     TagVal = interleaved;
  %   _ ->
  %     {Val0s, Val1s} = {"", ""},
  %     TagVal = undefined
  % end,
  % 
  % Proto2List =
  %   fun(tcp) -> "TCP";
  %      (udp) -> "UDP"
  %   end,
    NewTransport = OldTransport,
    NewState = State,
  % if ((Proto =/= undefined) andalso (TagVal =/= undefined)) ->
  %     case re:run(URL, "trackID=(\\d+)", [{capture, all, list}]) of
  %       {match, [_, _TrackID_S]} ->
  %         ?DBG("URL: ~p~nSDP:~n~p", [URL, SDP]),
  %         case lists:keyfind(binary_to_list(URL), #media_desc.track_control, SDP) of
  %           #media_desc{} = Stream ->
  %             {Val0, Val1} = {list_to_integer(Val0s), list_to_integer(Val1s)},
  %             if is_pid(ProducerCtl) ->
  %                 ProdCtlPid = ProducerCtl,
  %                 NewState = State;
  %                true ->
  %                 ?DBG("Start RTP process with media ~p", [Media]),
  %                 {ok, ProdCtlPid} =
  %                   rtp_server:start_link({producer, [{media, Media}]}),
  %                 ProducerRef = erlang:monitor(process, ProdCtlPid),
  %                 NewState = State#rtsp_socket{rtp = ProdCtlPid,
  %                                              rtp_ref = ProducerRef}
  %             end,
  %             ?DBG("Add Stream: ~p", [{Stream, Proto, Addr, TagVal, {Val0, Val1}}]),
  %             case TagVal of
  %               ports ->
  %                 {ok, {TagVal, {SRTPPort, SRTCPPort}}} = rtp_server:listen_ports(ProdCtlPid, Stream, Proto, TagVal),
  %                 ok = rtp_server:add_stream(ProdCtlPid, Stream, {TagVal, {Addr, Val0, Val1}}, {rtsp, Headers}),
  %                 ?DBG("Server Ports: ~p", [{SRTPPort, SRTCPPort}]),
  %                 ServerPorts = [";server_port=", integer_to_list(SRTPPort), "-", integer_to_list(SRTCPPort)],
  %                 NewTransport = iolist_to_binary(["RTP/AVP/", Proto2List(Proto), ";unicast;client_port=", Val0s, "-", Val1s, ServerPorts]);
  %               interleaved ->
  %                 rtp_server:listen_ports(ProdCtlPid, Stream, Proto, TagVal),
  %                 ok = rtp_server:add_stream(ProdCtlPid, Stream, {TagVal, {self(), Val0, Val1}}, {rtsp, Headers}),
  %                 NewTransport = iolist_to_binary(["RTP/AVP/", Proto2List(Proto), ";unicast;interleaved=", Val0s, "-", Val1s])
  %             end
  %         end;
  %       _ ->
  %         NewTransport = OldTransport,
  %         NewState = State
  %     end;
  %    true ->
  %     ?DBG("Error: Proto: ~p, TagVal: ~p", [Proto, TagVal]),
  %     NewTransport = OldTransport,
  %     NewState = State
  % end,

  NewSession =
    case Session of
      undefined ->
        {A1, A2, A3} = now(),
        (A1*1000*1000*1000*1000)+(A2*1000*1000)+A3;
      _ ->
        Session
    end,
  ReplyHeaders = [
                  {'Server', ?SERVER_NAME},
                  {'Transport', NewTransport},
                  {'Cseq', seq(Headers)},
                  {'Session', NewSession},
                  {'Cache-Control', "no-cache"}
                 ],
  reply(NewState#rtsp_socket{session = NewSession}, "200 OK", ReplyHeaders).

