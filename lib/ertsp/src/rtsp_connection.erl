-module(rtsp_connection).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(rtsp_socket).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-include_lib("ertsp/include/rtsp.hrl").

% -include("../include/rtsp.hrl").
% -include("../../../include/ems.hrl").
% -define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
% -define(TIMEOUT, 1000).

-export([start_link/1, set_socket/2]).

%% rtsp_socket callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([handle_rtsp_response/2, handle_rtp_packet/2, handle_rtsp_request/2]).

-export([binarize_header/1]).


-record(rtsp_connection, {
  socket,
  host,
  addr,
  port,
  request_re,
  rtsp_re,
  url_re,
  session_id,
  streams,
  sequence = 0,
  buffer = <<>>,
  rtp_streams = {undefined, undefined, undefined, undefined},
  media,
  callback
}).


start_link(Callback) ->
  rtsp_socket:start_link(?MODULE, [Callback]).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
  gen_server:cast(Pid, {socket_ready, Socket}).

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([Callback]) ->
  process_flag(trap_exit, true),
  random:seed(now()),
  {ok, RequestRe} = re:compile("([^ ]+) ([^ ]+) (\\w+)/([\\d]+\\.[\\d]+)"),
  {ok, RtspRe} = re:compile("(\\w)=(.*)\\r\\n$"),
  {ok, UrlRe} = re:compile("rtsp://([^/]+)/(.*)$"),
  {ok, #rtsp_connection{request_re = RequestRe, rtsp_re = RtspRe, url_re = UrlRe, callback = Callback}}.



%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_cast({socket_ready, Socket}, State) ->
  inet:setopts(Socket, [{active, once}, binary]),
  {ok, {IP, Port}} = inet:peername(Socket),
  {noreply, State#rtsp_connection{socket=Socket, addr=IP, port = Port}}.

handle_info({tcp_closed, Socket}, State) ->
  {stop, normal, State};

handle_info(timeout, State) ->
  {stop, normal, State}.
  

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.

handle_rtp_packet(#rtsp_connection{rtp_streams = Streams} = State, {rtp, ChannelId, RTP}) ->
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
  State#rtsp_connection{rtp_streams = Streams1}.

handle_rtsp_response(State, _Response) ->
  State.

% split_params(String) ->
%   lists:map(fun(Opt) -> case string:tokens(Opt, "=") of
%     [Key] -> Key;
%     [Key, Value] -> {Key, Value}
%   end end, string:tokens(binary_to_list(String), ";")).

hostpath(URL) ->
  {ok, Re} = re:compile("rtsp://([^/]+)/(.*)$"),
  {match, [_, HostPort, Path]} = re:run(URL, Re, [{capture, all, binary}]),
  {ems:host(HostPort), Path}.
  

handle_rtsp_request(#rtsp_connection{callback = Callback} = State, {request, 'ANNOUNCE', URL, Headers, Body}) ->
  Streams = sdp:decode(Body),
  {Host, Path} = hostpath(URL),
  case Callback:announce(Host, Path, Streams, Headers) of
    {ok, Media, NewStreams} -> reply(State#rtsp_connection{media = Media, streams = NewStreams}, "200 OK", []);
    {error, not_authed} -> reply(State, "401 Authorization Required", []);
    {error, _Reason} -> reply(State, "500 Server error", [])
  end;
  

handle_rtsp_request(State, {request, 'OPTIONS', _, _, _}) ->
  reply(State, "200 OK", [{'Public', "SETUP, TEARDOWN, PLAY, PAUSE, RECORD, OPTIONS, DESCRIBE"}]);

handle_rtsp_request(State, {request, 'RECORD', _, _, _}) ->
  reply(State, "200 OK", []);

handle_rtsp_request(State, {request, 'PAUSE', _, _, _}) ->
  reply(State, "200 OK", []);

handle_rtsp_request(#rtsp_connection{media = Media} = State, {request, 'TEARDOWN', _URL, _, _}) ->
  % Path = path(State),
  % Media = media_provider:find(Host, Path),
  Media ! stop,
  reply(State, "200 OK", []);

handle_rtsp_request(#rtsp_connection{streams = Streams, media = Media} = State, {request, 'SETUP', URL, Headers, _}) ->
  {ok, Re} = re:compile("rtsp://([^/]*)/(.*)/trackid=(\\d+)"),
  {match, [_, HostPort, _Path, TrackIdS]} = re:run(URL, Re, [{capture, all, list}]),
  TrackId = list_to_integer(TrackIdS),
  Transport = proplists:get_value('Transport', Headers),
  
  Stream = lists:keyfind(TrackId, #rtsp_stream.id, Streams),
  
  [ProtoS | TransportOptsCoded] = string:tokens(binary_to_list(Transport), ";"),
  Proto = case ProtoS of
    "RTP/AVP" -> udp;
    "RTP/AVP/TCP" -> tcp
  end,
  
  TransportOpts = lists:map(fun(S) -> 
    case string:tokens(S, "=") of
    [Key, Value] -> {Key, Value};
    [Key] -> {Key, true}
  end end, TransportOptsCoded),

  Date = httpd_util:rfc1123_date(),
  
  State1 = case Proto of
    udp -> 
      {ok, _Listener, {RTP, RTCP}} = rtsp_sup:start_rtp_server(Media, Stream),
      HostName = hd(string:tokens(HostPort, ":")),
      {ok, {hostent, _, _, inet, _, [HostAddr | _]}} = inet:gethostbyname(HostName),
      {IP1, IP2, IP3, IP4} = HostAddr,
      TransportReply = io_lib:format("~s;server_port=~p-~p;source=~p.~p.~p.~p",[Transport, RTP, RTCP, IP1, IP2, IP3, IP4]),
      State;
    tcp ->
      TransportReply = Transport,
      ?D({TransportOpts}),
      [RTPs, RTCPs] = string:tokens(proplists:get_value("interleaved", TransportOpts), "-"),
      RTP = list_to_integer(RTPs),
      RTCP = list_to_integer(RTCPs),
      RtpConfig = rtp_server:init(Stream, Media),
      NewStreams = setelement(RTP+1, State#rtsp_connection.rtp_streams, {Stream#rtsp_stream.type, RtpConfig}),
      NewStreams1 = setelement(RTCP+1, NewStreams, {rtcp, RTP}),
      State#rtsp_connection{rtp_streams = NewStreams1}
  end,
  ReplyHeaders = [{"Transport", TransportReply},  {'Date', Date}, {'Expires', Date}, {'Cache-Control', "no-cache"}],
  reply(State1#rtsp_connection{session_id = 42}, "200 OK", ReplyHeaders);

handle_rtsp_request(State, {request, _Method, _URL, _Headers, _Body}) ->
  reply(State, "200 OK", []).
  

reply(#rtsp_connection{socket = Socket, sequence = Sequence, session_id = SessionId} = State, Code, Headers) ->
  Headers1 = [{'Cseq', Sequence} | Headers],
  Headers2 = case SessionId of
    undefined -> Headers1;
    _ -> [{'Session', SessionId} | Headers1]
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
  
  
  

