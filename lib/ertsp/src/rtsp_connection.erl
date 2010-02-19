-module(rtsp_connection).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(rtsp_socket).
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
  buffer = <<>>,
  rtp_streams = {undefined, undefined, undefined, undefined},
  media,
  callback
}).


start_link(Callback) ->
  gen_fsm:start_link(?MODULE, [Callback], []).

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
  {noreply, State#rtsp_connection{socket=Socket, addr=IP, port = Port}, ?TIMEOUT}.

handle_info(timeout, State) ->
  {stop, normal, State}.

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
  State#rtsp_connection{rtp_streams = Streams1};


% split_params(String) ->
%   lists:map(fun(Opt) -> case string:tokens(Opt, "=") of
%     [Key] -> Key;
%     [Key, Value] -> {Key, Value}
%   end end, string:tokens(binary_to_list(String), ";")).

handle_rtsp_request(#rtsp_connection{host = Host, callback = Callback, headers = Headers} = State, {request, 'ANNOUNCE', _URL, Headers, Body}) ->
  Path = path(State),
  Streams = sdp:decode(Body),
  case Callback:announce(Host, Path, Streams, Headers) of
    {ok, Media, NewStreams} -> reply(State#rtsp_connection{media = Media, streams = NewStreams}, "200 OK", []);
    {error, not_authed} -> reply(State, "401 Authorization Required", []);
    {error, Reason} -> reply(State, "500 Server error", [])
  end;
  

handle_rtsp_request(#rtsp_connection{request = ['OPTIONS', _URL]} = State) ->
  reply(State, "200 OK", [{'Public', "SETUP, TEARDOWN, PLAY, PAUSE, RECORD, OPTIONS, DESCRIBE"}]);

handle_rtsp_request(#rtsp_connection{request = ['RECORD', _URL]} = State) ->
  reply(State, "200 OK", []);

handle_rtsp_request(#rtsp_connection{request = ['PAUSE', _URL]} = State) ->
  reply(State, "200 OK", []);

handle_rtsp_request(#rtsp_connection{request = ['TEARDOWN', _URL], media = Media, host = Host} = State) ->
  % Path = path(State),
  % Media = media_provider:find(Host, Path),
  Media ! stop,
  reply(State, "200 OK", []);

handle_rtsp_request(#rtsp_connection{request = ['SETUP', URL], headers = Headers, streams = Streams, media = Media} = State) ->
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

handle_rtsp_request(#rtsp_connection{request = [_Method, _URL]} = State) ->
  reply(State, "200 OK", []).
  

decode_line(Message, #rtsp_connection{rtsp_re = RtspRe, body = Body} = State) ->
  {match, [_, Key, Value]} = re:run(Message, RtspRe, [{capture, all, binary}]),
  io:format("[RTSP]  ~s: ~s~n", [Key, Value]),
  State#rtsp_connection{body = [{Key, Value} | Body]}.

  
  

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
  
  
path(#rtsp_connection{url_re = UrlRe, request = [_, URL]}) ->
  {match, [_, _Host, Path]} = re:run(URL, UrlRe, [{capture, all, binary}]),
  Path.
  
  

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
   io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, got_sync_request]),
  {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, _Socket, Bin}, 'WAIT_FOR_HEADERS' = StateName, State) ->
  case erlang:decode_packet(httph_bin, <<Bin/binary, "pad">>, []) of
    {ok, {http_header, _, Name, _, Value}, <<"pad">>} -> 
      Key = case Name of
        _ when is_binary(Name) -> binary_to_existing_atom(Name, utf8);
        _ when is_atom(Name) -> Name
      end,
      ?MODULE:StateName({header, Key, Value}, State);
    {ok, http_eoh, <<"pad">>} -> ?MODULE:StateName(end_of_headers, State)
  end;

handle_info({tcp, _Socket, Bin}, StateName, State) ->
  ?MODULE:StateName({data, Bin}, State);

% handle_info({http, _Socket, {http_header, _, Name, _, Value}}, StateName, State) ->
%   ?MODULE:StateName({header, Name, Value}, State);
% 
% handle_info({http, _Socket, http_eoh}, StateName, State) ->
%   ?MODULE:StateName(end_of_headers, State);


handle_info({tcp_closed, Socket}, _StateName, #rtsp_connection{socket=Socket, addr=Addr, port = Port} = StateData) ->
  error_logger:info_msg("~p Client ~p:~p disconnected.\n", [self(), Addr, Port]),
  {stop, normal, StateData};

handle_info(timeout, StateName, StateData) ->
  ?D({"Stop due timeout", StateName, StateData}),
  {stop, normal, StateData};


handle_info(_Info, StateName, StateData) ->
  ?D({"Some info handled", _Info, StateName, StateData}),
  {noreply, StateName, StateData}.


%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #rtsp_connection{socket=Socket}) ->
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

