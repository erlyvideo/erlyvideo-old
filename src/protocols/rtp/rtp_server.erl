-module(rtp_server).
-author('max@maxidoors.ru').

-include("../../../include/ems.hrl").
-include("../../../include/h264.hrl").

-record(rtp_server, {
	rtcp_listener,
	rtp_listener,
	rtcp_port,
	rtp_port,
	streams
	}).
	
	
-record(video, {
  media,
  clock_map,
  sequence = 0,
  timestamp,
  width,
  height,
  payload_type,
  h264
}).
	
-behaviour(gen_server).

%% External API
-export([start_link/0, register/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([port/0, video/2, audio/2, video/1, audio/1]).

%%--------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, Pid} | {error, Reason}
%%
%% @doc Called by a supervisor to start the listening process.
%% @end
%%----------------------------------------------------------------------
start_link()  ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------


port() ->
  gen_server:call(?MODULE, port).


register(Key, Handler, Streams) ->
  gen_server:call(?MODULE, {register, Key, Handler, Streams}).

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit, true),
  Opts = [binary, {active, once}],

  RTCP = 6256,
  RTP = RTCP + 1,
  
  {ok, RTCPListen} = gen_udp:open(RTCP, Opts),
  {ok, RTPListen} = gen_udp:open(RTP, Opts),
  
  Streams = ets:new(streams, [set, {keypos, 1}]),
  {ok, #rtp_server{rtcp_listener = RTCPListen, rtp_listener = RTPListen, 
                     rtcp_port = RTCP, rtp_port = RTP,
                     streams = Streams}}.

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

handle_call(port, _From, #rtp_server{rtcp_port = RTCP, rtp_port = RTP} = State) ->
  {reply, {ok, {RTCP, RTP}}, State};
  
handle_call({register, Key, Media, Stream}, _From, #rtp_server{streams = StreamTable} = State) ->
  Type = proplists:get_value(type, Stream),
  Decoder = spawn_link(?MODULE, Type, [Media, Stream]),
  ets:insert(StreamTable, {Key, Decoder}),
  ?D({"Registering", Key, Type, Decoder}),
  {reply, ok, State};

handle_call(Request, _From, State) ->
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

handle_info({udp,Socket,Host,Port, <<2:2, _Padding:1, _Extension:1, 0:4, _Marker:1, PayloadType:7, 
         Sequence:16, Timestamp:32, _StreamId:32, Body/binary>>}, #rtp_server{streams = StreamTable} = State) ->
  % ?D({"UDP message", Host, Port, size(Bin)}),
  % {ok, {Address, Local}} = inet:sockname(Socket),
  case ets:match_object(StreamTable, {{Host, Port}, '_'}) of
    [{_, Decoder}] -> Decoder ! {data, Body, Sequence, Timestamp, PayloadType};
    _ -> ?D({"Unknown payload", Host, Port})
  end,
  inet:setopts(Socket, [{active, once}]),
  {noreply, State};
    
handle_info({'EXIT', Pid, _Reason}, #rtp_server{streams = Streams} = State) ->
  ets:match_delete(Streams, {'_', Pid}),
  ?D({"Died linked process", Pid, _Reason}),
  {noreply, State};
    
  
handle_info(_Info, State) ->
  {noreply, State}.


audio(_Media, _Stream) ->
  audio(_Stream).
  
audio(_) -> ok.



video(Media, Stream) ->
  ClockMap = proplists:get_value(clock_map, Stream),
  Width = proplists:get_value(width, Stream),
  Height = proplists:get_value(height, Stream),
  PayloadType = proplists:get_value(payload_type, Stream),
  Video = #video{media = Media, clock_map = ClockMap, h264 = #h264{}, 
                 width = Width, height = Height, payload_type = PayloadType},
  link(Media),
  ?D({"Starting rtp video", Video}),
  video(Video).
  
video(#video{payload_type = PayloadType, h264 = H264, clock_map = ClockMap, media = Media} = Video) ->
  receive
    {data, Body, Sequence, Timestamp, PayloadType} ->
      {H264_1, Frames} = h264:decode_nal(Body, H264),
      lists:foreach(fun(Frame) ->
        ?D({"H264 Frame", Frame#video_frame.frame_type, Frame#video_frame.decoder_config}),
        Media ! Frame#video_frame{timestamp = Timestamp / ClockMap}
      end, Frames),
      
      ?MODULE:video(Video#video{sequence = Sequence + 1, timestamp = Timestamp, h264 = H264_1});
    Else ->
      ?D({"Unknown", Else})
  after
    50000 ->
      ?D("RTP video timeout")
  end.
  

% Version:2, Padding:1, Extension:1, CSRC:4, Marker:1, PayloadType:7, Sequence:16, Timestamp:32, StreamId:32, Other
% decode(<<2:2, _Padding:1, _Extension:1, 0:4, Marker:1, PayloadType:7, Sequence:16, Timestamp:32, StreamId:32, Rest/binary>>, Handler, Streams) ->
%   VideoFrame = #video_frame{body = Rest},
%   case lists:keyfind(PayloadType, 1, Streams) of
%     {PayloadType, audio, ClockMap, Stream} ->
%       % ?D({audio, Timestamp / ClockMap, size(Rest)}),
%       Handler ! VideoFrame#video_frame{timestamp = Timestamp / ClockMap, type = ?FLV_TAG_TYPE_AUDIO, codec_id = ?FLV_AUDIO_FORMAT_AAC},
%       Streams;
%   end.

  
%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, #rtp_server{rtcp_listener = RTCP, rtp_listener = RTP}) ->
  gen_udp:close(RTCP),
  gen_udp:close(RTP),
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------
