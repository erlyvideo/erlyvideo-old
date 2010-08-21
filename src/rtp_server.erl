%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTP decoder module
%%% @end
%%% @reference  See <a href="http://erlyvideo.org/ertp" target="_top">http://erlyvideo.org</a> for common information.
%%% @end
%%%
%%% This file is part of erlang-rtp.
%%%
%%% erlang-rtp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtp_server).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("erlmedia/include/h264.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include("sdp.hrl").
-include("log.hrl").

-record(rtp_state, {
  rtcp_socket,
  rtp_socket,
  type,
  state
}).

-record(base_rtp, {
  media,
  clock_map,
  base_timecode = undefined,
  sequence = undefined,
  wall_clock = undefined,
  base_wall_clock = undefined,
  timecode = undefined,
  synced = false,
  stream_id,
  last_sr,
  codec,
  marker       :: undefined | true | false,
  framelens    :: integer(),                    % Size of frame length in bytes
  packets = 0  :: integer(),
  bytes   = 0  :: integer()
}).

-record(video, {
  media,
  clock_map,
  base_timecode = undefined,
  sequence = undefined,
  wall_clock = undefined,
  base_wall_clock = undefined,
  timecode = undefined,
  synced = false,
  stream_id,
  last_sr,
  codec,
  h264 = #h264{},
  broken = false,
  buffer = []
}).

-record(audio, {
  media,
  clock_map,
  base_timecode = undefined,
  sequence = undefined,
  wall_clock = undefined,
  base_wall_clock = undefined,
  timecode = undefined,
  synced = false,
  stream_id,
  last_sr,
  codec,
  audio_rate,
  audio_headers = <<>>,
  audio_data = <<>>
}).



-define(RTCP_SR, 200).
-define(RTCP_RR, 201).
-define(RTCP_SD, 202).
-define(YEARS_70, 2208988800).  % RTP bases its timestamp on NTP. NTP counts from 1900. Shift it to 1970. This constant is not precise.

%% API
-export([
         start_link/1,
         play/3,
         add_stream/5,
         stop/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).


-export([video/2, audio/2, rtcp_sr/2]).
-export([decode/3, init/2, get_socket/3, wait_data/1]).
-export([configure/2, configure/3, presync/2]).

-export([encode/2, encode/4]).

-record(ports_desc, {
          proto        :: tcp | udp,
          addr         :: term(),
          port_rtp     :: integer(),
          port_rtcp    :: integer(),
          socket_rtp   :: term(),
          socket_rtcp  :: term()
         }).

-record(interleaved_desc, {
          socket_owner    :: pid(),
          channel_rtp     :: integer(),
          channel_rtcp    :: integer()
         }).

-record(desc, {
          method    ::  #ports_desc{} | #interleaved_desc{},
          track_control,
          state
         }).


-record(state, {
          audio     :: #desc{},
          video     :: #desc{},
          media     :: pid(),
          parent    :: pid()
         }).

%% Gen server process does control RTP-stream.
start_link(Args) ->
  Parent = self(),
  gen_server:start_link(?MODULE, [Args, Parent], []).

init([Args, Parent]) ->
  %% Ask Max, why RTP server starts up as application?
  if is_pid(Args) ->
      ?DBG("Startup Media: ~p", [Args]),
      Media = Args;
     true ->
      Media = undefined
  end,
  {ok, #state{media = Media,
              parent = Parent}}.


handle_call({play, Fun, Media}, _From,
            #state{audio = AudioDesc,
                   video = VideoDesc,
                   media = OldMedia} = State) ->
  ?DBG("DS: Play", []),
  ?DBG("Media: ~p, Old Media: ~p", [Media, OldMedia]),
  Info = [{Track, Seq, RtpTime} ||
           #desc{track_control = Track,
                 state = #base_rtp{sequence = Seq,
                                   timecode = RtpTime}} <- [AudioDesc, VideoDesc]],
  Fun(),
  timer:send_interval(2000, {send_sr, audio}),
  timer:send_interval(2000, {send_sr, video}),
  {reply, {ok, Info}, State};

handle_call({add_stream,
             #media_desc{type = Type, payload = PayloadType,
                         clock_map = ClockMap, track_control = TCtl},
             Proto, Addr, {Method, Params}}, _From,
            #state{} = State) ->
  ?DBG("DS: Add Stream", []),
  case Method of
    ports ->
      {PortRTP, PortRTCP} = Params,
      ?DBG("Connect to ~p:~p", [Addr, PortRTP]),
      OP = open_ports(Type),
      ?DBG("OP: ~p", [OP]),
      {RTP, RTPSocket, RTCP, RTCPSocket} = OP,
      gen_udp:controlling_process(RTPSocket, self()),
      gen_udp:controlling_process(RTCPSocket, self()),
      Result = {RTP, RTCP},
      MethodDesc = #ports_desc{
        proto = Proto, addr = Addr,
        port_rtp = PortRTP, port_rtcp = PortRTCP,
        socket_rtp = RTPSocket, socket_rtcp = RTCPSocket
       };
    interleaved ->
      {SocketOwner, ChanRTP, ChanRTCP} = Params,
      Result = ok,
      MethodDesc = #interleaved_desc{
        socket_owner = SocketOwner,
        channel_rtp = ChanRTP,
        channel_rtcp = ChanRTCP}
  end,
  ?DBG("PayloadType: ~p", [PayloadType]),
  WallClock = get_wall_clock(),
  Timecode = init_rnd_timecode(),
  BaseRTP = #base_rtp{codec = PayloadType,           % FIXME: PCM
                      media = Type,
                      clock_map = ClockMap,
                      sequence = init_rnd_seq(),
                      base_timecode = Timecode,
                      timecode = Timecode,
                      base_wall_clock = WallClock,
                      wall_clock = WallClock,
                      stream_id = init_rnd_ssrc()},
  NewState =
    case Type of
      audio ->
        State#state{audio = #desc{method = MethodDesc,
                                  track_control = TCtl,
                                  state = BaseRTP}};
      video ->
        State#state{video = #desc{method = MethodDesc,
                                  track_control = TCtl,
                                  state = BaseRTP}}
    end,
  ?DBG("NewState:~n~p", [NewState]),
  {reply, {ok, {Method, Result}}, NewState};

handle_call({stop}, _From, State) ->
  ?DBG("Stop RTP Process ~p", [self()]),
  {stop, normal, ok, State};
handle_call(Request, _From, State) ->
  ?DBG("Unknown call: ~p", [Request]),
  Reply = pass,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({send_sr, Type},
            #state{audio = AudioDesc,
                    video = VideoDesc} = State) ->
  case Type of
    audio ->
      case AudioDesc of
        #desc{method = MDesc, state = BaseRTP} ->
          {NewBaseRTP, RTCP} = encode(sender_report, BaseRTP),
          case MDesc of
            #ports_desc{addr = Addr, socket_rtcp = RTCPSocket, port_rtcp = PortRTCP} ->
              send_udp(RTCPSocket, Addr, PortRTCP, RTCP);
            #interleaved_desc{socket_owner = SocketOwner, channel_rtcp = ChanRTCP} ->
              send_interleaved(SocketOwner, ChanRTCP, {rtcp, RTCP})
          end,
          NewState = State#state{audio = AudioDesc#desc{state = NewBaseRTP}};
        _ ->
          NewState = State
      end;
    video ->
      case VideoDesc of
        #desc{method = MDesc, state = BaseRTP} ->
          {NewBaseRTP, RTCP} = encode(sender_report, BaseRTP),
          case MDesc of
            #ports_desc{addr = Addr, socket_rtcp = RTCPSocket, port_rtcp = PortRTCP} ->
              send_udp(RTCPSocket, Addr, PortRTCP, RTCP);
            #interleaved_desc{socket_owner = SocketOwner, channel_rtcp = ChanRTCP} ->
              send_interleaved(SocketOwner, ChanRTCP, {rtcp, RTCP})
          end,
          NewState = State#state{video = VideoDesc#desc{state = NewBaseRTP}};
        _ ->
          NewState = State
      end
  end,
  {noreply, NewState};

handle_info(#video_frame{content = audio, flavor = frame,
                         codec = Codec, sound = {_Channel, _Size, _Rate},
                         body = Body},
            #state{audio = AudioDesc} = State) ->
  case AudioDesc of
    #desc{method = MDesc, state = BaseRTP} ->
      %%?DBG("DS: Audio Frame(~p) (pl ~p):~n~p", [self(), iolist_size(Body), Frame]),
      %% Send Audio
      {NewBaseRTP, RTPs} = encode(rtp, inc_timecode(BaseRTP), Codec, Body),
      case MDesc of
        #ports_desc{addr = Addr, socket_rtp = RTPSocket, port_rtp = PortRTP} ->
          %%?DBG("RTP to ~p:~p :~n~p", [Addr, PortRTP, RTPs]),
          send_udp(RTPSocket, Addr, PortRTP, RTPs);
        #interleaved_desc{socket_owner = SocketOwner, channel_rtp = ChanRTP, channel_rtcp = _ChanRTCP} ->
          send_interleaved(SocketOwner, ChanRTP, {rtp, RTPs})
      end,
      NewState = State#state{audio = AudioDesc#desc{state = NewBaseRTP}};
    _ ->
      NewState = State
  end,
  {noreply, NewState};


handle_info(#video_frame{content = video, flavor = Flavor,
                         codec = Codec, body = Body},
            #state{video = VideoDesc} = State) ->
  case VideoDesc of
    #desc{method = MDesc, state = #base_rtp{framelens = CurFLS} = BaseRTP} ->
      %%?DBG("DS: Video Frame(~p):~n~p", [self(), Frame]),
      %%?DBG("VideoDesc: ~p", [VideoDesc]),
      %%?DBG("Video Frame(~p):~n~p", [self(), Frame]),
      %% Send Video
      case Flavor of
        config ->
          case h264:unpack_config(Body) of
            {FrameLength, [SPS, PPS]} ->    % TODO: store FrameLength into State
              Data = {config, [SPS, PPS]};
            _ ->
              FrameLength = CurFLS,
              <<_:64,Data/binary>> = Body
          end;
        keyframe ->
          FrameLength = CurFLS,
          Data = {keyframe, Body};
        frame ->
          FrameLength = CurFLS,
          Data = {frame, Body}
      end,

      {NewBaseRTP, RTPs} = encode(rtp, inc_timecode(BaseRTP#base_rtp{framelens = FrameLength}), Codec, Data),
      case MDesc of
        #ports_desc{addr = Addr, socket_rtp = RTPSocket, port_rtp = PortRTP} ->
          %%?DBG("RTPs to ~p:~p~n~p", [Addr, PortRTP, RTPs]),
          send_udp(RTPSocket, Addr, PortRTP, RTPs);
        #interleaved_desc{socket_owner = SocketOwner, channel_rtp = ChanRTP, channel_rtcp = _ChanRTCP} ->
          send_interleaved(SocketOwner, ChanRTP, {rtp, RTPs})
      end,
      NewState = State#state{video = VideoDesc#desc{state = NewBaseRTP}};
    _ ->
      NewState = State
  end,
  {noreply, NewState};

handle_info({udp, SSocket, SAddr, SPort, Data},
            #state{audio = AudioDesc,
                   video = VideoDesc} = State) ->
  {AudioRTCPSock, AudioRTPSock} =
    if is_record(AudioDesc, desc) ->
        {(AudioDesc#desc.method)#ports_desc.socket_rtcp, (AudioDesc#desc.method)#ports_desc.socket_rtp};
       true ->
        {undefined, undefined}
    end,
  {VideoRTCPSock, VideoRTPSock} =
    if is_record(VideoDesc, desc) ->
        {(VideoDesc#desc.method)#ports_desc.socket_rtcp, (VideoDesc#desc.method)#ports_desc.socket_rtp};
       true ->
        {undefined, undefined}
    end,

  case SSocket of
    AudioRTCPSock ->
      %%?DBG("Audio RTCP", []),
      do_audio_rtcp;
    AudioRTPSock ->
      %%?DBG("Audio RTP", []),
      do_audio_rtp;
    VideoRTCPSock ->
      %%?DBG("Video RTCP", []),
      do_video_rtcp;
    VideoRTPSock ->
      %%?DBG("Video RTP", []),
      do_video_rtp;
    _Other ->
      ?DBG("Error: Other case: ~p, ~p, ~p", [SSocket, AudioDesc, VideoDesc]),
      error
  end,
  {noreply, State};

handle_info({interleaved, rtp, RTP}, State) ->
  ?DBG("Interleaved RTP: ~p", [RTP]),
  {noreply, State};
handle_info({interleaved, rtcp, RTCP}, State) ->
  ?DBG("Interleaved RTCP: ~p", [RTCP]),
  {noreply, State};
handle_info({ems_stream, _, play_complete, _}, State) ->
  {stop, normal, State};
handle_info(Info, State) ->
  ?DBG("Unknown message: ~p", [Info]),
  {noreply, State}.

terminate(Reason, _State) ->
    ?DBG("RTP Process ~p terminates: ~p", [self(), Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% API
play(Pid, Fun, Media) ->
  gen_server:call(Pid, {play, Fun, Media}).

add_stream(Pid, Stream, Proto, Addr, {Method, Params}) ->
  gen_server:call(Pid, {add_stream, Stream, Proto, Addr, {Method, Params}}).

stop(Pid) ->
  gen_server:call(Pid, {stop}).

send_udp(Socket, Addr, Port, RTPs) ->
  F = fun(P) ->
          gen_udp:send(Socket, Addr, Port, P)
      end,
  send_rtp(F, RTPs).

send_interleaved(SockOwner, Channel, {Type, RTPs}) ->
  F = fun(P) ->
          SockOwner ! {interleaved, Channel, {Type, P}}
      end,
  send_rtp(F, RTPs).

send_rtp(F, RTP) when is_binary(RTP) ->
  F(RTP);
send_rtp(F, RTPs) when is_list(RTPs) ->
  [begin
     if is_list(R) ->
         [F(Rr) || Rr <- R];
        true ->
         F(R)
     end
   end || R <- RTPs].


configure(Body, RTPStreams) ->
  configure(Body, RTPStreams, self()).

configure(Body, RTPStreams, Media) ->
  Streams = sdp:decode(Body),

  {Streams1, Frames} = config_media(Streams),

  RtpStreams2 = configure(Streams1, RTPStreams, Media, 0),
  {Streams1, RtpStreams2, Frames}.


configure([], RTPStreams, _, _) ->
  RTPStreams;

configure([undefined | Streams], RTPStreams, Media, RTP) ->
  configure(Streams, RTPStreams, Media, RTP+2);

configure([#media_desc{} = Stream | Streams], RTPStreams, Media, RTP) ->
  RtpConfig = rtp_server:init(Stream, Media),
  RtpStreams1 = setelement(RTP+1, RTPStreams, {Stream#media_desc.type, RtpConfig}),
  RTPStreams2 = setelement(RTP+2, RtpStreams1, {rtcp, RTP}),
  configure(Streams, RTPStreams2, Media, RTP+2).


presync(Streams, Info) ->
  {Now, _} = erlang:statistics(wall_clock),
  presync(Streams, Info, 1, Now).

presync(Streams, [], _N, _Now) ->
  Streams;

presync(Streams, [_RTP | Info], N, Now) when element(N, Streams) == undefined ->
  presync(Streams, Info, N+2, Now);

presync(Streams, [RTP | Info], N, Now) ->
  {Type, Stream} = element(N, Streams),

  RTPSeq = proplists:get_value("seq", RTP),
  RTPTime = proplists:get_value("rtptime", RTP),
  % ?D({"Presync", RTPSeq, RTPTime}),
  Stream1 =
    Stream#base_rtp{sequence = list_to_integer(RTPSeq) - 2,
                    base_timecode = list_to_integer(RTPTime),
                    timecode = list_to_integer(RTPTime),
                    synced = true,
                    wall_clock = Now},
  presync(setelement(N, Streams, {Type, Stream1}), Info, N+2, Now).

config_media(Streams) -> config_media(Streams, [], []).

config_media([], Output, Frames) -> {lists:reverse(Output), Frames};
config_media([#media_desc{codec = Codec} | Streams], Output, Frames) when not is_atom(Codec) ->
  ?D({"Unknown rtp codec", Codec}),
  config_media(Streams, [undefined | Output], Frames);

config_media([#media_desc{type = video, codec = h264, pps = PPS, sps = SPS} = Stream | Streams], Output, Frames) ->
  {H264, _} = h264:decode_nal(SPS, #h264{}),
  {H264_2, _} = h264:decode_nal(PPS, H264),
  Configs = case h264:video_config(H264_2) of
    undefined -> [];
    Config -> [Config]
  end,
  config_media(Streams, [Stream#media_desc{config = H264_2} | Output], Configs ++ Frames);

config_media([#media_desc{type = audio, codec = aac, config = Config} = Stream | Streams], Output, Frames) when is_binary(Config) ->
  AudioConfig = #video_frame{
   	content = audio,
   	flavor  = config,
		dts     = 0,
		pts     = 0,
		body    = Config,
	  codec	  = aac,
	  sound	  = {stereo, bit16, rate44}
	},
  config_media(Streams, [Stream | Output], [AudioConfig | Frames]);

config_media([#media_desc{} = Stream | Streams], Output, Frames) ->
  config_media(Streams, [Stream | Output], Frames).



init(#media_desc{type = video, clock_map = ClockMap, config = H264, codec = Codec}, Media) ->
  #video{media = Media, clock_map = ClockMap, h264 = H264, codec = Codec};

init(#media_desc{type = audio, clock_map = ClockMap, codec = Codec}, Media) ->
  #audio{media = Media, clock_map = ClockMap, codec = Codec};

init(undefined, _Media) ->
  undefined.

open_ports(audio) ->
  try_rtp(8000);

open_ports(video) ->
  try_rtp(5000).

try_rtp(40000) ->
  error;

try_rtp(Port) ->
  case gen_udp:open(Port, [binary, {active, true}, {recbuf, 1048576}]) of
    {ok, RTPSocket} ->
      try_rtcp(Port, RTPSocket);
    {error, _} ->
      try_rtp(Port + 2)
  end.

try_rtcp(RTP, RTPSocket) ->
  RTCP = RTP+1,
  case gen_udp:open(RTCP, [binary, {active, true}]) of
    {ok, RTCPSocket} ->
      {RTP, RTPSocket, RTCP, RTCPSocket};
    {error, _} ->
      gen_udp:close(RTPSocket),
      try_rtp(RTP + 2)
  end.

%
%  0                   1                   2                   3
%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%  |V=2|P|X|  CC   |M|     PT      |       sequence number         |
%  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%  |                           timestamp                           |
%  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%  |           synchronization source (SSRC) identifier            |
%  +=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
%  |            contributing source (CSRC) identifiers             |
%  |                             ....                              |
%  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

get_socket(Type, State, Media) ->
  link(Media),
  receive
    {socket, RTPSocket, RTCPSocket} ->
      inet:setopts(RTPSocket, [{active, true}]),
      inet:setopts(RTCPSocket, [{active, true}]),
      ?MODULE:wait_data(#rtp_state{rtp_socket = RTPSocket, rtcp_socket = RTCPSocket, type = Type, state = State})
  after
    50000 ->
      error_logger:error_msg("RTP socket timeout: ~p~n", [Type])
  end.

wait_data(#rtp_state{rtp_socket = RTPSocket, rtcp_socket = RTCPSocket, state = State, type = Type} = RtpStream) ->
  receive
    {udp,RTPSocket,_Host,_Port, Bin} ->
      % read_video(Video, {data, Body, Sequence, Timestamp});
      State1 = decode(Type, State, Bin),
      ?MODULE:wait_data(RtpStream#rtp_state{state = State1});

    {udp,RTCPSocket,_Host,_Port, Bin} ->
      % inet:setopts(RTPSocket, [{active, once}]),
      State1 = decode(rtcp, State, Bin),
      ?MODULE:wait_data(RtpStream#rtp_state{state = State1});
    Else ->
      ?D({"Unknown", Else}),
      ok
  after
    50000 ->
      error_logger:error_msg("RTP timeout: ~p~n", [Type])
  end.

decode(rtcp, State, <<2:2, 0:1, _Count:5, ?RTCP_SR, _/binary>> = SR) ->
  ?MODULE:rtcp_sr(State, SR);

decode(rtcp, State, <<2:2, 0:1, _Count:5, ?RTCP_RR, _/binary>> = RR) ->
  ?MODULE:rtcp_rr(State, RR);

decode(rtcp, _State, <<2:2, 0:1, _Count:5, Type, _/binary>>) ->
  erlang:error({unhandled_rtcp, Type});

decode(_Type, State, <<2:2, 0:1, _Extension:1, 0:4, _Marker:1, _PayloadType:7, _Sequence:16, _Timecode:32, _StreamId:32, _Data/binary>>) when element(#base_rtp.base_timecode, State) == undefined ->
  {State, []};

% decode(Type, State, <<2:2, 0:1, _Extension:1, 0:4, _Marker:1, _PayloadType:7, Sequence:16, Timecode:32, _StreamId:32, Data/binary>>) when element(#base_rtp.base_timecode, State) == undefined ->
%   ?D({autosync),
%   {{Type,State1}} = rtp_server:presync({{Type,State}}, [[{"seq",integer_to_list(Sequence)},{"rtptime",integer_to_list(Timecode)}]]),
%   ?MODULE:Type(State1, {data, Data, Sequence, Timecode});
%   % {State, []};

decode(Type, State, <<2:2, 0:1, _Extension:1, 0:4, _Marker:1, _PayloadType:7, Sequence:16, Timecode:32, _StreamId:32, Data/binary>>)  ->
  % ?D({Type, Sequence, Timecode, element(#base_rtp.base_timecode, State)}),
  ?MODULE:Type(State, {data, Data, Sequence, Timecode}).


convert_timecode(State) ->
  Timecode = State#base_rtp.timecode,
  ClockMap = State#base_rtp.clock_map,
  BaseTimecode = State#base_rtp.base_timecode,
  WallClock = State#base_rtp.wall_clock,
  _BaseWallClock = State#base_rtp.base_wall_clock,
  % ?D({"TC", WallClock, Timecode, BaseTimecode, ClockMap}),
  WallClock + (Timecode - BaseTimecode)/ClockMap.


%%
%% http://webee.technion.ac.il/labs/comnet/netcourse/CIE/RFC/1889/19.htm
%%
%% or google:  RTCP Sender Report
rtcp_sr(State, <<2:2, 0:1, _Count:5, ?RTCP_SR, _Length:16, StreamId:32, NTP:64, Timecode:32, _PacketCount:32, _OctetCount:32, _Rest/binary>>) ->
  WallClock = round((NTP / 16#100000000 - ?YEARS_70) * 1000),
  _ClockMap = State#base_rtp.clock_map,
  State1 =
    case State#base_rtp.base_wall_clock of
      undefined -> State#base_rtp{base_wall_clock = WallClock - 2000};
      _ -> State
    end,
  State2 = setelement(#base_rtp.wall_clock, State1, WallClock - element(#base_rtp.base_wall_clock, State1)),
  State3 = setelement(#base_rtp.base_timecode, State2, Timecode),
  State4 = case element(#base_rtp.base_timecode, State) of
    undefined -> State3;
    _ -> State
  end,
  State5 = setelement(#base_rtp.stream_id, State4, StreamId),
  State6 = setelement(#base_rtp.timecode, State5, Timecode),
  State7 = setelement(#base_rtp.last_sr, State6, NTP),

  % decode_sender_reports(Count, Rest),

  {State7, []}.


%% This part of Sender Report is useless to me, however not to forget, I've added parsing
%% decode_sender_reports(0, <<_FractionLost, _Lost:24, _MaxSeq:32, _Jitter:32, _LSR:32, _DLSR:32>>) ->
%% decode_sender_reports(_, _) ->
%%   _Delay = _DLSR / 65.536,
%%   ?D({sr, FractionLost, Lost, MaxSeq, Jitter, LSR, DLSR, round(Delay)}),
%%   ok.

audio(#audio{media = _Media, audio_headers = <<>>, codec = aac} = Audio, {data, <<AULength:16, AUHeaders:AULength/bitstring, AudioData/binary>>, _Sequence, _Timestamp}) ->
  unpack_aac_units(Audio#audio{audio_headers = AUHeaders, audio_data = AudioData}, []);

audio(#audio{media = _Media, audio_data = AudioData, codec = aac} = Audio, {data, Bin, _Sequence, _Timestamp}) ->
  unpack_aac_units(Audio#audio{audio_data = <<AudioData/binary, Bin/binary>>}, []);

audio(#audio{codec = Codec} = Audio, {data, Bin, _Sequence, Timestamp}) when Codec == pcma orelse Codec == pcmu orelse Codec == g726_16 ->
  DTS = convert_timecode(Audio#audio{timecode = Timestamp}),
  % ?D({"Audio", size(Bin), DTS}),
  Frame = #video_frame{
    content = audio,
    dts     = DTS,
    pts     = DTS,
    body    = Bin,
	  codec	  = Codec,
	  flavor  = frame,
	  sound	  = {mono, bit8, rate11}
  },
  _Samples =
    case Codec of
      g726_16 -> 1344;
      _ -> 1024
    end,
  {Audio, [Frame]}.


unpack_aac_units(#audio{audio_headers = <<>>} = Audio, Frames) ->
  {Audio#audio{audio_headers = <<>>, audio_data = <<>>}, lists:reverse(Frames)};

unpack_aac_units(#audio{audio_data = <<>>} = Audio, Frames) ->
  {Audio#audio{audio_headers = <<>>, audio_data = <<>>}, lists:reverse(Frames)};

unpack_aac_units(#audio{clock_map = _ClockMap, audio_headers = <<AUSize:13, _Delta:3, AUHeaders/bitstring>>, audio_data = AudioData, timecode = Timecode, codec = Codec} = Audio, Frames) ->
  DTS = convert_timecode(Audio),
  % ?D({"Audio", Codec, Timecode, DTS}),
  case AudioData of
    <<Data:AUSize/binary, Rest/binary>> ->
      AudioFrame = #video_frame{
        content = audio,
        dts     = DTS,
        pts     = DTS,
        body    = Data,
    	  codec	  = Codec,
    	  flavor  = frame,
    	  sound	  = {stereo, bit16, rate44}
      },
      unpack_aac_units(Audio#audio{audio_headers = AUHeaders, audio_data = Rest, timecode = Timecode + 1024}, [AudioFrame | Frames]);
    _ ->
      {Audio, lists:reverse(Frames)}
  end.

video(#video{timecode = undefined} = Video, {data, _, _, Timecode} = Packet) ->
  ?D({"No timecode"}),
  video(Video#video{timecode = Timecode}, Packet);

video(#video{sequence = undefined} = Video, {data, _, Sequence, _} = Packet) ->
  ?D({"Reset seq to", Sequence}),
  video(Video#video{sequence = Sequence - 1}, Packet);

video(#video{sequence = PrevSeq} = Video, {data, _, Sequence, _} = Packet) when Sequence /= (PrevSeq + 1) rem 65536 ->
  ?D({PrevSeq + 1, Sequence}),
  video(Video#video{broken = true, sequence = Sequence - 1}, Packet);

video(#video{h264 = H264, buffer = Buffer, timecode = Timecode, codec = h264} = Video, {data, Body, Sequence, Timecode}) ->
  {H264_1, Frames} = h264:decode_nal(Body, H264),
  {Video#video{sequence = Sequence, h264 = H264_1, buffer = Buffer ++ Frames}, []};

video(#video{timecode = _Timecode, broken = _Broken} = Video, {data, <<>>, Sequence, NewTimecode}) ->
  ?D({"Warning! Zero frame"}),
  {Video#video{sequence = Sequence, timecode = NewTimecode}};

video(#video{h264 = H264, timecode = Timecode, broken = Broken, codec = h264} = Video, {data, Body, Sequence, NewTimecode}) ->

  {Video1, Frames} = case Broken of
    true -> ?D({"Drop broken video frame", Timecode}), {Video, []};
    false -> send_video(Video)
  end,
  {H264_1, NewFrames} = h264:decode_nal(Body, H264),
  % ?D({"ZZZ", Timecode, size(Body), NewFrames}),

  {Video1#video{sequence = Sequence, broken = false, h264 = H264_1, buffer = NewFrames, timecode = NewTimecode}, Frames}.

send_video(#video{synced = false, buffer = [#video_frame{flavor = frame} | _]} = Video) ->
  {Video#video{buffer = []}, []};

send_video(#video{buffer = []} = Video) ->
  {Video, []};

send_video(#video{media = _Media, buffer = Frames, timecode = _Timecode, h264 = H264} = Video) ->
  Frame = lists:foldl(fun(_, undefined) -> undefined;
                         (#video_frame{body = NAL} = F, #video_frame{body = NALs}) ->
                                F#video_frame{body = <<NALs/binary, NAL/binary>>}
                      end, #video_frame{body = <<>>}, Frames),
  Timestamp = convert_timecode(Video),
  % ?D({"Video", _Timecode, Timestamp}),
  Frames1 = case Frame of
    undefined -> [];
    _ -> [Frame#video_frame{dts = Timestamp, pts = Timestamp, content = video}]
  end,
  Frames2 = case Frame of
    #video_frame{content = video, flavor = keyframe} ->
      Config = h264:video_config(H264),
      [Config#video_frame{dts = Timestamp, pts = Timestamp} | Frames1];
    _ -> Frames1
  end,
  {Video#video{synced = true, buffer = []}, Frames2}.


%%----------------------------------------------------------------------
%% @spec (receiver_report, RtpState) -> Data::binary()
%%
%% @doc Creates different RTCP packets
%%
%% http://webee.technion.ac.il/labs/comnet/netcourse/CIE/RFC/1889/20.htm
%%
%% or google:  RTCP Receiver Report
%% @end
%%----------------------------------------------------------------------

-define(RTP_SIZE, 1100).

encode(rtp, BaseRTP, Codec, Data) ->
  case Codec of
    pcm_le ->
      {NewBaseRTP, Packs} = compose_rtp(BaseRTP, l2b(Data), ?RTP_SIZE);
    mp3 ->
      Size = size(Data),
      %% Add support of frames with size more than 14 bit
      %% (set continuating flag, split to several RTP: http://tools.ietf.org/html/rfc5219#section-4.2)
      ADU =
        if Size < 16#40 ->                           % more than 6 bit
            <<0:1,0:1,Size:6>>;
           Size < 16#4000 ->
            <<0:1,1:1,Size:14>>;
           true ->
            ?DBG("Error: big frame", []),
            <<>>
        end,
      MP3 = <<ADU/binary, Data/binary>>,
      {NewBaseRTP, Packs} = compose_rtp(BaseRTP, MP3);
    %% mp3 ->
    %%   MPEG = <<0:16,0:16,Data/binary>>,
    %%   {NewBaseRTP, Packs} = compose_rtp(BaseRTP, MPEG);
    aac ->
      AH = 16#00,
      ASsize = 16#10,                           % TODO: size of > 16#ff
      DataSize = bit_size(Data),
      Size = <<DataSize:2/big-integer-unit:8>>,
      AS = <<ASsize:8, Size/binary>>,
      Header = <<AH:8,AS/binary>>,
      AAC = <<Header/binary,Data/binary>>,
      {NewBaseRTP, Packs} = compose_rtp(BaseRTP#base_rtp{marker = true}, AAC);
    h264 ->
      case Data of
        {config, [SPS, PPS]} ->
          {BR1, [Pack1]} = compose_rtp(BaseRTP, SPS),
          {BR2, [Pack2]} = compose_rtp(BR1#base_rtp{marker = true}, PPS),
          {NewBaseRTP, Packs} = {BR2, [Pack1, Pack2]};
        {KF, Frame} when KF =:= frame;
                         KF =:= keyframe ->
          {BaseRTP1, RevPacks} =
            lists:foldl(fun({M, F}, {BR, Acc}) ->
                            {NewBR, Ps} = compose_rtp(BR#base_rtp{marker = M}, F),
                            {NewBR, [Ps | Acc]}
                        end, {BaseRTP, []},
                        split_h264_frame(BaseRTP#base_rtp.framelens, Frame)),
          NewBaseRTP = BaseRTP1#base_rtp{marker = false},
          Packs = lists:reverse(RevPacks)
      end;
    mpeg4 ->
      {NewBaseRTP, Packs} = compose_rtp(BaseRTP, Data, 1388);
    _ ->
      {NewBaseRTP, Packs} = compose_rtp(BaseRTP, Data) % STUB
  end,
  {NewBaseRTP, Packs}.

split_h264_frame(FLS, Frame) ->
  split_h264_frame(FLS, Frame, []).

split_h264_frame(_FLS, <<>>, Acc) ->
  lists:reverse(Acc);
split_h264_frame(FLS, Frame, Acc) ->
  Len = (8*FLS),
  <<Size:Len, FrameRest/binary>> = Frame,
  <<D:Size/binary-unit:8, Rest/binary>> = FrameRest,
  M = (Rest =:= <<>>),
  split_h264_frame(FLS, Rest, [{M, D} | Acc]).


make_rtp_pack(#base_rtp{codec = PayloadType,
                        sequence = Sequence,
                        timecode = Timestamp,
                        stream_id = SSRC}, Marker, Payload) ->
  Version = 2,
  Padding = 0,
  Extension = 0,
  CSRC = 0,
  <<Version:2, Padding:1, Extension:1, CSRC:4, Marker:1, PayloadType:7, Sequence:16, Timestamp:32, SSRC:32, Payload/binary>>.


%% Compose one RTP-packet from whole Data
compose_rtp(Base, Data) ->
  compose_rtp(Base, Data, undefined, []).

%% Compose number of RTP-packets from splitten Data to Size
compose_rtp(Base, Data, Size)
  when is_integer(Size) ->
  compose_rtp(Base, Data, Size, []).

compose_rtp(Base, <<>>, _, Acc) -> % Return new Sequence ID and list of RTP-binaries
  %%?DBG("New Sequence: ~p", [Sequence]),
  {Base, lists:reverse(Acc)};
compose_rtp(#base_rtp{sequence = Sequence, marker = _Marker,
                      packets = Packets, bytes = Bytes} = Base, Data, Size, Acc)
  when (is_integer(Size) andalso (size(Data) > Size)) ->
  %%?DBG("Chunk ~b", [?RTP_SIZE]),
  <<P:Size/binary,Rest/binary>> = Data,
  %% if Marker -> M = 1;
  %%    true -> M = 0
  %% end,
  M = 0,
  Pack = make_rtp_pack(Base, M, P),
  compose_rtp(Base#base_rtp{sequence = inc_seq(Sequence),
                            packets = inc_packets(Packets, 1),
                            bytes = inc_bytes(Bytes, size(Pack))}, Rest, Size, [Pack | Acc]);
compose_rtp(#base_rtp{sequence = Sequence, marker = Marker,
                      packets = Packets, bytes = Bytes} = Base, Data, Size, Acc) ->
  %%?DBG("Chunk ~b", [size(Data)]),
  %% if not Marker -> M = 0;
  %%    true -> M = 1
  %% end,
  %%M = 1,
  if Marker -> M = 1; true -> M = 0 end,
  Pack = make_rtp_pack(Base, M, Data),
  compose_rtp(Base#base_rtp{sequence = inc_seq(Sequence),
                            packets = inc_packets(Packets, 1),
                            bytes = inc_bytes(Bytes, size(Pack))}, <<>>, Size, [Pack | Acc]).

init_rnd_seq() ->
  random:uniform(16#FFFF).

init_rnd_ssrc() ->
  random:uniform(16#FFFFFFFF).

init_rnd_timecode() ->
  Range = 1000000000,
  random:uniform(Range) + Range.

get_wall_clock() ->
  now().

get_date() ->
  {A1, A2, A3} = now(),
  {(A1*1000*1000) + A2 + ?YEARS_70, A3*1000}.

inc_timecode(#base_rtp{base_wall_clock = _BWC,
                       wall_clock = WC,
                       timecode = TC,
                       media = _Type,
                       clock_map = ClockMap} = State) ->
  NewWC = now(),
  Inc = trunc(ClockMap * timer:now_diff(NewWC, WC)/(1000*1000)),
  NewTC = TC + Inc,
  State#base_rtp{timecode = NewTC, wall_clock = NewWC}.

inc_seq(S) ->
  (S+1) band 16#FFFF.

inc_packets(S, V) ->
  (S+V) band 16#FFFFFFFF.

inc_bytes(S, V) ->
  (S+V) band 16#FFFFFFFF.

l2b(Bin) ->
    l2b(Bin, []).

l2b(<<>>, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
l2b(<<A:2/little-unit:8,Rest/binary>>, Acc) ->
    l2b(Rest, [<<A:2/big-unit:8>> | Acc]).


encode(receiver_report, State) ->
  Count = 0,
  StreamId = element(#base_rtp.stream_id, State),
  Length = 16,
  FractionLost = 0,
  LostPackets = 0,
  MaxSeq = case element(#base_rtp.sequence, State) of
    undefined -> 0;
    MS -> MS
  end,
  Jitter = 0,
  LSR = element(#base_rtp.last_sr, State),
  DLSR = 0,
  % ?D({rr, StreamId, MaxSeq, LSR}),
  {State, <<2:2, 0:1, Count:5, ?RTCP_RR, Length:16, StreamId:32, FractionLost, LostPackets:24, MaxSeq:32, Jitter:32, LSR:32, DLSR:32>>};

encode(sender_report, #base_rtp{stream_id = StreamId,
                                media = _Type,
                                timecode = Timestamp,
                                packets = SPC,
                                bytes = SOC} = State) ->
  Count = 0,
  {MSW, LSW} = get_date(),
  Packet = <<StreamId:32, MSW:32, LSW:32, Timestamp:32, SPC:32, SOC:32>>,
  Length = trunc(size(Packet)/4),
  Header = <<2:2, 0:1, Count:5, ?RTCP_SR, Length:16>>,
  {State, <<Header/binary,Packet/binary>>}.
