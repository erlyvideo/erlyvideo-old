-module(rtp_rtsp).

-include_lib("erlmedia/include/h264.hrl").
-include_lib("erlmedia/include/aac.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("rtp.hrl").
-include("log.hrl").


-export([video/2, audio/2, rtcp_sr/2]).
-export([encode/2, decode/3, init/2, get_socket/3, wait_data/1]).
-export([configure/2, configure/3, presync/2]).

-record(rtp_state, {
  rtcp_socket,
  rtp_socket,
  type,
  state
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
  offset_f,
  h264 = #h264{},
  broken = false,
  buffer = [],
  stream_info  
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
  offset_f,
  audio_rate,
  audio_headers = <<>>,
  audio_data = <<>>,
  stream_info
}).


configure(Body, RTPStreams) ->
  configure(Body, RTPStreams, self()).

configure(Body, RTPStreams, Media) ->
  ?D(Body),
  #media_info{audio = A, video = V} = sdp:decode(Body),
  Streams = A ++ V,
  
  Frames = [F || F <- [video_frame:config_frame(Stream) || Stream <- Streams], F =/= undefined],

  RtpStreams2 = configure(Streams, RTPStreams, Media, 0),
  {Streams, RtpStreams2, Frames}.


configure([], RTPStreams, _, _) ->
  RTPStreams;

configure([undefined | Streams], RTPStreams, Media, RTP) ->
  configure(Streams, RTPStreams, Media, RTP+2);

configure([#stream_info{} = Stream | Streams], RTPStreams, Media, RTP) ->
  RtpConfig = ?MODULE:init(Stream, Media),
  RtpStreams1 = setelement(RTP+1, RTPStreams, {Stream#stream_info.content, RtpConfig}),
  RTPStreams2 = setelement(RTP+2, RtpStreams1, {rtcp, RTP}),
  configure(Streams, RTPStreams2, Media, RTP+2).


presync(Streams, Info) ->
  % {Now, _} = erlang:statistics(wall_clock),
  Now = 0,
  presync(Streams, Info, 1, Now).

presync(Streams, [], _N, _Now) ->
  Streams;

presync(Streams, [_RTP | Info], N, Now) when element(N, Streams) == undefined ->
  presync(Streams, Info, N+2, Now);


presync(Streams, [RTP | Info], N, Now) ->
  {Type, Stream} = element(N, Streams),

  RTPSeq = proplists:get_value("seq", RTP),
  RTPTime = proplists:get_value("rtptime", RTP),
  % ?D({"Presync", RTPSeq, RTPTime,Now}),
  Stream1 = setelement(#base_rtp.sequence, Stream, list_to_integer(RTPSeq) - 1),
  Stream2 = setelement(#base_rtp.base_timecode, Stream1, list_to_integer(RTPTime)),
  Stream3 = setelement(#base_rtp.timecode, Stream2, list_to_integer(RTPTime)),
  Stream4 = setelement(#base_rtp.synced, Stream3, true),
  Stream5 = setelement(#base_rtp.wall_clock, Stream4, Now),
  presync(setelement(N, Streams, {Type, Stream5}), Info, N+2, Now).

init(#stream_info{content = video, codec = Codec, timescale = ClockMap} = Stream, Media) ->
  #video{media = Media, stream_info = Stream, clock_map = ClockMap, h264 = h264:init(), codec = Codec};

init(#stream_info{content = audio, codec = Codec, timescale = ClockMap} = Stream, Media) ->
  #audio{media = Media, stream_info = Stream, clock_map = ClockMap, codec = Codec};

init(undefined, _Media) ->
  undefined.


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
  ?D({rtcp, sr, SR}),
  ?MODULE:rtcp_sr(State, SR);

decode(rtcp, State, <<2:2, 0:1, _Count:5, ?RTCP_RR, _/binary>> = RR) ->
  ?D({rtcp, rr, RR}),
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

decode(Type, State, <<2:2, 0:1, _Extension:1, 0:4, _Marker:1, _PayloadType:7, Sequence:16, Timecode:32, _StreamId:32, Data/binary>> = RTP)  ->
  ?D({Type, RTP}),
  % ?D({Type, Sequence, Timecode, element(#base_rtp.base_timecode, State), element(#base_rtp.wall_clock, State)}),
  ?MODULE:Type(State, {data, Data, Sequence, Timecode}).



audio(#audio{media = _Media, audio_headers = <<>>, codec = aac} = Audio, {data, <<AULength:16, AUHeaders:AULength/bitstring, AudioData/binary>>, _Sequence, _Timestamp}) ->
  unpack_aac_units(Audio#audio{audio_headers = AUHeaders, audio_data = AudioData}, []);

audio(#audio{media = _Media, audio_data = AudioData, codec = aac} = Audio, {data, Bin, _Sequence, _Timestamp}) ->
  unpack_aac_units(Audio#audio{audio_data = <<AudioData/binary, Bin/binary>>}, []);

audio(#audio{codec = Codec, clock_map = Clock} = Audio, {data, Bin, _Sequence, Timestamp}) when Codec == pcma orelse Codec == pcmu orelse Codec == g726_16 ->
  DTS = timecode_to_dts(Audio#audio{timecode = Timestamp}),
  % ?D({"Audio", Codec, Clock, size(Bin), DTS}),
  Frame = #video_frame{
    content = audio,
    dts     = DTS,
    pts     = DTS,
    body    = Bin,
	  codec	  = Codec,
	  flavor  = frame,
	  sound	  = {mono, bit8, Clock}
  },
  {Audio, [Frame]}.


unpack_aac_units(#audio{audio_headers = <<>>} = Audio, Frames) ->
  {Audio#audio{audio_headers = <<>>, audio_data = <<>>}, lists:reverse(Frames)};

unpack_aac_units(#audio{audio_data = <<>>} = Audio, Frames) ->
  {Audio#audio{audio_headers = <<>>, audio_data = <<>>}, lists:reverse(Frames)};

unpack_aac_units(#audio{clock_map = _ClockMap, audio_headers = <<AUSize:13, _Delta:3, AUHeaders/bitstring>>, audio_data = AudioData, timecode = Timecode, codec = Codec} = Audio, Frames) ->
  DTS = timecode_to_dts(Audio),
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

send_video(#video{media = _Media, buffer = Frames, timecode = _Timecode, stream_info = Stream} = Video) ->
  Frame = lists:foldl(fun(_, undefined) -> undefined;
                         (#video_frame{body = NAL} = F, #video_frame{body = NALs}) ->
                                F#video_frame{body = <<NALs/binary, NAL/binary>>}
                      end, #video_frame{body = <<>>}, Frames),
  Timestamp = timecode_to_dts(Video),
  % ?D({"Video", _Timecode, Timestamp}),
  Frames1 = case Frame of
    undefined -> [];
    _ -> [Frame#video_frame{dts = Timestamp, pts = Timestamp, content = video}]
  end,
  Frames2 = case Frame of
    #video_frame{content = video, flavor = keyframe} ->
      Config = video_frame:config_frame(Stream),
      [Config#video_frame{dts = Timestamp, pts = Timestamp} | Frames1];
    _ -> Frames1
  end,
  {Video#video{synced = true, buffer = []}, Frames2}.

timecode_to_dts(State) ->
  Timecode = element(#base_rtp.timecode, State),
  ClockMap = element(#base_rtp.clock_map, State),
  BaseTimecode = element(#base_rtp.base_timecode, State),
  WallClock = element(#base_rtp.wall_clock, State),
  DTS = WallClock + (Timecode - BaseTimecode)/ClockMap,
  % ?D({"->", WallClock, Timecode, BaseTimecode, ClockMap, DTS}),
  DTS.



%%
%% http://webee.technion.ac.il/labs/comnet/netcourse/CIE/RFC/1889/19.htm
%%
%% or google:  RTCP Sender Report
rtcp_sr(State, <<2:2, 0:1, _Count:5, ?RTCP_SR, _Length:16, StreamId:32, NTP:64, Timecode:32, _PacketCount:32, _OctetCount:32, _Rest/binary>>) ->
  WallClock = round((NTP / 16#100000000 - ?YEARS_70) * 1000),
  _ClockMap = element(#base_rtp.clock_map, State),
  State1 = case element(#base_rtp.base_wall_clock, State) of
    undefined -> setelement(#base_rtp.base_wall_clock, State, WallClock - 2000);
    _ -> State
  end,
  BaseWallClock = element(#base_rtp.base_wall_clock, State1),
  State2 = setelement(#base_rtp.wall_clock, State1, WallClock - BaseWallClock),
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
  {State, <<2:2, 0:1, Count:5, ?RTCP_RR, Length:16, StreamId:32, FractionLost, LostPackets:24, MaxSeq:32, Jitter:32, LSR:32, DLSR:32>>}.
