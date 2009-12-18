-module(rtp_server).
-author('max@maxidoors.ru').

-include("../../../include/ems.hrl").
-include("../../../include/h264.hrl").
-include("../../../include/rtsp.hrl").

-record(rtp_state, {
  rtcp_socket,
  rtp_socket,
  type,
  state
}).

-record(base_rtp, {
  media,
  clock_map,
  base_timestamp = undefined,
  sequence = undefined,
  wall_clock = undefined,
  base_wall_clock = undefined,
  timecode = undefined
}).

-record(video, {
  media,
  clock_map,
  base_timestamp = undefined,
  sequence = undefined,
  wall_clock = undefined,
  base_wall_clock = undefined,
  timecode = undefined,
  h264,
  synced = false,
  broken = false,
  buffer = []
}).

-record(audio, {
  media,
  clock_map,
  base_timestamp = undefined,
  sequence = undefined,
  wall_clock = undefined,
  base_wall_clock = undefined,
  timecode = undefined,
  audio_rate,
  audio_headers = <<>>,
  audio_data = <<>>
}).
	


-define(RTCP_SR, 200).
-define(RTCP_SD, 202).
-define(YEARS_70, 2209032000).  % RTP bases its timestamp on NTP. NTP counts from 1900. Shift it to 1970. This constant is not precise.

%% External API
-export([start_link/2]).

%% gen_server callbacks

-export([video/2, audio/2, decode/3, init/2, get_socket/3, wait_data/1]).

%%--------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, Pid} | {error, Reason}
%%
%% @doc Called by a supervisor to start the listening process.
%% @end
%%----------------------------------------------------------------------

start_link(Media, #rtsp_stream{type = Type} = Stream)  ->
  {RTP, RTPSocket, RTCP, RTCPSocket} = open_ports(Type),
  Pid = spawn_link(?MODULE, get_socket, [Type, init(Stream, Media), Media]),
  link(Pid),
  gen_udp:controlling_process(RTPSocket, Pid),
  gen_udp:controlling_process(RTCPSocket, Pid),
  Pid ! {socket, RTPSocket, RTCPSocket},
  {ok, Pid, {RTP, RTCP}}.


init(#rtsp_stream{type = video, clock_map = ClockMap, config = H264}, Media) ->
  #video{media = Media, clock_map = ClockMap, h264 = H264};

init(#rtsp_stream{type = audio, clock_map = ClockMap}, Media) ->
  #audio{media = Media, clock_map = ClockMap}.
  

open_ports(audio) ->
  try_rtp(8000);

open_ports(video) ->
  try_rtp(5000).

try_rtp(40000) ->
  error;
  
try_rtp(Port) ->
  case gen_udp:open(Port, [binary, {active, false}, {recbuf, 1048576}]) of
    {ok, RTPSocket} ->
      try_rtcp(Port, RTPSocket);
    {error, _} ->
      try_rtp(Port + 2)
  end.

try_rtcp(RTP, RTPSocket) ->
  RTCP = RTP+1,
  case gen_udp:open(RTCP, [binary, {active, false}]) of
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

decode(rtcp, State, <<2:2, 0:1, Count:5, ?RTCP_SR, Length:16, _StreamId:32, NTP:64, Timecode:32, PacketCount:32, OctetCount:32, _/binary>>) ->
  WallClock = round((NTP / 16#100000000 - ?YEARS_70) * 1000),
  ?D({"RTCP", element(1, State), WallClock, Timecode}),
  ClockMap = element(#base_rtp.clock_map, State),
  State1 = case element(#base_rtp.base_wall_clock, State) of
    undefined -> setelement(#base_rtp.base_wall_clock, State, WallClock - 2000);
    _ -> State
  end,
  State2 = setelement(#base_rtp.wall_clock, State1, WallClock - element(#base_rtp.base_wall_clock, State1)),
  State3 = setelement(#base_rtp.base_timestamp, State2, round(Timecode / ClockMap)),
  setelement(#base_rtp.timecode, State3, Timecode);
  % State3.
  
decode(_, State, _Bin) when element(#base_rtp.base_timestamp, State) == undefined ->
  State;

decode(Type, State, <<2:2, 0:1, _Extension:1, 0:4, _Marker:1, PayloadType:7, Sequence:16, Timecode:32, _StreamId:32, Data/binary>>)  ->
  ?MODULE:Type(State, {data, Data, Sequence, Timecode}).


convert_timecode(State) ->
  Timecode = element(#base_rtp.timecode, State),
  ClockMap = element(#base_rtp.clock_map, State),
  BaseTimestamp = element(#base_rtp.base_timestamp, State),
  WallClock = element(#base_rtp.wall_clock, State),
  BaseWallClock = element(#base_rtp.base_wall_clock, State),
  WallClock + Timecode/ClockMap - BaseTimestamp.
  

% rtcp(State, {data, _, _, Timestamp} = Packet) ->
%   audio(Audio#audio{base_timestamp = Timestamp}, Packet)ю
  
  
audio(#audio{media = _Media, audio_headers = <<>>} = Audio, {data, <<AULength:16, AUHeaders:AULength/bitstring, AudioData/binary>>, _Sequence, _Timestamp}) ->
  unpack_audio_units(Audio#audio{audio_headers = AUHeaders, audio_data = AudioData});
  
audio(#audio{media = _Media, audio_data = AudioData} = Audio, {data, Bin, _Sequence, _Timestamp}) ->
  unpack_audio_units(Audio#audio{audio_data = <<AudioData/binary, Bin/binary>>}).


  
unpack_audio_units(#audio{audio_headers = <<>>} = Audio) ->
  Audio#audio{audio_headers = <<>>, audio_data = <<>>};
  
unpack_audio_units(#audio{audio_data = <<>>} = Audio) ->
  Audio#audio{audio_headers = <<>>, audio_data = <<>>};
  
unpack_audio_units(#audio{media = Media, clock_map = ClockMap, audio_headers = <<AUSize:13, Delta:3, AUHeaders/bitstring>>, audio_data = AudioData, timecode = Timecode} = Audio) ->
  Timestamp = convert_timecode(Audio),
  % ?D({"Audio", Timecode, Timestamp}),
  case AudioData of
    <<Data:AUSize/binary, Rest/binary>> ->
      AudioFrame = #video_frame{       
        type          = ?FLV_TAG_TYPE_AUDIO,
        timestamp     = Timestamp,
        body          = Data,
        sound_format  = ?FLV_AUDIO_FORMAT_AAC,
        sound_type    = ?FLV_AUDIO_TYPE_STEREO,
        sound_size    = ?FLV_AUDIO_SIZE_16BIT,
        sound_rate    = ?FLV_AUDIO_RATE_44
      },
      Media ! AudioFrame,
      unpack_audio_units(Audio#audio{audio_headers = AUHeaders, audio_data = Rest, timecode = Timecode + 1024});
    _ ->
      Audio
  end.
    
video(#video{timecode = undefined} = Video, {data, _, _, Timecode} = Packet) ->
  video(Video#video{timecode = Timecode}, Packet);

video(#video{sequence = undefined} = Video, {data, _, Sequence, _} = Packet) ->
  ?D({"Reset seq to", Sequence}),
  video(Video#video{sequence = Sequence - 1}, Packet);

video(#video{sequence = PrevSeq} = Video, {data, _, Sequence, _} = Packet) when Sequence /= PrevSeq + 1->
  ?D({PrevSeq + 1, Sequence}),
  video(Video#video{broken = true, sequence = Sequence - 1}, Packet);

video(#video{h264 = H264, buffer = Buffer, timecode = Timecode} = Video, {data, Body, Sequence, Timecode}) ->
  {H264_1, Frames} = h264:decode_nal(Body, H264),
  Video#video{sequence = Sequence, h264 = H264_1, buffer = Buffer ++ Frames};
  
video(#video{h264 = H264, timecode = Timecode, broken = Broken} = Video, {data, Body, Sequence, NewTimecode}) ->

  Video1 = case Broken of
    true -> ?D({"Drop broken video frame", Timecode}), Video;
    false -> send_video(Video)
  end,
  {H264_1, Frames} = h264:decode_nal(Body, H264),

  Video1#video{sequence = Sequence, broken = false, h264 = H264_1, buffer = Frames, timecode = NewTimecode}.
 
send_video(#video{synced = false, buffer = [#video_frame{frame_type = ?FLV_VIDEO_FRAME_TYPEINTER_FRAME} | _]} = Video) ->
  Video#video{buffer = []};

send_video(#video{buffer = []} = Video) ->
  Video;

send_video(#video{media = Media, buffer = Frames, timecode = Timecode} = Video) ->
  Frame = lists:foldl(fun(_, undefined) -> undefined;
                         (#video_frame{body = NAL} = F, #video_frame{body = NALs}) -> 
                                F#video_frame{body = <<NALs/binary, NAL/binary>>}
  end, #video_frame{body = <<>>}, Frames),
  Timestamp = convert_timecode(Video),
  % ?D({"Video", Timecode, Timestamp}),
  case Frame of
    undefined -> ok;
    _ -> Media ! Frame#video_frame{timestamp = Timestamp, type = ?FLV_TAG_TYPE_VIDEO}
  end,
  Video#video{synced = true, buffer = []}.
