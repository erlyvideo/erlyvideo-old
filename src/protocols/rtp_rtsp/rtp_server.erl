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

-record(video, {
  media,
  clock_map,
  sequence = undefined,
  base_timestamp = undefined,
  timestamp = undefined,
  h264,
  synced = false,
  broken = false,
  buffer = []
}).

-record(audio, {
  media,
  clock_map,
  audio_headers = <<>>,
  audio_data = <<>>,
  base_timestamp = undefined,
  timestamp
}).
	

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

    {udp,RTCPSocket,_Host,_Port, _Bin} ->
      ?D("RTCP audio"),
      % inet:setopts(RTPSocket, [{active, once}]),
      ?MODULE:wait_data(RtpStream);
    Else ->
      ?D({"Unknown", Else}),
      ok
  after
    50000 ->
      error_logger:error_msg("RTP timeout: ~p~n", [Type])
  end.

decode(Type, State, <<2:2, 0:1, _Extension:1, 0:4, _Marker:1, _PayloadType:7, Sequence:16, RtpTs:32, _StreamId:32, Data/binary>>) ->
  ClockMap = element(3, State),
  Timestamp = round(RtpTs / ClockMap),
  ?MODULE:Type(State, {data, Data, Sequence, Timestamp}).
  
  
audio(#audio{base_timestamp = undefined} = Audio, {data, _, _, Timestamp} = Packet) ->
  audio(Audio#audio{base_timestamp = Timestamp}, Packet);

audio(#audio{media = _Media, audio_headers = <<>>, base_timestamp = BaseTs} = Audio, {data, <<AULength:16, AUHeaders:AULength/bitstring, AudioData/binary>> = Data, _Sequence, Timestamp}) ->
  unpack_audio_units(Audio#audio{audio_headers = AUHeaders, audio_data = AudioData, timestamp = Timestamp - BaseTs});
  
audio(#audio{media = _Media, audio_headers = AUHeaders, audio_data = AudioData} = Audio, {data, Bin, _Sequence, Timestamp}) ->
  unpack_audio_units(Audio#audio{audio_data = <<AudioData/binary, Bin/binary>>, timestamp = Timestamp}).


  
unpack_audio_units(#audio{audio_headers = <<>>} = Audio) ->
  Audio#audio{audio_headers = <<>>, audio_data = <<>>};
  
unpack_audio_units(#audio{audio_data = <<>>} = Audio) ->
  Audio#audio{audio_headers = <<>>, audio_data = <<>>};
  
unpack_audio_units(#audio{media = Media, audio_headers = <<AUSize:13, Delta:3, AUHeaders/bitstring>>, audio_data = AudioData, timestamp = BaseTimestamp, clock_map = ClockMap} = Audio) ->
  Timestamp = BaseTimestamp + 100, %round(Delta * 1024 / ClockMap),
  case AudioData of
    <<Data:AUSize/binary, Rest/binary>> ->
      ?D({"audio", Timestamp}),
      AudioFrame = #video_frame{       
        type          = ?FLV_TAG_TYPE_AUDIO,
        timestamp     = Timestamp,
        body          = Data,
        sound_format  = ?FLV_AUDIO_FORMAT_AAC,
        sound_type    = ?FLV_AUDIO_TYPE_STEREO,
        sound_size    = ?FLV_AUDIO_SIZE_16BIT,
        sound_rate    = ?FLV_AUDIO_RATE_44
      },
      % Media ! AudioFrame,
      unpack_audio_units(Audio#audio{audio_headers = AUHeaders, audio_data = Rest, timestamp = Timestamp});
    _ ->
      Audio
  end.
    
video(#video{base_timestamp = undefined} = Video, {data, _, _, Timestamp} = Packet) ->
  video(Video#video{base_timestamp = Timestamp}, Packet);

video(#video{timestamp = undefined} = Video, {data, _, _, Timestamp} = Packet) ->
  video(Video#video{timestamp = Timestamp}, Packet);

video(#video{sequence = undefined} = Video, {data, _, Sequence, _} = Packet) ->
  ?D({"Reset seq to", Sequence}),
  video(Video#video{sequence = Sequence - 1}, Packet);

video(#video{sequence = PrevSeq} = Video, {data, _, Sequence, _} = Packet) when Sequence /= PrevSeq + 1->
  ?D({PrevSeq + 1, Sequence}),
  video(Video#video{broken = true, sequence = Sequence - 1}, Packet);

video(#video{h264 = H264, buffer = Buffer, timestamp = Timestamp} = Video, {data, Body, Sequence, Timestamp}) ->
  {H264_1, Frames} = h264:decode_nal(Body, H264),
  Video#video{sequence = Sequence, h264 = H264_1, buffer = Buffer ++ Frames};
  
video(#video{h264 = H264, timestamp = RtpTs, broken = Broken} = Video, {data, Body, Sequence, NewRtpTs}) ->

  Video1 = case Broken of
    true -> ?D({"Drop broken video frame", RtpTs}), Video;
    false -> send_video(Video)
  end,
  {H264_1, Frames} = h264:decode_nal(Body, H264),

  Video1#video{sequence = Sequence, broken = false, h264 = H264_1, buffer = Frames, timestamp = NewRtpTs}.
 
send_video(#video{synced = false, buffer = [#video_frame{frame_type = ?FLV_VIDEO_FRAME_TYPEINTER_FRAME} | _]} = Video) ->
  Video#video{buffer = []};

send_video(#video{buffer = []} = Video) ->
  Video;

send_video(#video{media = Media, buffer = Frames, timestamp = RtpTs, base_timestamp = BaseTs} = Video) ->
  Frame = lists:foldl(fun(_, undefined) -> undefined;
                         (#video_frame{body = NAL} = F, #video_frame{body = NALs}) -> 
                                F#video_frame{body = <<NALs/binary, NAL/binary>>}
  end, #video_frame{body = <<>>}, Frames),
  Timestamp = RtpTs - BaseTs,
  ?D({"Video", Timestamp}),
  case Frame of
    undefined -> ok;
    _ -> Media ! Frame#video_frame{timestamp = Timestamp, type = ?FLV_TAG_TYPE_VIDEO}
  end,
  Video#video{synced = true, buffer = []}.
