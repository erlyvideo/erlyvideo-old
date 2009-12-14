-module(rtp_server).
-author('max@maxidoors.ru').

-include("../../../include/ems.hrl").
-include("../../../include/h264.hrl").
-include("../../../include/rtsp.hrl").

-record(video, {
	rtcp_socket,
	rtp_socket,
	rtcp_port,
	rtp_port,
  media,
  clock_map,
  sequence = undefined,
  timestamp = undefined,
  h264,
  synced = false,
  broken = false,
  buffer = []
}).

-record(audio, {
	rtcp_socket,
	rtp_socket,
	rtcp_port,
	rtp_port,
  media,
  clock_map
}).
	

%% External API
-export([start_link/3]).

%% gen_server callbacks

-export([video/2, video/1, audio/2, audio/1]).

%%--------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, Pid} | {error, Reason}
%%
%% @doc Called by a supervisor to start the listening process.
%% @end
%%----------------------------------------------------------------------

start_link(Media, Type, Stream)  ->
  {RTP, RTPSocket, RTCP, RTCPSocket} = open_ports(Type),
  Pid = spawn_link(?MODULE, Type, [init(Type, {RTP, RTCP, Media}), Stream]),
  link(Pid),
  gen_udp:controlling_process(RTPSocket, Pid),
  gen_udp:controlling_process(RTCPSocket, Pid),
  Pid ! {socket, RTPSocket, RTCPSocket},
  {ok, Pid, {RTP, RTCP}}.


init(video, {RTP, RTCP, Media}) ->
  #video{rtp_port = RTP, rtcp_port = RTCP, media = Media};

init(audio, {RTP, RTCP, Media}) ->
  #audio{rtp_port = RTP, rtcp_port = RTCP, media = Media}.
  

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


audio(#audio{media = _Media} = Audio, #rtsp_stream{clock_map = ClockMap}) ->
  Config = <<18,16,6>>,
  _AudioConfig = #video_frame{       
   	type          = ?FLV_TAG_TYPE_AUDIO,
   	decoder_config = true,
		timestamp      = 0,
		body          = Config,
	  sound_format	= ?FLV_AUDIO_FORMAT_AAC,
	  sound_type	  = ?FLV_AUDIO_TYPE_STEREO,
	  sound_size	  = ?FLV_AUDIO_SIZE_16BIT,
	  sound_rate	  = ?FLV_AUDIO_RATE_44
	},
  % Media ! AudioConfig,
  
  receive
    {socket, RTPSocket, RTCPSocket} ->
      inet:setopts(RTPSocket, [{active, true}]),
      inet:setopts(RTCPSocket, [{active, true}]),
      ?MODULE:audio(Audio#audio{rtp_socket = RTPSocket, rtcp_socket = RTCPSocket, clock_map = ClockMap})
  end.
  
audio(#audio{rtp_socket = RTPSocket, rtcp_socket = RTCPSocket,clock_map = ClockMap} = Audio) ->
  receive
    {udp,RTPSocket,_Host,_Port, <<2:2, 0:1, _Extension:1, 0:4, _Marker:1, _PayloadType:7, 
             Sequence:16, RtpTs:32, _StreamId:32, Data/binary>> = _Bin} ->
      % read_video(Video, {data, Body, Sequence, Timestamp});
      read_audio(Audio, {data, Data, Sequence, round(RtpTs / ClockMap)});

    {udp,RTCPSocket,_Host,_Port, _Bin} ->
      ?D("RTCP audio"),
      % inet:setopts(RTPSocket, [{active, once}]),
      ?MODULE:audio(Audio);
    Else ->
      ?D({"Unknown", Else}),
      ok
  after
    50000 ->
      ?D("RTP audio timeout")
  end.

read_audio(#audio{media = _Media} = Audio, {data, <<_:3, F:1, S:1, ElementId:5, Fbits:3, Lbits:3, Data/binary>>, _Sequence, Timestamp}) ->
  % ?D({F, S, ElementId, Fbits, Lbits, Timestamp}),
  _AudioFrame = #video_frame{       
    type          = ?FLV_TAG_TYPE_AUDIO,
    timestamp     = Timestamp,
    body          = Data,
    sound_format  = ?FLV_AUDIO_FORMAT_AAC,
    sound_type    = ?FLV_AUDIO_TYPE_STEREO,
    sound_size    = ?FLV_AUDIO_SIZE_16BIT,
    sound_rate    = ?FLV_AUDIO_RATE_44
  },
  % Media ! AudioFrame,
  ?MODULE:audio(Audio).
  

video(#video{media = Media} = Video, #rtsp_stream{clock_map = ClockMap, pps = PPS, sps = SPS}) ->
  {H264, _} = h264:decode_nal(SPS, #h264{}),
  {H264_2, Configs} = h264:decode_nal(PPS, H264),
    
  Video1 = Video#video{clock_map = ClockMap, h264 = H264_2},
  link(Media),
  
  lists:foreach(fun(Frame) ->
    Media ! Frame#video_frame{timestamp = 0}
  end, Configs),

  receive
    {socket, RTPSocket, RTCPSocket} ->
      inet:setopts(RTPSocket, [{active, true}]),
      inet:setopts(RTCPSocket, [{active, true}]),
      ?MODULE:video(Video1#video{rtp_socket = RTPSocket, rtcp_socket = RTCPSocket})
  end.
  
video(#video{rtp_socket = RTPSocket, rtcp_socket = RTCPSocket} = Video) ->
  receive
    {udp,RTPSocket,_Host,_Port, <<2:2, 0:1, _Extension:1, 0:4, _Marker:1, _PayloadType:7, 
             Sequence:16, Timestamp:32, _StreamId:32, Body/binary>> = _Bin} ->
      read_video(Video, {data, Body, Sequence, Timestamp});

    {udp,RTCPSocket,_Host,_Port, _Bin} ->
      ?D("RTCP video"),
      % inet:setopts(RTPSocket, [{active, once}]),
      ?MODULE:video(Video);
    Else ->
      ?D({"Unknown", Else}),
      ok
  after
    50000 ->
      ?D("RTP video timeout")
  end.

read_video(#video{timestamp = undefined} = Video, {data, _, _, Timestamp} = Packet) ->
  read_video(Video#video{timestamp = Timestamp}, Packet);

read_video(#video{sequence = undefined} = Video, {data, _, Sequence, _} = Packet) ->
  ?D({"Reset seq to", Sequence}),
  read_video(Video#video{sequence = Sequence - 1}, Packet);

read_video(#video{sequence = PrevSeq} = Video, {data, _, Sequence, _} = Packet) when Sequence /= PrevSeq + 1->
  ?D({PrevSeq + 1, Sequence}),
  read_video(Video#video{broken = true, sequence = Sequence - 1}, Packet);

read_video(#video{h264 = H264, buffer = Buffer, timestamp = Timestamp} = Video, {data, Body, Sequence, Timestamp}) ->
  {H264_1, Frames} = h264:decode_nal(Body, H264),
  ?MODULE:video(Video#video{sequence = Sequence, h264 = H264_1, buffer = Buffer ++ Frames});
  
read_video(#video{h264 = H264, timestamp = RtpTs, broken = Broken} = Video, {data, Body, Sequence, NewRtpTs}) ->

  Video1 = case Broken of
    true -> ?D({"Drop broken video frame", RtpTs}), Video;
    false -> send_video(Video)
  end,
  {H264_1, Frames} = h264:decode_nal(Body, H264),

  ?MODULE:video(Video1#video{sequence = Sequence, broken = false, h264 = H264_1, buffer = Frames, timestamp = NewRtpTs}).
 
send_video(#video{synced = false, buffer = [#video_frame{frame_type = ?FLV_VIDEO_FRAME_TYPEINTER_FRAME} | _]} = Video) ->
  Video#video{buffer = []};

send_video(#video{buffer = []} = Video) ->
  Video;

send_video(#video{clock_map = ClockMap, media = Media, buffer = Frames, timestamp = RtpTs} = Video) ->
  Frame = lists:foldl(fun(_, undefined) -> undefined;
                         (#video_frame{body = NAL} = F, #video_frame{body = NALs}) -> 
                                F#video_frame{body = <<NALs/binary, NAL/binary>>}
  end, #video_frame{body = <<>>}, Frames),
  case Frame of
    undefined -> ok;
    _ -> Media ! Frame#video_frame{timestamp = round(RtpTs / ClockMap), type = ?FLV_TAG_TYPE_VIDEO}
  end,
  Video#video{synced = true, buffer = []}.
