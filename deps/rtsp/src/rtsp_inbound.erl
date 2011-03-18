%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2011 Max Lapshin
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
-module(rtsp_inbound).
-author('Max Lapshin <max@maxidoors.ru>').

-include("log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("erlmedia/include/sdp.hrl").
-include("rtsp.hrl").

-export([handle_call/3, sync_rtp/2, handle_announce_request/4, handle_receive_setup/4, handle_rtp/2]).

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
  {noreply, RTSP#rtsp_socket{pending = From, state = play, seq = Seq + 1}, Timeout}.




sync_rtp(#rtsp_socket{rtp_streams = RtpStreams, control_map = ControlMap, url = URL} = Socket, RtpHeaders) ->
  case proplists:get_value(<<"Rtp-Info">>, RtpHeaders) of
    undefined ->
      Socket;
    Info ->
      % ?D(RtpStreams),
      {ok, Re} = re:compile("([^=]+)=(.*)"),
      F = fun(S) ->
        {match, [_, K, V]} = re:run(S, Re, [{capture, all, list}]),
        {K, V}
      end,
      RtpInfo = [[F(S1) || S1 <- string:tokens(S, ";")] || S <- string:tokens(binary_to_list(Info), ",")],

      Streams1 = lists:foldl(fun(Headers, Streams) ->
        case extract_control(proplists:get_value("url", Headers), URL, ControlMap) of
          undefined -> ?D({unsynced, Headers}), Streams;
          StreamNum ->
            Stream = rtp_decoder:sync(element(StreamNum, Streams), Headers),
            setelement(StreamNum, Streams, Stream)
        end
      end, RtpStreams, RtpInfo),
      Socket#rtsp_socket{rtp_streams = Streams1}
  end.



handle_announce_request(#rtsp_socket{callback = Callback} = Socket, URL, Headers, Body) ->
  Socket1 = #rtsp_socket{pending_reply = {ok, MediaInfo, _}} = rtsp_socket:handle_sdp(Socket, Headers, Body),
  case Callback:announce(URL, Headers, MediaInfo) of
    {ok, Media} ->
      ?D({"Announced to", Media}),
      erlang:monitor(process, Media),
      rtsp_socket:reply(Socket1#rtsp_socket{session = 42, media = Media, direction = in}, "200 OK", [{'Cseq', seq(Headers)}]);
    {error, authentication} ->
      rtsp_socket:reply(Socket1, "401 Unauthorized", [{"WWW-Authenticate", "Basic realm=\"Erlyvideo Streaming Server\""}, {'Cseq', seq(Headers)}])
  end.

handle_receive_setup(#rtsp_socket{} = Socket, URL, Headers, _Body) ->
  {match, [Control]} = re:run(URL, "/([^/]+)$", [{capture, all_but_first, list}]),
  StreamNum = proplists:get_value(Control, Socket#rtsp_socket.control_map),
  StreamInfo = element(StreamNum, Socket#rtsp_socket.rtp_streams),
  Streams = setelement(StreamNum, Socket#rtsp_socket.rtp_streams, rtp_decoder:init(StreamInfo)),
  rtsp_socket:reply(Socket#rtsp_socket{rtp_streams = Streams}, "200 OK", [{'Cseq', seq(Headers)}, {'Session', 42}, {'Transport', proplists:get_value('Transport', Headers)}]).



seq(Headers) ->
  proplists:get_value('Cseq', Headers, 1).



append_trackid(_URL, ("rtsp://"++ _) = TrackID) ->
  TrackID;

append_trackid(URL, TrackID) ->
  case string:tokens(URL, "?") of
    [URL1, URL2] -> URL1 ++ "/" ++ TrackID ++ "?" ++ URL2;
    [URL] -> URL ++ "/" ++ TrackID
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
      {RtpState2, RtcpData} = rtp_decoder:rtcp_rr(RtpState1),
      RTCP_RR = packet_codec:encode({rtcp, RtpNum, RtcpData}),
      gen_tcp:send(Sock, RTCP_RR),
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



extract_control(ControlUrl, URL, ControlMap) ->
  case proplists:get_value(ControlUrl, ControlMap) of
    undefined ->
      {_Proto, _Auth, _Addr, _Port, Path1, _Query1} = http_uri2:parse(ControlUrl),
      {_Proto, _Auth, _Addr, _Port, Path2, _Query2} = http_uri2:parse(URL),
      Control = string:sub_string(Path1, length(string:strip(Path2, right, $/)) + 2),
      proplists:get_value(Control, ControlMap);
    Else ->
      Else
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

extract_control_test_() ->
  [
    ?_assertEqual(1, extract_control("track1", "rtsp://95.34.123.4:554/h264", [{"track1",1}])),
    ?_assertEqual(1, extract_control("track1", "rtsp://95.34.123.4/h264", [{"track1",1}])),
    ?_assertEqual(1, extract_control("rtsp://95.34.123.4:554/h264/track1", "rtsp://95.34.123.4:554/h264", [{"track1",1}])),
    ?_assertEqual(1, extract_control("rtsp://95.34.123.4:554/h264/track1", "rtsp://95.34.123.4:554/h264/", [{"track1",1}])),
    ?_assertEqual(1, extract_control("rtsp://95.34.123.4/h264/track1", "rtsp://95.34.123.4:554/h264", [{"track1",1}])),
    ?_assertEqual(1, extract_control("rtsp://95.34.123.4/h264/track1", "rtsp://95.34.123.4:554/h264/", [{"track1",1}])),
    ?_assertEqual(1, extract_control("rtsp://95.34.123.4:554/h264/track1", "rtsp://95.34.123.4/h264", [{"track1",1}])),
    ?_assertEqual(1, extract_control("rtsp://95.34.123.4:554/h264/track1", "rtsp://95.34.123.4/h264/", [{"track1",1}]))
  ].








