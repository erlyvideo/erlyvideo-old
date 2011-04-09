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

-export([handle_call/3, sync_rtp/2, handle_announce_request/4, handle_receive_setup/4]).


dump_io(#rtsp_socket{dump_traffic = false}, _) -> ok;
dump_io(_, Call) -> io:format(">>>>>> RTSP OUT (~p:~p) >>>>>~n~s~n", [?MODULE, ?LINE, Call]).


handle_call({connect, URL, Options}, _From, RTSP) ->
  Consumer = proplists:get_value(consumer, Options),
  Ref = erlang:monitor(process, Consumer),
  Timeout = proplists:get_value(timeout, Options, ?DEFAULT_TIMEOUT),
  {rtsp, UserInfo, Host, Port, _Path, _Query} = http_uri2:parse(URL),
  Transport = proplists:get_value(transport, Options, tcp),

  Auth = case UserInfo of
    [] -> "";
    _ -> "Authorization: Basic "++binary_to_list(base64:encode(UserInfo))++"\r\n"
  end,
  RTSP1 = RTSP#rtsp_socket{url = URL, options = Options, media = Consumer, rtp_ref = Ref, auth = Auth, timeout = Timeout, transport = Transport},

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


handle_call({request, options}, From, #rtsp_socket{socket = Socket, url = URL, auth = Auth, seq = Seq, timeout = Timeout} = RTSP) ->
  Call = io_lib:format("OPTIONS ~s RTSP/1.0\r\nCSeq: ~p\r\n"++Auth++"\r\n", [URL, Seq+1]),
  gen_tcp:send(Socket, Call),
  dump_io(RTSP, Call),
  {noreply, RTSP#rtsp_socket{pending = From, state = options, seq = Seq+1}, Timeout};


handle_call({request, describe}, From, #rtsp_socket{socket = Socket, url = URL, auth = Auth, seq = Seq, timeout = Timeout} = RTSP) ->
  Call = io_lib:format("DESCRIBE ~s RTSP/1.0\r\nCSeq: ~p\r\nAccept: application/sdp\r\n"++Auth++"\r\n", [URL, Seq+1]),
  gen_tcp:send(Socket, Call),
  dump_io(RTSP, Call),
  {noreply, RTSP#rtsp_socket{pending = From, state = describe, seq = Seq+1}, Timeout};

handle_call({request, setup, Num}, From, 
  #rtsp_socket{socket = Socket, rtp = RTP, rtp_streams = Streams, url = URL, seq = Seq, auth = Auth, timeout = Timeout, transport = Transport} = RTSP) ->
  % ?D({"Setup", Num, Streams}),
  _Stream = #stream_info{options = Options} = element(Num, Streams),
  Control = proplists:get_value(control, Options),
  {ok, RTP1, Reply} = rtp:setup_channel(RTP, Num, [{proto,Transport},{tcp_socket,Socket}]),

  Sess = case RTSP#rtsp_socket.session of
    undefined -> "";
    Session -> "Session: "++Session++"\r\n"
  end,
  TransportHeader = case Transport of
    tcp -> io_lib:format("Transport: RTP/AVP/TCP;unicast;interleaved=~p-~p\r\n", [Num*2 - 2, Num*2-1]);
    udp ->
      Port1 = proplists:get_value(local_rtp_port, Reply),
      Port2 = proplists:get_value(local_rtcp_port, Reply),
      io_lib:format("Transport: RTP/AVP;unicast;client_port=~p-~p\r\n", [Port1, Port2])
  end,
  Call = io_lib:format("SETUP ~s RTSP/1.0\r\nCSeq: ~p\r\n"++Sess++TransportHeader++Auth++"\r\n",
        [append_trackid(URL, Control), Seq + 1]),
  gen_tcp:send(Socket, Call),
  dump_io(RTSP, Call),
  {noreply, RTSP#rtsp_socket{pending = From, state = {setup, Num}, rtp = RTP1, seq = Seq+1}, Timeout};

handle_call({request, play}, From, #rtsp_socket{socket = Socket, url = URL, seq = Seq, session = Session, auth = Auth, timeout = Timeout} = RTSP) ->
  Call = io_lib:format("PLAY ~s RTSP/1.0\r\nCSeq: ~p\r\nSession: ~s\r\n"++Auth++"\r\n", [URL, Seq + 1, Session]),
  gen_tcp:send(Socket, Call),
  dump_io(RTSP, Call),
  {noreply, RTSP#rtsp_socket{pending = From, state = play, seq = Seq + 1}, Timeout}.




sync_rtp(#rtsp_socket{rtp = RTP, control_map = ControlMap, url = URL} = Socket, RtpHeaders) ->
  case proplists:get_value('Rtp-Info', RtpHeaders) of
    undefined ->
      Socket;
    RtpInfo ->
      RTP1 = lists:foldl(fun(Headers, RTP_) ->
        case extract_control(proplists:get_value(url, Headers), URL, ControlMap) of
          undefined -> ?D({unsynced, Headers}), RTP_;
          StreamNum -> rtp:sync(RTP_, StreamNum, Headers)
        end
      end, RTP, RtpInfo),
      Socket#rtsp_socket{rtp = RTP1}
  end.



handle_announce_request(#rtsp_socket{callback = Callback} = Socket, URL, Headers, Body) ->
  Socket1 = #rtsp_socket{pending_reply = {ok, MediaInfo, _}} = rtsp_socket:handle_sdp(Socket, Headers, Body),
  case Callback:announce(URL, Headers, MediaInfo) of
    {ok, Media} ->
      ?D({"Announced to", Media}),
      erlang:monitor(process, Media),
      rtsp_socket:reply(Socket1#rtsp_socket{session = rtsp_socket:generate_session(), rtp = rtp:init(out,MediaInfo),
                        media = Media, direction = in}, "200 OK", [{'Cseq', seq(Headers)}]);
    {error, authentication} ->
      rtsp_socket:reply(Socket1, "401 Unauthorized", [{"WWW-Authenticate", "Basic realm=\"Erlyvideo Streaming Server\""}, {'Cseq', seq(Headers)}])
  end.

handle_receive_setup(#rtsp_socket{socket = Sock, rtp = RTP} = Socket, URL, Headers, _Body) ->
  {match, [Control]} = re:run(URL, "/([^/]+)$", [{capture, all_but_first, list}]),
  StreamId = proplists:get_value(Control, Socket#rtsp_socket.control_map),
  Transport = proplists:get_value('Transport', Headers),
  {ok, RTP1, _Reply} = rtp:setup_channel(RTP, StreamId, [{tcp_socket,Sock}|Transport]),
  
  rtsp_socket:reply(Socket#rtsp_socket{rtp = RTP1}, "200 OK", [{'Cseq', seq(Headers)}, {'Transport', proplists:get_value('Transport', Headers)}]).



seq(Headers) ->
  proplists:get_value('Cseq', Headers, 1).



append_trackid(_URL, ("rtsp://"++ _) = TrackID) ->
  TrackID;

append_trackid(URL, TrackID) ->
  case string:tokens(URL, "?") of
    [URL1, URL2] -> URL1 ++ "/" ++ TrackID ++ "?" ++ URL2;
    [URL] -> URL ++ "/" ++ TrackID
  end.





lookup_in_control_map(_ControlUrl, []) -> undefined;
lookup_in_control_map(ControlUrl, [{Track,Number}|ControlMap]) ->
  Postfix = string:substr(ControlUrl, length(ControlUrl) - length(Track) + 1),
  if
    Postfix == Track -> Number;
    true -> lookup_in_control_map(ControlUrl, ControlMap)
  end.



extract_control(ControlUrl, _URL, ControlMap) ->
  lookup_in_control_map(ControlUrl, ControlMap).

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
    ?_assertEqual(1, extract_control("rtsp://95.34.123.4:554/h264/track1", "rtsp://95.34.123.4/h264/", [{"track1",1}])),
    ?_assertEqual(1, extract_control("rtsp://192.168.0.74/h264/track1", "rtsp://user:password@192.168.23.43/554/h264", [{"track1",1}]))
  ].








