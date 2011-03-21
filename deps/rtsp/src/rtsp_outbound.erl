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
-module(rtsp_outbound).
-author('Max Lapshin <max@maxidoors.ru>').

-include("log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("erlmedia/include/sdp.hrl").
-include_lib("rtp/include/rtp.hrl").
-include("rtsp.hrl").

-export([encode_frame/2, handle_describe_request/4, handle_play_setup/4, handle_play_request/4]).



handle_describe_request(#rtsp_socket{callback = Callback} = Socket, URL, Headers, Body) ->
  case Callback:describe(URL, Headers, Body) of
    {error, authentication} ->
      rtsp_socket:reply(Socket, "401 Unauthorized", [{"WWW-Authenticate", "Basic realm=\"Erlyvideo Streaming Server\""}]);
    {ok, Media} ->
      handle_authorized_describe(Socket, URL, Headers, Media)
  end.
  
handle_authorized_describe(#rtsp_socket{} = Socket, URL, Headers, Media) ->
  Socket1 = Socket#rtsp_socket{session = rtsp_socket:generate_session()},
  MediaInfo = #media_info{audio = A, video = V} = ems_media:media_info(Media),
  Info1 = add_rtsp_options(MediaInfo, Socket1),
  SDP = sdp:encode(Info1),
  RtpUdp = list_to_tuple([undefined || _ <- lists:seq(1, length(A)+length(V))]),
  Socket2 = rtsp_socket:save_media_info(Socket1#rtsp_socket{media = Media, direction = out, rtp_udp = RtpUdp}, Info1),
  rtsp_socket:reply(Socket2, "200 OK", [{'Cseq', seq(Headers)}, {'Server', ?SERVER_NAME}, 
      {'Date', httpd_util:rfc1123_date()}, {'Expires', httpd_util:rfc1123_date()},
      {'Content-Base', io_lib:format("~s/", [URL])}], SDP).




add_rtsp_options(#media_info{options = Options, video = V, audio = A} = Info, #rtsp_socket{transport = Transport, session = Session} = Socket) ->
  % ?DBG("Describe INFO (~p): ~p", [self(), MediaInfo]),

  SessionDesc = #sdp_session{version = 0,
    originator = #sdp_o{username = "-",
                        sessionid = Session,
                        version = Session,
                        netaddrtype = inet4,
                        address = "127.0.0.1"},
    name = "Test",
    connect = {inet4, "0.0.0.0"},
    attrs = [
       {tool, "LIVE555 Streaming Media v2008.04.09"},
       % recvonly,
       % {type, "broadcast"},
       {control, "*"},
       % {charset, "UTF-8"},
       {range, "npt=0-"}
      ]
  },
  
  AddControl = fun(Streams, Timescale) ->
    lists:map(fun(#stream_info{options = Opts, stream_id = Id} = Stream) ->
      Control = "trackID="++integer_to_list(Id),
      Stream#stream_info{options = [{control,Control}|Opts], timescale = Timescale}
    end, Streams)
  end,
  Info#media_info{options = [{sdp_session, SessionDesc}|Options], audio = AddControl(A, 44.1), video = AddControl(V, 90)}.

  


handle_play_setup(#rtsp_socket{} = Socket, URL, Headers, _Body) ->
  {match, [Control]} = re:run(URL, "/([^/]+)$", [{capture, all_but_first, list}]),
  Transport = proplists:get_value('Transport', Headers),
  StreamNum = proplists:get_value(Control, Socket#rtsp_socket.control_map),
  StreamInfo = element(StreamNum, Socket#rtsp_socket.rtp_streams),
  Streams = setelement(StreamNum, Socket#rtsp_socket.rtp_streams, rtp_encoder:init(StreamInfo)),
  {Socket1, Transport1} = case proplists:get_value(proto, Transport) of
    udp -> setup_udp_transport(Socket, StreamInfo, Transport);
    tcp -> {Socket#rtsp_socket{transport = interleaved}, Transport}
  end,
  rtsp_socket:reply(Socket1#rtsp_socket{rtp_streams = Streams}, "200 OK", [{'Cseq', seq(Headers)}, {'Transport', Transport1}]).

setup_udp_transport(#rtsp_socket{rtp_udp = RtpUdp} = Socket, #stream_info{content = Content, stream_id = Id} = StreamInfo, Transport) ->
  RTP = #rtp_udp{server_rtp_port = SPort1, server_rtcp_port = SPort2, source = Source} = rtp:open_ports(Content),
  {CPort1, CPort2} = proplists:get_value(client_port, Transport),
  RTP1 = #rtp_udp{client_rtp_port = CPort1, client_rtcp_port = CPort2},
  
  Transport1 = [{proto,udp},{unicast,true},{client_port,{CPort1, CPort2}},{server_port,{SPort1, SPort2}},{source, Source}],
  {Socket#rtsp_socket{rtp_udp = setelement(Id, RtpUdp, RTP1), transport = udp}, Transport1}.

  
handle_play_request(#rtsp_socket{callback = Callback, rtp_streams = RtpStreams} = Socket, URL, Headers, Body) ->
  case Callback:play(URL, Headers, Body) of
    {ok, Media} ->
      RtpInfo = string:join([rtp_encoder:rtp_info(Encoder) || Encoder <- tuple_to_list(RtpStreams)], ","),
      rtsp_socket:reply(Socket#rtsp_socket{media = Media}, "200 OK",
            [{'Cseq', seq(Headers)}, {'Cache-control', "no-cache"}, {'Range', "npt=0-"}, {'RTP-Info', RtpInfo}]);
    {error, authentication} ->
      rtsp_socket:reply(Socket, "401 Unauthorized", [{"WWW-Authenticate", "Basic realm=\"Erlyvideo Streaming Server\""}])
  end.


encode_frame(#video_frame{content = audio} = Frame, #rtsp_socket{audio_rtp_stream = Num} = Socket) ->
  encode_frame(Frame, Socket, Num);

encode_frame(#video_frame{content = video} = Frame, #rtsp_socket{video_rtp_stream = Num} = Socket) ->
  encode_frame(Frame, Socket, Num);

encode_frame(#video_frame{content = metadata}, #rtsp_socket{} = Socket) ->
  Socket.

% d(<<B:10/binary, _/binary>>) -> B;
% d(B) ->B.
% 

encode_frame(#video_frame{} = Frame, #rtsp_socket{rtp_streams = Streams, socket = Sock} = Socket, Num) ->
  % ?D({Frame#video_frame.codec,Frame#video_frame.flavor,Frame#video_frame.dts, d(Frame#video_frame.body)}),
  RTP = element(Num, Streams),
  {ok, RTP1, Packets} = rtp_encoder:encode(Frame, RTP),
  RTPData = [packet_codec:encode({rtp, (Num-1)*2, Packet}) || Packet <- Packets],
  gen_tcp:send(Sock, RTPData),
  Socket#rtsp_socket{rtp_streams = setelement(Num, Streams, RTP1)}.



seq(Headers) ->
  proplists:get_value('Cseq', Headers, 1).
