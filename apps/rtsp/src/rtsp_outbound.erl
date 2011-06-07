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
  MediaInfo = ems_media:media_info(Media),
  Info1 = add_rtsp_options(MediaInfo, Socket1),
  SDP = sdp:encode(Info1),
  Socket2 = rtsp_socket:save_media_info(Socket1#rtsp_socket{media = Media, direction = out, rtp = rtp:init(remote, Info1)}, Info1),
  rtsp_socket:reply(Socket2, "200 OK", [{'Cseq', seq(Headers)}, {'Server', ?SERVER_NAME},
      {'Date', httpd_util:rfc1123_date()}, {'Expires', httpd_util:rfc1123_date()},
      {'Content-Base', io_lib:format("~s/", [URL])}], SDP).




add_rtsp_options(#media_info{options = Options, video = V, audio = A} = Info, #rtsp_socket{session = Session}) ->
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

decode_transport(Transport) -> decode_transport(Transport, []).

decode_transport([], Headers) -> Headers;
decode_transport([{interleaved,I}|Transport], Headers) -> decode_transport(Transport, [{interleaved,I}|Headers]);
decode_transport([{proto,Proto}|Transport], Headers) -> decode_transport(Transport, [{proto,Proto}|Headers]);
decode_transport([{client_port,{Port1,Port2}}|Transport], Headers) -> decode_transport(Transport, [{remote_rtp_port,Port1},{remote_rtcp_port,Port2}|Headers]);
decode_transport([_T|Transport], Headers) -> decode_transport(Transport, Headers).

encode_transport(Transport, Reply) ->
  case proplists:get_value(proto, Transport) of
    tcp ->
      [{proto,tcp},{unicast,true},{interleaved,proplists:get_value(interleaved,Transport)}];
    udp ->
      Client = {proplists:get_value(remote_rtp_port,Transport), proplists:get_value(remote_rtcp_port,Transport)},
      Server = {proplists:get_value(local_rtp_port,Reply), proplists:get_value(local_rtcp_port,Reply)},
      Source = io_lib:format("~p.~p.~p.~p", tuple_to_list(proplists:get_value(local_addr, Reply))),
      [{proto,udp},{unicast,true},{client_port, Client},{server_port, Server},{source, lists:flatten(Source)}]
  end.

handle_play_setup(#rtsp_socket{rtp = RTP, addr = Addr, socket = Sock} = Socket, URL, Headers, _Body) ->
  {match, [Control]} = re:run(URL, "/trackID=(\\d+)$", [{capture, all_but_first, list}]),
  StreamId = list_to_integer(Control),
  Transport = decode_transport(proplists:get_value('Transport', Headers)),
  {ok, RTP1, Reply} = rtp:setup_channel(RTP, StreamId, [{remote_addr, Addr},{tcp_socket,Sock}|Transport]),
  % ?D({transport, Transport, encode_transport(Transport, Reply)}),
  rtsp_socket:reply(Socket#rtsp_socket{rtp = RTP1}, "200 OK", [{'Cseq', seq(Headers)}, {'Transport', encode_transport(Transport, Reply)}]).



handle_play_request(#rtsp_socket{callback = Callback, rtp = RTP} = Socket, URL, Headers, Body) ->
  case Callback:play(URL, Headers, Body) of
    {ok, Media} ->
      rtp:send_rtcp(RTP, sender_report, []),
      timer:send_interval(1000, send_sr),
      rtsp_socket:reply(Socket#rtsp_socket{media = Media}, "200 OK", [{'Cseq', seq(Headers)}, {'Range', "npt=0-"}, {'RTP-Info', rtp:rtp_info(RTP)}]);
    {error, authentication} ->
      rtsp_socket:reply(Socket, "401 Unauthorized", [{"WWW-Authenticate", "Basic realm=\"Erlyvideo Streaming Server\""}])
  end.


encode_frame(#video_frame{content = Content} = Frame, #rtsp_socket{rtp = RTP} = Socket) when Content == audio orelse Content == video ->
  {ok, RTP1} = rtp:handle_frame(RTP, Frame),
  Socket#rtsp_socket{rtp = RTP1};

encode_frame(#video_frame{content = metadata}, #rtsp_socket{} = Socket) ->
  Socket.


seq(Headers) ->
  proplists:get_value('Cseq', Headers, 1).
