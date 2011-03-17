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
-include("rtsp.hrl").

-export([encode_frame/2, handle_describe_request/4, handle_play_setup/4, handle_play_request/4]).

handle_describe_request(#rtsp_socket{callback = Callback} = Socket, URL, Headers, Body) ->
  case Callback:describe(URL, Headers, Body) of
    {error, authentication} ->
      rtsp_socket:reply(Socket, "401 Unauthorized", [{"WWW-Authenticate", "Basic realm=\"Erlyvideo Streaming Server\""}]);
    {ok, Media} ->
      handle_authorized_describe(Socket, URL, Headers, Media)
  end.
  
handle_authorized_describe(Socket, _URL, Headers, Media) ->  
  MediaInfo = #media_info{} = ems_media:media_info(Media),
  Info1 = add_rtsp_options(MediaInfo),
  SDP = sdp:encode(Info1),
  rtsp_socket:reply(rtsp_socket:save_media_info(Socket#rtsp_socket{media = Media, direction = out}, Info1), "200 OK",
        [{'Cseq', seq(Headers)}, {'Server', ?SERVER_NAME}, {'Cache-Control', "no-cache"}], SDP).




add_rtsp_options(#media_info{options = Options, video = V, audio = A} = Info) ->
  % ?DBG("Describe INFO (~p): ~p", [self(), MediaInfo]),

  SessionDesc = #sdp_session{version = 0,
    originator = #sdp_o{username = "-",
                        sessionid = "1275067839203788",
                        version = "1",
                        netaddrtype = inet4,
                        address = "0.0.0.0"},
    name = "Test",
    connect = {inet4, "0.0.0.0"},
    attrs = [
       {tool, "LIVE555 Streaming Media v2008.04.09"},
       recvonly,
       {type, "broadcast"},
       {control, "*"},
       {charset, "UTF-8"},
       {range, " npt=0-"}
      ]
  },
  
  AddControl = fun(Streams) ->
    lists:map(fun(#stream_info{options = Opts, stream_id = Id} = Stream) ->
      Control = "trackID="++integer_to_list(Id),
      Stream#stream_info{options = [{control,Control}|Opts]}
    end, Streams)
  end,
  Info#media_info{options = [{sdp_session, SessionDesc}|Options], audio = AddControl(A), video = AddControl(V)}.

  


handle_play_setup(#rtsp_socket{} = Socket, URL, Headers, _Body) ->
  {match, [Control]} = re:run(URL, "/([^/]+)$", [{capture, all_but_first, list}]),
  Transport = proplists:get_value('Transport', Headers),
  StreamNum = proplists:get_value(Control, Socket#rtsp_socket.control_map),
  StreamInfo = element(StreamNum, Socket#rtsp_socket.rtp_streams),
  Streams = setelement(StreamNum, Socket#rtsp_socket.rtp_streams, rtp_encoder:init(StreamInfo)),
  rtsp_socket:reply(Socket#rtsp_socket{rtp_streams = Streams}, "200 OK", [{'Cseq', seq(Headers)}, {'Session', 42}, {'Transport', Transport}]).

  
handle_play_request(#rtsp_socket{callback = Callback, rtp_streams = RtpStreams} = Socket, URL, Headers, Body) ->
  case Callback:play(URL, Headers, Body) of
    {ok, Media} ->
      RtpInfo = string:join([rtp_encoder:rtp_info(Encoder) || Encoder <- tuple_to_list(RtpStreams)], ","),
      rtsp_socket:reply(Socket#rtsp_socket{media = Media}, "200 OK",
            [{'Cseq', seq(Headers)}, {'Cache-control', "no-cache"}, {'RTP-Info', RtpInfo}]);
    {error, authentication} ->
      rtsp_socket:reply(Socket, "401 Unauthorized", [{"WWW-Authenticate", "Basic realm=\"Erlyvideo Streaming Server\""}])
  end.


encode_frame(#video_frame{content = audio} = Frame, #rtsp_socket{audio_rtp_stream = Num} = Socket) ->
  encode_frame(Frame, Socket, Num);

encode_frame(#video_frame{content = video} = Frame, #rtsp_socket{video_rtp_stream = Num} = Socket) ->
  encode_frame(Frame, Socket, Num);

encode_frame(#video_frame{content = metadata}, #rtsp_socket{} = Socket) ->
  Socket.

encode_frame(#video_frame{} = Frame, #rtsp_socket{rtp_streams = Streams, socket = Sock} = Socket, Num) ->
  RTP = element(Num, Streams),
  {ok, RTP1, Packets} = rtp_encoder:encode(Frame, RTP),
  RTPData = [packet_codec:encode({rtp, (Num-1)*2, Packet}) || Packet <- Packets],
  gen_tcp:send(Sock, RTPData),
  Socket#rtsp_socket{rtp_streams = setelement(Num, Streams, RTP1)}.



seq(Headers) ->
  proplists:get_value('Cseq', Headers, 1).
