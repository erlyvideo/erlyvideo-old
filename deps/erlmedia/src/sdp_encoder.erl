%%% @author     Maxim Treskin <zerthud@gmail.com> [http://erlyvideo.org]
%%% @copyright  2010,2011 Max Lapshin
%%% @doc        SDP decoder module
%%% @end
%%% @reference  See <a href="http://erlyvideo.org/rtp" target="_top">http://erlyvideo.org</a> for common information.
%%% @end
%%%
%%% This file is part of erlmedia
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
-module(sdp_encoder).
-author('Maxim Treskin <zerthud@gmail.com>').

-export([encode/1, payload_type/1]).
%, prep_media_config/2]).

-include("../include/sdp.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/video_frame.hrl").
-include("../include/media_info.hrl").
-include("../include/h264.hrl").
-include("log.hrl").
-define(DBG(X,A), ok).


-define(LSEP, <<$\r,$\n>>).

payload_type(h264) -> 96;
payload_type(h263) -> 98;
payload_type(aac) -> 97;
payload_type(pcma) -> 8;
payload_type(pcmu) -> 0;
payload_type(g726_16) -> 99;
payload_type(speex) -> 100.


additional_codec_params(#stream_info{content = audio, params = #audio_params{channels = Channels}}) ->
  "/"++integer_to_list(Channels);

additional_codec_params(_) -> 
  "".
  
to_fmtp(h264, Config) -> h264:to_fmtp(Config);
to_fmtp(aac, Config) -> aac:to_fmtp(Config);
to_fmtp(_, _) -> undefined.

encode(#stream_info{content = Content, codec = Codec, stream_id = Id, options = Options, config = Config, timescale = Timescale, params = Params} = Stream) ->
  FMTP = case to_fmtp(Codec, Config) of
    undefined -> "";
    Else -> io_lib:format("a=fmtp:~p ~s\r\n", [payload_type(Codec), Else])
  end,
  Cliprect = case Params of
    #video_params{width = Width, height = Height} when Width >= 0 andalso Height >= 0 ->
      io_lib:format("a=cliprect:0,0,~p,~p\r\na=framesize:~p ~p-~p\r\na=x-dimensions:~p,~p\r\n", [Width, Height, payload_type(Codec),Width, Height,Width,Height]);
    _ -> ""
  end,
  Control = proplists:get_value(control, Options, io_lib:format("trackID=~p", [Id])),
  SDP = [
    io_lib:format("m=~s ~p RTP/AVP ~p", [Content, proplists:get_value(port, Options, 0), payload_type(Codec)]), ?LSEP,
    "a=control:", Control, ?LSEP,
    io_lib:format("a=rtpmap:~p ~s/~p", [payload_type(Codec), sdp:codec_to_sdp(Codec), round(Timescale*1000)]), additional_codec_params(Stream), ?LSEP,
    Cliprect,
    FMTP
  ],
  iolist_to_binary(SDP);

encode(#media_info{video = Video, audio = Audio, options = Options}) ->
  iolist_to_binary([
    encode_sdp_session(proplists:get_value(sdp_session, Options)),
    [encode(V) || V <- Video],
    [encode(A) || A <- Audio]
  ]).


net(inet6) -> "IN IP6";
net(_) -> "IN IP4".

encode_sdp_session(#sdp_session{
  name = SessName,
  connect = {ConnectNet, ConnectAddr},
  originator = #sdp_o{
    username = User,
    sessionid = SessionId,
    version = SessVersion,
    netaddrtype = NetType,
    address = OriginAddr
  }, attrs = Attrs} = Sess) ->
  [
    "v=", integer_to_list(Sess#sdp_session.version), ?LSEP,
    io_lib:format("o=~s ~s ~s ~s ~s", [User, SessionId, SessVersion, net(NetType), OriginAddr]), ?LSEP,
    io_lib:format("s=~s", [SessName]), ?LSEP,
    io_lib:format("c=~s ~s", [net(ConnectNet), ConnectAddr]), ?LSEP,
    "t=0 0", ?LSEP,
    encode_attrs(Attrs)
  ].


encode_attrs(Attrs) ->
  lists:map(fun
    ({K, V}) -> io_lib:format("a=~s:~s\r\n", [K,V]);
    (K) when is_atom(K) -> io_lib:format("a=~s\r\n", [K])
  end, Attrs).




% 
% %%
% encode(#session_desc{connect = GConnect} = Session,
%        MediaSeq) ->
%   S = encode_session(Session),
%   M = encode_media_seq(MediaSeq, GConnect),
%   <<S/binary,M/binary>>.
% 
% encode_session(S) ->
%   encode_session(S, <<>>).
% 
% encode_session(#session_desc{version = Ver,
%                              originator = #sdp_o{username = UN,
%                                                  sessionid = SI,
%                                                  version = OV,
%                                                  netaddrtype = NAT,
%                                                  address = AD},
%                              name = N,
%                              connect = Connect,
%                              time = Time,
%                              attrs = Attrs
%                             } = _D, _A) ->
%   SV = ["v=", Ver, ?LSEP],
%   SO = ["o=", UN, $ , SI, $ , OV, $ , at2bin(NAT), $ , AD, ?LSEP],
%   SN = ["s=", N, ?LSEP],
%   SC =
%     case Connect of
%       {Type, Addr} when (is_atom(Type)
%                          andalso (is_list(Addr) or is_binary(Addr))) ->
%         ["c=", at2bin(Type), $ , Addr, ?LSEP];
%       _ -> []
%     end,
%   AttrL = encode_attrs(Attrs),
%   TimeB =
%     case Time of
%       {TimeStart, TimeStop} when is_integer(TimeStart), is_integer(TimeStop) ->
%         ["t=", integer_to_list(TimeStart), $ , integer_to_list(TimeStop), ?LSEP];
%       _ -> []
%     end,
%   iolist_to_binary([SV, SO, SN, SC, TimeB, AttrL]).
% 
% %%  encode(D#session_desc{version = undefined}, <<A/binary,S/binary,?LSEP/binary>>);
% 
% 
% %% encode_session(#session_desc{name = N} = D, A) ->
% %%   S = <<"s="/binary, N/binary>>,
% %%   encode(D#session_desc{name = undefined}, <<A/binary,S/binary,?LSEP/binary>>);
% %% encode_session(#session_desc{connect = {Type, Addr}}, A) ->
% %%   AT = at2bin(Type),
% %%   S = <<"c="/binary,AT/binary,$ ,(list_to_binary(Addr))/binary>>,
% %%   <<A/binary,S/binary,?LSEP/binary>>.
% 
% encode_attrs(Attrs) ->
%   [begin
%      ResB =
%        case KV of
%          {K, V} when (is_atom(K)
%                       andalso (is_list(V) or is_binary(V))) ->
%            [atom_to_list(K), $:, V];
%          _ when is_atom(KV) ->
%            atom_to_list(KV);
%          _Other ->
%            ?DBG("Err: ~p", [KV]),
%            ""
%        end,
%      ["a=", ResB, ?LSEP]
%    end || KV <- Attrs].
% 
% encode_media_seq(MS, GConnect) ->
%   encode_media_seq(MS, GConnect, <<>>).
% 
% encode_media_seq([], _, A) ->
%   A;
% encode_media_seq([H|T], GConnect, A) ->
%   NA = <<A/binary,(encode_media(H, GConnect))/binary>>,
%   encode_media_seq(T, GConnect, NA).
% 
% encode_media(M, GConnect) ->
%   encode_media(M, GConnect, <<>>).
% 
