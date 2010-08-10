%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        SDP decoder module
%%% @end
%%% @reference  See <a href="http://erlyvideo.org/rtp" target="_top">http://erlyvideo.org</a> for common information.
%%% @end
%%%
%%% This file is part of erlang-rtp.
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
-module(sdp).
-author('Max Lapshin <max@maxidoors.ru>').

-export([decode/1, encode/2]).
-include("../include/sdp.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").

%%----------------------------------------------------------------------
%% @spec (Data::binary()) -> [media_desc()]
%%
%% @doc Called by {@link rtp_server.} to decode SDP when it meets "Content-Type: application/sdp"
%% in incoming headers. Returns list of preconfigured, but unsynced rtsp streams
%% @end
%%----------------------------------------------------------------------

decode(Announce) when is_binary(Announce) ->
  Lines = string:tokens(binary_to_list(Announce), "\r\n"),
  KeyValues = [{list_to_atom([K]), Value} || [K,$=|Value] <- Lines],
  decode(KeyValues);

decode(Announce) ->
  parse_announce(Announce, [], undefined, undefined).


parse_announce([], Streams, undefined, Connect) ->
  lists:reverse(Streams);

parse_announce([], Streams, Stream, Connect) ->
  lists:reverse([Stream | Streams]);

parse_announce([{v, _} | Announce], Streams, Stream, Connect) ->
  parse_announce(Announce, Streams, Stream, Connect);

parse_announce([{o, _} | Announce], Streams, Stream, Connect) ->
  parse_announce(Announce, Streams, Stream, Connect);

parse_announce([{s, _} | Announce], Streams, Stream, Connect) ->
  parse_announce(Announce, Streams, Stream, Connect);

parse_announce([{e, _} | Announce], Streams, Stream, Connect) ->
  parse_announce(Announce, Streams, Stream, Connect);

parse_announce([{b, _} | Announce], Streams, undefined, Connect) ->
  parse_announce(Announce, Streams, undefined, Connect);

parse_announce([{c, Connect} | Announce], Streams, undefined, undefined) ->
  parse_announce(Announce, Streams, undefined, parse_connect(Connect));

parse_announce([{t, _} | Announce], Streams, Stream, Connect) ->
  parse_announce(Announce, Streams, Stream, Connect);

parse_announce([{a, _} | Announce], Streams, undefined, Connect) ->
  parse_announce(Announce, Streams, undefined, Connect);

parse_announce([{m, Info} | Announce], Streams, #media_desc{} = Stream, Connect) ->
  parse_announce([{m, Info} | Announce], [Stream | Streams], undefined, Connect);

parse_announce([{m, Info} | Announce], Streams, undefined, Connect) ->
  [TypeS, PortS, "RTP/AVP", PayloadType] = string:tokens(Info, " "),
  Type = binary_to_existing_atom(list_to_binary(TypeS), utf8),
  parse_announce(Announce, Streams, #media_desc{type = Type,
                                                connect = Connect,
                                                port = PortS,
                                                payload = list_to_integer(PayloadType),
                                                track_control = "trackID="++integer_to_list(length(Streams)+1)}, Connect);

parse_announce([{b, _Bitrate} | Announce], Streams, #media_desc{} = Stream, Connect) ->
  parse_announce(Announce, Streams, Stream, Connect);

parse_announce([{a, Attribute} | Announce], Streams, #media_desc{} = Stream, Connect) ->
  Pos = string:chr(Attribute, $:),
  Key = string:substr(Attribute, 1, Pos - 1),
  Value = string:substr(Attribute, Pos + 1),

  Stream1 = case Key of
    "rtpmap" ->
      {ok, Re} = re:compile("\\d+ ([^/]+)/([\\d]+)"),
      {match, [_, CodecCode, ClockMap1]} = re:run(Value, Re, [{capture, all, list}]),
      Codec = case CodecCode of
        "H264" -> h264;
        "mpeg4-generic" -> aac;
        "PCMA" -> pcma;
        "PCMU" -> pcmu;
        "G726-16" -> g726_16;
        "L16" -> pcm;
        Other -> Other
      end,
      ClockMap = case Codec of
        g726_16 -> 8000;
        _ -> list_to_integer(ClockMap1)
      end,
      Stream#media_desc{clock_map = ClockMap/1000, codec = Codec};
    "control" ->
      Stream#media_desc{track_control = Value};
    "fmtp" ->
      {ok, Re} = re:compile("([^=]+)=(.*)"),
      [_ | OptList] = string:tokens(Value, " "),
      Opts = lists:map(fun(Opt) ->
        {match, [_, Key1, Value1]} = re:run(Opt, Re, [{capture, all, list}]),
        {string:to_lower(Key1), Value1}
      end, string:tokens(string:join(OptList, ""), ";")),
      parse_fmtp(Stream, Opts);
    _Else ->
      Stream
  end,
  parse_announce(Announce, Streams, Stream1, Connect);

parse_announce([{c, Connect} | Announce], Streams, #media_desc{} = Stream, Connect) ->
  parse_announce(Announce, Streams, Stream#media_desc{connect = Connect}, Connect);

parse_announce([{_Other, _Info} | Announce], Streams, Stream, Connect) ->
  parse_announce(Announce, Streams, Stream, Connect).


parse_fmtp(#media_desc{type = video} = Stream, Opts) ->
  % {value, {_, ProfileLevelId}, Opts2} = lists:keytake('profile-level-id', 1, Opts1),
  % ProfileId = erlang:list_to_integer(string:sub_string(ProfileLevelId, 1, 2), 16),
  % ProfileIop = erlang:list_to_integer(string:sub_string(ProfileLevelId, 3, 4), 16),
  % <<Constraint1:1, Constraint2:1, Constraint3:1, 0:5>> = <<ProfileIop>>,
  % ?D({Constraint1, Constraint2, Constraint3}),
  % LevelIdc = erlang:list_to_integer(string:sub_string(ProfileLevelId, 5, 6), 16),
  % Opts3 = lists:keymerge(1, Opts2, [{profile, ProfileId}, {level, LevelIdc}]),

  Sprop = proplists:get_value("sprop-parameter-sets", Opts),
  [SPS, PPS] = lists:map(fun(S) -> base64:decode(S) end, string:tokens(Sprop, ",")),
  Stream#media_desc{pps = PPS, sps = SPS};

parse_fmtp(#media_desc{type = audio} = Stream, Opts) ->
  ?D(Opts),
  % "13" = proplists:get_value("sizelength", Opts), % Length of size in bits in Access Unit header
  % "3" = proplists:get_value("indexlength", Opts),
  % "3" = proplists:get_value("indexdeltalength", Opts),
  Config = case proplists:get_value("config", Opts) of
    undefined -> undefined;
    HexConfig -> ssl_debug:unhex(HexConfig)
  end,

  % {value, {_, _Mode}, Opts1} = lists:keytake('mode', 1, lists:keysort(1, Opts)),
  % {value, {_, Config}, Opts2} = lists:keytake('config', 1, Opts1),
  % lists:keytake('sizelength', 1, Opts2),
  % lists:keytake('indexlength', 1, Opts2),
  % lists:keytake('indexdeltalength', 1, Opts2),
  %
  % parse_announce(Announce, Streams, [{config, Config} | Stream])
  Stream#media_desc{config = Config}.


parse_connect(Connect) ->
  {ok, Re} = re:compile("IN +IP(\\d) +([^ ]+) *"),
  {match, [_, NT, Addr]} = re:run(Connect, Re, [{capture, all, list}]),
  N = case NT of
        "4" -> inet4;
        "6" -> inet6
      end,
  {N, Addr}.

%%
-define(LSEP, <<$\n>>).
encode(#session_desc{connect = GConnect} = Session,
       MediaSeq) ->
  S = encode_session(Session),
  M = encode_media_seq(MediaSeq, GConnect),
  <<S/binary,M/binary>>.

encode_session(S) ->
  encode_session(S, <<>>).

encode_session(#session_desc{version = Ver,
                             originator = #sdp_o{username = UN,
                                                 sessionid = SI,
                                                 version = OV,
                                                 netaddrtype = NAT,
                                                 address = AD},
                             name = N,
                             connect = Connect,
                             time = Time,
                             attrs = Attrs
                            } = D, A) ->
  SV = ["v=", Ver, ?LSEP],
  SO = ["o=", UN, $ , SI, $ , OV, $ , at2bin(NAT), $ , AD, ?LSEP],
  SN = ["s=", N, ?LSEP],
  SC =
    case Connect of
      {Type, Addr} when (is_atom(Type)
                         andalso (is_list(Addr) or is_binary(Addr))) ->
        ["c=", at2bin(Type), $ , Addr, ?LSEP];
      _ -> []
    end,
  AttrL = [begin
             ResB =
               case KV of
                 {K, V} when (is_atom(K)
                              andalso (is_list(V) or is_binary(V))) ->
                   [atom_to_list(K), $:, V];
                 _ when is_atom(KV) ->
                   atom_to_list(KV);
                 Other ->
                   ?DBG("Err: ~p", [KV]),
                   ""
               end,
             ["a=", ResB, ?LSEP]
           end || KV <- Attrs],
  TimeB =
    case Time of
      {TimeStart, TimeStop} when is_integer(TimeStart), is_integer(TimeStop) ->
        ["t=", integer_to_list(TimeStart), $ , integer_to_list(TimeStop), ?LSEP];
      _ -> []
    end,
  iolist_to_binary([SV, SO, SN, TimeB, SC, AttrL]).

%%  encode(D#session_desc{version = undefined}, <<A/binary,S/binary,?LSEP/binary>>);


%% encode_session(#session_desc{name = N} = D, A) ->
%%   S = <<"s="/binary, N/binary>>,
%%   encode(D#session_desc{name = undefined}, <<A/binary,S/binary,?LSEP/binary>>);
%% encode_session(#session_desc{connect = {Type, Addr}}, A) ->
%%   AT = at2bin(Type),
%%   S = <<"c="/binary,AT/binary,$ ,(list_to_binary(Addr))/binary>>,
%%   <<A/binary,S/binary,?LSEP/binary>>.

encode_media_seq(MS, GConnect) ->
  encode_media_seq(MS, GConnect, <<>>).

encode_media_seq([], _, A) ->
  A;
encode_media_seq([H|T], GConnect, A) ->
  NA = <<A/binary,(encode_media(H, GConnect))/binary>>,
  encode_media_seq(T, GConnect, NA).

encode_media(M, GConnect) ->
  encode_media(M, GConnect, <<>>).

encode_media(#media_desc{type = Type,
                         connect = Connect,
                         port = Port,
                         payload = PayLoad,
                         clock_map = ClockMap,
                         codec = Codec,
                         track_control = TControl
                        }, GConnect, A) ->
  Tb = type2bin(Type),
  M = ["m=", Tb, $ , integer_to_list(Port), $ , "RTP/AVP", $ , integer_to_list(PayLoad), ?LSEP],
  AC = ["a=", "control:", TControl, ?LSEP],
  %% TODO: support of several payload types
  Codecb = codec2bin(Codec),
  CMapb = clockmap2bin(ClockMap),
  AR = ["a=", "rtpmap:", integer_to_list(PayLoad), $ , Codecb, $/, CMapb, ?LSEP],
  iolist_to_binary([M, AC, AR]).

type2bin(T) ->
  case T of
    audio -> <<"audio">>;
    video -> <<"video">>
  end.

at2bin(AT) ->
  case AT of
    inet4 -> <<"IN IP4">>;
    inet6 -> <<"IN IP6">>
  end.

codec2bin(C) ->
  case C of
    h264 -> <<"H264">>;
    aac -> <<"mpeg4-generic">>;
    pcma -> <<"PCMA">>;
    pcmu -> <<"PCMU">>;
    g726_16 -> <<"G726-16">>;
    mp4 -> <<"MP4A-LATM">>;
    pcm -> <<"L16">>
  end.

clockmap2bin(CM) ->
  list_to_binary(integer_to_list(CM*1000)).


% Example of SDP:
%
% v=0
% o=- 1266472632763124 1266472632763124 IN IP4 192.168.4.1
% s=Media Presentation
% e=NONE
% c=IN IP4 0.0.0.0
% b=AS:50000
% t=0 0
% a=control:*
% a=range:npt=0.000000-
% m=video 0 RTP/AVP 96
% b=AS:50000
% a=framerate:25.0
% a=control:trackID=1
% a=rtpmap:96 H264/90000
% a=fmtp:96 packetization-mode=1; profile-level-id=420029; sprop-parameter-sets=Z0IAKeNQFAe2AtwEBAaQeJEV,aM48gA==

