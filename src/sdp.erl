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

-export([decode/1, encode/2, prep_media_config/2]).
-include("../include/sdp.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
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


parse_announce([], Streams, undefined, _Connect) ->
  lists:reverse(Streams);

parse_announce([], Streams, Stream, _Connect) ->
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
  [TypeS, PortS, "RTP/AVP" | PayloadTypes] = string:tokens(Info, " "), % TODO: add support of multiple payload
  Type = binary_to_existing_atom(list_to_binary(TypeS), utf8),
  parse_announce(Announce, Streams, #media_desc{type = Type,
                                                connect = Connect,
                                                port = list_to_integer(PortS),
                                                payloads = [#payload{num = list_to_integer(PT)} || PT <- PayloadTypes],
                                                track_control = "trackID="++integer_to_list(length(Streams)+1)}, Connect);

parse_announce([{b, _Bitrate} | Announce], Streams, #media_desc{} = Stream, Connect) ->
  parse_announce(Announce, Streams, Stream, Connect);

parse_announce([{a, Attribute} | Announce], Streams, #media_desc{} = Stream, Connect) ->
  case string:chr(Attribute, $:) of
    0 -> Key = Attribute, Value = undefined;
    Pos when Pos > 0 ->
      Key = string:substr(Attribute, 1, Pos - 1),
      Value = string:substr(Attribute, Pos + 1)
  end,

  Stream1 = case Key of
    "rtpmap" ->
      {ok, Re} = re:compile("(\\d+) ([^/]+)/([\\d]+)"),
      {match, [_, PayLoadNum, CodecCode, ClockMap1]} = re:run(Value, Re, [{capture, all, list}]),
      Pt0 = Stream#media_desc.payloads,
      Codec = case CodecCode of
        "H264" -> h264;
        "mpeg4-generic" -> aac;
        "MPA" -> mpa;
        "mpa-robust" -> mp3;
        "MP4A-LATM" -> mp4a;
        "MP4V-ES" -> mp4v;
        "PCMA" -> pcma;
        "PCMU" -> pcmu;
        "G726-16" -> g726_16;
        "L16" -> pcm;
        "speex" -> speex;
        Other -> Other
      end,
      ClockMap = case Codec of
        g726_16 -> 8000;
        _ -> list_to_integer(ClockMap1)
      end,
      Pt1 = #payload{num = list_to_integer(PayLoadNum), codec = Codec, clock_map = ClockMap/1000},
      NewPt = lists:keystore(Pt1#payload.num, #payload.num, Pt0, Pt1),
      Stream#media_desc{payloads = NewPt};
    "control" ->
      Stream#media_desc{track_control = Value};
    "fmtp" ->
      {ok, Re} = re:compile("([^=]+)=(.*)"),
      ?DBG("Value: ~p", [Value]),
      [_ | OptList] = string:tokens(Value, " "),
      Opts = lists:map(fun(Opt) ->
        ?DBG("Opt: ~p", [Opt]),
        case re:run(Opt, Re, [{capture, all, list}]) of
          {match, [_, Key1, Value1]} ->
            {string:to_lower(Key1), Value1};
          _ -> Opt
        end
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

  case proplists:get_value("sprop-parameter-sets", Opts) of
    Sprop when is_list(Sprop) ->
      case [base64:decode(S) || S <- string:tokens(Sprop, ",")] of
        [SPS, PPS] ->
          Stream#media_desc{pps = PPS, sps = SPS};
        _ ->
          Stream
      end;
    _ ->
      Stream
  end;

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
                            } = _D, _A) ->
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
                 _Other ->
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
  iolist_to_binary([SV, SO, SN, SC, TimeB, AttrL]).

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
                         connect = _Connect,
                         port = Port,
                         payloads = PayLoads,
                         track_control = TControl,
                         config = Config
                        }, _GConnect, _A) ->
  Tb = type2bin(Type),
  M = ["m=", Tb, $ , integer_to_list(Port), $ , "RTP/AVP", $ ,
       string:join([integer_to_list(PTnum) || #payload{num = PTnum} <- PayLoads], " "), ?LSEP],
  AC = case TControl of undefined -> []; _ -> ["a=", "control:", TControl, ?LSEP] end,
  %% TODO: support of several payload types
  AR = [begin
          Codecb = codec2bin(Codec),
          CMapb = integer_to_list(ClockMap),
          MSb = ms2bin(MS),
          if is_list(PTConfig) ->
              PTC = [["a=", "fmtp:", integer_to_list(PTnum), $ , C, ?LSEP] || C <- PTConfig];
             true ->
              PTC = []
          end,
          [["a=", "rtpmap:", integer_to_list(PTnum), $ , Codecb, $/, CMapb, MSb, ?LSEP], PTC]
        end || #payload{num = PTnum, codec = Codec, clock_map = ClockMap, ms = MS, config = PTConfig} <- PayLoads],
  ACfg = case Config of
           %% _ when (is_list(Config) or
           %%         is_binary(Config)) ->
           _ when ((is_list(Config) and (length(Config) > 0))
                   or (is_binary(Config) and (size(Config) > 0))) ->
             [["a=", "fmtp:", integer_to_list(PTnum), $ , Config, ?LSEP] || #payload{num = PTnum} <- PayLoads];
           _ ->
             []
         end,
  iolist_to_binary([M, AC, AR, ACfg]).

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
    mpa -> <<"MPA">>;
    mp4a -> <<"MP4A-LATM">>;
    mp4v -> <<"MP4V-ES">>;
    mp3 -> <<"mpa-robust">>;
    pcm -> <<"L16">>;
    speex -> <<"speex">>
  end.

ms2bin(MS) ->
  case MS of
    undefined -> <<>>;
    mono -> <<$/,$1>>;
    stereo -> <<$/,$2>>
  end.

prep_media_config({video,
                   #video_frame{content = video,
                                flavor = config,
                                codec = h264 = Codec,
                                body = Body}}, Opts) ->
  {_, [SPS, PPS]} = h264:unpack_config(Body),
  #media_desc{type = video,
              port = proplists:get_value(video_port, Opts, 0),
              payloads = [#payload{num = 96, codec = Codec, clock_map = 90000,
                                   config = [iolist_to_binary(["packetization-mode=1;"
                                                               "profile-level-id=64001e;"
                                                               "sprop-parameter-sets=",
                                                               base64:encode(SPS), $,, base64:encode(PPS)])]}],
              track_control = proplists:get_value(video, Opts, "1")
             };
prep_media_config({audio,
                   #video_frame{content = audio,
                                flavor = config,
                                codec = aac = Codec,
                                sound = {_Channs, _Size, Rate},
                                body = Body}}, Opts) ->
  <<ConfigVal:2/big-integer-unit:8>> = Body,
  #media_desc{type = audio,
              port = proplists:get_value(audio_port, Opts, 0),
              payloads = [#payload{num = 97, codec = Codec, clock_map = rate2num(Rate),
                                   config = [iolist_to_binary(["streamtype=5;"
                                                               "profile-level-id=15;"
                                                               "mode=AAC-hbr;"
                                                               "config=",
                                                               erlang:integer_to_list(ConfigVal, 16) ++ ";",
                                                               "SizeLength=13;"
                                                               "IndexLength=3;"
                                                               "IndexDeltaLength=3;"
                                                               "Profile=1;"])]}],
              track_control = proplists:get_value(audio, Opts, "2")
             };
prep_media_config({audio,
                   #video_frame{content = audio,
                                flavor = config,
                                codec = speex = Codec,
                                sound = {_Channs, _Size, Rate},
                                body = _Body}}, Opts) ->
  #media_desc{type = audio,
              port = proplists:get_value(audio_port, Opts, 0),
              payloads = [#payload{num = 111, codec = Codec,
                                   clock_map = rate2num(Rate), ms = mono,
                                   config = ["vbr=on"]}],
              track_control = undefined}.

rate2num(Rate) ->
  case Rate of
    rate11 -> 11025;
    rate22 -> 22050;
    rate44 -> 44100;
    R when is_integer(R) -> R
  end.

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

