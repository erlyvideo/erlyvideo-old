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
-include_lib("erlmedia/include/h264.hrl").
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
  MediaDesc = case Type of
    _ when Type == video orelse Type == audio ->
      #media_desc{type = Type, connect = Connect, port = list_to_integer(PortS),
      payloads = [#payload{num = list_to_integer(PT)} || PT <- PayloadTypes]};
    data ->
      undefined
  end,
  parse_announce(Announce, Streams, MediaDesc, Connect);

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
      {match, [PayLoadNum, CodecCode, ClockMap1]} = re:run(Value, "(\\d+)[\\ ]+([^/\\s]+)/([\\d]+)", [{capture, all_but_first, list}]),
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
        [SPS, PPS] -> Stream#media_desc{pps = PPS, sps = SPS};
        [SPS, PPS|_] -> error_logger:error_msg("SDP with many PPS: ~p", [Sprop]), Stream#media_desc{pps = PPS, sps = SPS};
        _ -> Stream
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


encode(Session, MediaSeq) ->
  sdp_encoder:encode(Session, MediaSeq).

prep_media_config(Config, Options) ->
  sdp_encoder:prep_media_config(Config, Options).

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

