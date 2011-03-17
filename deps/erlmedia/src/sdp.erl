%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
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
-module(sdp).
-author('Max Lapshin <max@maxidoors.ru>').

-export([sdp_codecs/0, sdp_to_codec/1, codec_to_sdp/1,
         decode/1,
         encode/2, encode/1
        ]).

-export([
         make_session/0,
         make_username/0
        ]).

-include("../include/sdp.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/video_frame.hrl").
-include("../include/media_info.hrl").
-include("../include/h264.hrl").
-include("log.hrl").
-define(DBG(X,A), ok).

%%----------------------------------------------------------------------
%% @spec (Data::binary()) -> [media_desc()]
%%
%% @doc Called by {@link rtp_server.} to decode SDP when it meets "Content-Type: application/sdp"
%% in incoming headers. Returns list of preconfigured, but unsynced rtsp streams
%% @end
%%----------------------------------------------------------------------

codec_to_sdp(Codec) -> 
  case lists:keyfind(Codec, 1, sdp_codecs()) of
    false -> undefined;
    {Codec, SDP} -> SDP
  end.



sdp_to_codec(SDP) -> 
  case lists:keyfind(string:to_lower(SDP), 2, [{K,string:to_lower(V)} || {K,V} <- sdp_codecs()]) of
    false -> undefined;
    {Codec, _SDP} -> Codec
  end.

sdp_codecs() ->
  [
  {h264, "H264"},
  {h263, "H263"},
  {aac, "mpeg4-generic"},
  {pcma, "PCMA"},
  {pcmu, "PCMU"},
  {g726_16, "G726-16"},
  {mpa, "MPA"},
  {mp4a, "MP4A-LATM"},
  {mp4v, "MP4V-ES"},
  {mp3, "mpa-robust"},
  {pcm, "L16"},
  {speex, "speex"}
  ].
  

decode(Announce) when is_binary(Announce) ->
  Lines = string:tokens(binary_to_list(Announce), "\r\n"),
  % Just to announce atoms;
  put(valid_atoms, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]),
  KeyValues = [{list_to_existing_atom([K]), parse_value(list_to_existing_atom([K]), Value)} || [K,$=|Value] <- Lines],
  % ?D(KeyValues),
  decode(KeyValues);

decode(Announce) ->
  parse_announce(Announce, #media_info{flow_type = stream}, undefined).



parse_value(v, Version) -> list_to_integer(Version);

parse_value(a, "fmtp:" ++ String) ->
  {match, [FMTP]} = re:run(String, "[\\d]+ +(.*)", [{capture, all_but_first, list}]),
  Opts = lists:map(fun(Opt) ->
    case re:run(Opt, " *([^=]+)=(.*)", [{capture, all_but_first, list}]) of
      {match, [Key, Value]} ->
        {string:to_lower(Key), Value};
      _ -> Opt
    end
  end, string:tokens(FMTP, ";")),
  {fmtp, Opts};

parse_value(a, String) ->
  case re:run(String, "([^:]+):(.+)", [{capture, all_but_first, list}]) of
    {match, [Key, Value]} -> {Key, Value};
    _ -> {String, true}
  end;

parse_value(c, String) ->
  {match, [NT, Addr]} = re:run(String, "IN +IP(\\d) +([^ ]+) *", [{capture, all_but_first, list}]),
  N = case NT of
    "4" -> inet4;
    "6" -> inet6
  end,
  {N, Addr};
  
parse_value(m, String) ->
  [TypeS, PortS, "RTP/AVP" | PayloadTypes] = string:tokens(String, " "), % TODO: add support of multiple payload
  Type = erlang:list_to_existing_atom(TypeS),
  [Type, PortS | PayloadTypes];

parse_value(_K, Value) ->
  Value.


add_stream(#media_info{audio = Audio} = Media, #stream_info{content = audio} = Stream) ->
  Media#media_info{audio = Audio ++ [Stream#stream_info{stream_id = stream_count(Media)+1}]};

add_stream(#media_info{video = Video} = Media, #stream_info{content = video} = Stream) ->
  Media#media_info{video = Video ++ [Stream#stream_info{stream_id = stream_count(Media)+1}]}.

stream_count(#media_info{audio = A, video = V}) -> length(A) + length(V).

parse_announce([], MediaInfo, undefined) ->
  MediaInfo;

parse_announce([], Streams, Stream) ->
  add_stream(Streams, Stream);

parse_announce([{c, Connect} | Announce], #media_info{options = Options} = MediaInfo, Stream) ->
  parse_announce(Announce, MediaInfo#media_info{options = [{connect,Connect}|Options]}, Stream);

parse_announce([{m, Info} | Announce], MediaInfo, #stream_info{} = Stream) ->
  parse_announce([{m, Info} | Announce], add_stream(MediaInfo, Stream), undefined);

parse_announce([{m, [Type |_ ]} | Announce], MediaInfo, undefined) when Type == video orelse Type == audio ->
  Params = case Type of
    video -> #video_params{};
    audio -> #audio_params{}
  end,
  parse_announce(Announce, MediaInfo, #stream_info{content = Type, params = Params});

parse_announce([{a, {"rtpmap", Value}} | Announce], MediaInfo, #stream_info{params = Params} = Stream) ->
  [_PayLoadNum, CodecInfo] = string:tokens(Value, " "),
  [CodecCode, ClockMap | EncodingParams] = string:tokens(CodecInfo, "/"),
  Codec = sdp_to_codec(CodecCode),
  Params1 = case {Codec, EncodingParams} of
    {pcma, ["1"]} -> #audio_params{sample_rate = list_to_integer(ClockMap), channels = 1};
    {pcmu, ["1"]} -> #audio_params{sample_rate = list_to_integer(ClockMap), channels = 1};
    _ -> Params
  end,
  parse_announce(Announce, MediaInfo, Stream#stream_info{codec = Codec, timescale = list_to_integer(ClockMap)/1000, params = Params1});


parse_announce([{a, {"control", Value}} | Announce], MediaInfo, #stream_info{options = Options} = Stream) ->
  parse_announce(Announce, MediaInfo, Stream#stream_info{options = [{control, Value}|Options]});

parse_announce([{a, {fmtp, Opts}} | Announce], MediaInfo, #stream_info{content = video, codec = Codec} = Stream) ->
  Stream1 = case proplists:get_value("sprop-parameter-sets", Opts) of
    undefined -> Stream;
    Sprop when is_list(Sprop) andalso Codec == h264 ->
      ProfileLevelId = proplists:get_value("profile-level-id", Opts),
      Profile = erlang:list_to_integer(string:sub_string(ProfileLevelId, 1, 2), 16),
      Level = erlang:list_to_integer(string:sub_string(ProfileLevelId, 5, 6), 16),
      
      NALS = [base64:decode(S) || S <- string:tokens(Sprop, ",")],
      [SPS|_] = [NAL || NAL <- NALS, h264:type(NAL) == sps],
      #h264_sps{width = Width, height = Height} = h264:parse_sps(SPS),
      
      H264 = #h264{profile = Profile, level = Level, sps = [NAL || NAL <- NALS, h264:type(NAL) == sps], pps = [NAL || NAL <- NALS, h264:type(NAL) == pps]},
      Stream#stream_info{config = h264:decoder_config(H264), params = #video_params{width = Width, height = Height}};
    _ ->
      Stream
  end,
  parse_announce(Announce, MediaInfo, Stream1);

parse_announce([{a, {fmtp, Opts}} | Announce], MediaInfo, #stream_info{content = audio} = Stream) ->
  Config = case proplists:get_value("config", Opts) of
    undefined -> undefined;
    HexConfig -> ssl_debug:unhex(HexConfig)
  end,
  parse_announce(Announce, MediaInfo, Stream#stream_info{config = Config});

parse_announce([_ | Announce], Streams, Stream) ->
  parse_announce(Announce, Streams, Stream).


encode(Info) ->
  sdp_encoder:encode(Info).

encode(Session, MediaSeq) ->
  sdp_encoder:encode(Session, MediaSeq).

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

-spec make_session() -> string().
make_session() ->
  random:seed(now()),
  M = 100000000000000,
  integer_to_list(M+random:uniform(M)*5).

-spec make_username() -> string().
make_username() ->
  "-".
