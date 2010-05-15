%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        SDP decoder module
%%% @end
%%% @reference  See <a href="http://erlyvideo.org/rtsp" target="_top">http://erlyvideo.org</a> for common information.
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(sdp).
-author('Max Lapshin <max@maxidoors.ru>').

-export([decode/1]).
-include("../include/rtsp.hrl").
-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------
%% @spec (Data::binary()) -> [rtsp_streams()]
%%
%% @doc Called by {@link rtsp_socket.} to decode SDP when it meets "Content-Type: application/sdp" 
%% in incoming headers. Returns list of preconfigured, but unsynced rtsp streams
%% @end
%%----------------------------------------------------------------------

decode(Announce) when is_binary(Announce) ->
  decode(decode_body(Announce));

decode(Announce) ->
  parse_announce(Announce, [], undefined).


decode_body(Body) ->
  {ok, Re} = re:compile("(\\w)=(.*)\\r\\n$"),
  Split = split_body(Body, []),
  % ?D({"Split", Split}),
  decode_body(Split, [], Re).

decode_body([], List, _Re) ->
  lists:reverse(List);

decode_body([Message | Body], List, Re) ->
  ?D({"SDP", Message}),
  {match, [_, Key, Value]} = re:run(Message, Re, [{capture, all, binary}]),
  decode_body(Body, [{Key, Value} | List], Re).

split_body(<<>>, List) ->
  lists:reverse(List);

split_body(Body, List) ->
  case erlang:decode_packet(line, Body, []) of
    {ok, Line, More} ->
      split_body(More, [Line | List]);
    {more, undefined} ->
      lists:reverse([<<Body/binary, "\r\n">> | List])
  end.


parse_announce([], Streams, undefined) ->
  Streams;

parse_announce([], Streams, Stream) ->
  [Stream | Streams];
  
parse_announce([{<<"v">>, _} | Announce], Streams, Stream) ->
  parse_announce(Announce, Streams, Stream);

parse_announce([{<<"o">>, _} | Announce], Streams, Stream) ->
  parse_announce(Announce, Streams, Stream);

parse_announce([{<<"s">>, _} | Announce], Streams, Stream) ->
  parse_announce(Announce, Streams, Stream);

parse_announce([{<<"e">>, _} | Announce], Streams, Stream) ->
  parse_announce(Announce, Streams, Stream);

parse_announce([{<<"b">>, _} | Announce], Streams, undefined) ->
  parse_announce(Announce, Streams, undefined);

parse_announce([{<<"c">>, _} | Announce], Streams, Stream) ->
  parse_announce(Announce, Streams, Stream);

parse_announce([{<<"t">>, _} | Announce], Streams, Stream) ->
  parse_announce(Announce, Streams, Stream);

parse_announce([{<<"a">>, _} | Announce], Streams, undefined) ->
  parse_announce(Announce, Streams, undefined);

parse_announce([{<<"m">>, Info} | Announce], Streams, #rtsp_stream{} = Stream) ->
  parse_announce([{<<"m">>, Info} | Announce], [Stream | Streams], undefined);

parse_announce([{<<"m">>, Info} | Announce], Streams, undefined) ->
  [TypeS, _PortS, "RTP/AVP", PayloadType] = string:tokens(binary_to_list(Info), " "),
  Type = binary_to_existing_atom(list_to_binary(TypeS), utf8),
  parse_announce(Announce, Streams, #rtsp_stream{type = Type, payload_type = list_to_integer(PayloadType)});

parse_announce([{<<"b">>, <<"AS:", Bitrate/binary>>} | Announce], Streams, #rtsp_stream{} = Stream) ->
  _Bitrate = list_to_integer(binary_to_list(Bitrate)),
  parse_announce(Announce, Streams, Stream);

parse_announce([{<<"a">>, <<"rtpmap:", Info/binary>>} | Announce], Streams, #rtsp_stream{} = Stream) ->
  {ok, Re} = re:compile("\\d+ ([^/]+)/([\\d]+)"),
  {match, [_, CodecCode, ClockMap]} = re:run(Info, Re, [{capture, all, list}]),
  Codec = case CodecCode of
    "H264" -> h264;
    "mpeg4-generic" -> aac;
    Other -> Other
  end,
  parse_announce(Announce, Streams, Stream#rtsp_stream{clock_map = list_to_integer(ClockMap)/1000, codec = Codec});

% parse_announce([{a, <<"cliprect:", Info/binary>>} | Announce], Streams, Stream) when is_list(Stream) ->
%   [_,_,Width, Height] = string:tokens(binary_to_list(Info), ","),
%   parse_announce(Announce, Streams, [{height, list_to_integer(Height)} | [{width, list_to_integer(Width)} | Stream]]);

parse_announce([{<<"a">>, <<"control:trackid=", Track/binary>>} | Announce], Streams, #rtsp_stream{} = Stream) ->
  parse_announce(Announce, Streams, Stream#rtsp_stream{id = list_to_integer(binary_to_list(Track))});

parse_announce([{<<"a">>, <<"fmtp:", Info/binary>>} | Announce], Streams, #rtsp_stream{} = Stream) ->
  {ok, Re} = re:compile("([^=]+)=(.*)"),
  [_ | OptList] = string:tokens(binary_to_list(Info), " "),
  Opts = lists:map(fun(Opt) ->
    {match, [_, Key, Value]} = re:run(Opt, Re, [{capture, all, list}]),
    {string:to_lower(Key), Value}
  end, string:tokens(string:join(OptList, ""), ";")),
  
  parse_announce(Announce, Streams, parse_fmtp(Stream, Opts ));

parse_announce([{<<"a">>, _Info} | Announce], Streams, Stream) ->
  parse_announce(Announce, Streams, Stream);

parse_announce([{<<"i">>, _Info} | Announce], Streams, Stream) ->
  parse_announce(Announce, Streams, Stream).


parse_fmtp(#rtsp_stream{type = video} = Stream, Opts) ->
  % {value, {_, ProfileLevelId}, Opts2} = lists:keytake('profile-level-id', 1, Opts1),
  % ProfileId = erlang:list_to_integer(string:sub_string(ProfileLevelId, 1, 2), 16),
  % ProfileIop = erlang:list_to_integer(string:sub_string(ProfileLevelId, 3, 4), 16),
  % <<Constraint1:1, Constraint2:1, Constraint3:1, 0:5>> = <<ProfileIop>>,
  % ?D({Constraint1, Constraint2, Constraint3}),
  % LevelIdc = erlang:list_to_integer(string:sub_string(ProfileLevelId, 5, 6), 16),
  % Opts3 = lists:keymerge(1, Opts2, [{profile, ProfileId}, {level, LevelIdc}]),

  Sprop = proplists:get_value("sprop-parameter-sets", Opts),
  [SPS, PPS] = lists:map(fun(S) -> base64:decode(S) end, string:tokens(Sprop, ",")),
  Stream#rtsp_stream{pps = PPS, sps = SPS};

parse_fmtp(#rtsp_stream{type = audio} = Stream, Opts) ->
  "13" = proplists:get_value("sizelength", Opts), % Length of size in bits in Access Unit header
  "3" = proplists:get_value("indexlength", Opts),
  "3" = proplists:get_value("indexdeltalength", Opts),
  Config = ssl_debug:unhex(proplists:get_value("config", Opts)),
  ?D({"audio config", aac:decode_config(Config)}),
  
  % {value, {_, _Mode}, Opts1} = lists:keytake('mode', 1, lists:keysort(1, Opts)),
  % {value, {_, Config}, Opts2} = lists:keytake('config', 1, Opts1),
  % lists:keytake('sizelength', 1, Opts2),
  % lists:keytake('indexlength', 1, Opts2),
  % lists:keytake('indexdeltalength', 1, Opts2),
  % 
  % parse_announce(Announce, Streams, [{config, Config} | Stream])
  Stream#rtsp_stream{config = Config}.
  
  
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
  
