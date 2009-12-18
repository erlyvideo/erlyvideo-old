-module(sdp).
-author(max@maxidoors.ru).

-export([decode/1]).
-include("../../../include/ems.hrl").
-include("../../../include/rtsp.hrl").

decode(Announce) ->
  parse_announce(Announce, [], undefined).

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
  {match, [_, _Codec, ClockMap]} = re:run(Info, Re, [{capture, all, list}]),
  % case proplists:get_value(type, Stream) of
  %   video ->  "H264" = Codec;
  %   audio -> "mpeg4-generic" = Codec
  % end,
  parse_announce(Announce, Streams, Stream#rtsp_stream{clock_map = list_to_integer(ClockMap)/1000});

% parse_announce([{a, <<"cliprect:", Info/binary>>} | Announce], Streams, Stream) when is_list(Stream) ->
%   [_,_,Width, Height] = string:tokens(binary_to_list(Info), ","),
%   parse_announce(Announce, Streams, [{height, list_to_integer(Height)} | [{width, list_to_integer(Width)} | Stream]]);

parse_announce([{<<"a">>, <<"control:trackid=", Track/binary>>} | Announce], Streams, #rtsp_stream{} = Stream) ->
  parse_announce(Announce, Streams, Stream#rtsp_stream{id = list_to_integer(binary_to_list(Track))});

parse_announce([{<<"a">>, <<"fmtp:", Info/binary>>} | Announce], Streams, #rtsp_stream{} = Stream) ->
  {ok, Re} = re:compile("([^=]+)=(.*)"),
  [_, OptList] = string:tokens(binary_to_list(Info), " "),
  Opts = lists:map(fun(Opt) ->
    {match, [_, Key, Value]} = re:run(Opt, Re, [{capture, all, list}]),
    {Key, Value}
  end, string:tokens(OptList, ";")),
  
  parse_announce(Announce, Streams, parse_fmtp(Stream, Opts ));

parse_announce([{<<"a">>, _Info} | Announce], Streams, Stream) ->
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
  Config = list_to_binary(hmac256:unhex(proplists:get_value("config", Opts))),
  ?D({"audio config", aac:decode_config(Config)}),
  
  % {value, {_, _Mode}, Opts1} = lists:keytake('mode', 1, lists:keysort(1, Opts)),
  % {value, {_, Config}, Opts2} = lists:keytake('config', 1, Opts1),
  % lists:keytake('sizelength', 1, Opts2),
  % lists:keytake('indexlength', 1, Opts2),
  % lists:keytake('indexdeltalength', 1, Opts2),
  % 
  % parse_announce(Announce, Streams, [{config, Config} | Stream])
  Stream#rtsp_stream{config = Config}.
  
  
