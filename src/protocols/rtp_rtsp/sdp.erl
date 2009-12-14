-module(sdp).
-author(max@maxidoors.ru).

-export([decode/1]).


decode(Announce) ->
  parse_announce(Announce, [], undefined).

parse_announce([], Streams, undefined) ->
  Streams;

parse_announce([], Streams, Stream) ->
  [Stream | Streams];
  
parse_announce([{v, _} | Announce], Streams, Stream) ->
  parse_announce(Announce, Streams, Stream);

parse_announce([{o, _} | Announce], Streams, Stream) ->
  parse_announce(Announce, Streams, Stream);

parse_announce([{s, _} | Announce], Streams, Stream) ->
  parse_announce(Announce, Streams, Stream);

parse_announce([{c, _} | Announce], Streams, Stream) ->
  parse_announce(Announce, Streams, Stream);

parse_announce([{t, _} | Announce], Streams, Stream) ->
  parse_announce(Announce, Streams, Stream);

parse_announce([{a, _} | Announce], Streams, undefined) ->
  parse_announce(Announce, Streams, undefined);

parse_announce([{m, Info} | Announce], Streams, Stream) when is_list(Stream) ->
  parse_announce([{m, Info} | Announce], [Stream | Streams], undefined);

parse_announce([{m, Info} | Announce], Streams, undefined) ->
  [TypeS, PortS, "RTP/AVP", S] = string:tokens(binary_to_list(Info), " "),
  Type = binary_to_existing_atom(list_to_binary(TypeS), utf8),
  Port = list_to_integer(PortS),
  parse_announce(Announce, Streams, [{type, Type}, {port, Port}, {payload_type, list_to_integer(S)}]);

parse_announce([{b, Info} | Announce], Streams, Stream) when is_list(Stream) ->
  ["AS", S] = string:tokens(binary_to_list(Info), ":"),
  parse_announce(Announce, Streams, [{bitrate, list_to_integer(S)} | Stream]);

parse_announce([{a, <<"rtpmap:", Info/binary>>} | Announce], Streams, Stream) when is_list(Stream) ->
  {ok, Re} = re:compile("\\d+ ([^/]+)/(\\d+)"),
  {match, [_, Codec, ClockMap]} = re:run(Info, Re, [{capture, all, list}]),
  case proplists:get_value(type, Stream) of
    video ->  "H264" = Codec;
    audio -> "mpeg4-generic" = Codec
  end,
  parse_announce(Announce, Streams, [{clock_map, list_to_integer(ClockMap)/1000} | Stream]);

parse_announce([{a, <<"cliprect:", Info/binary>>} | Announce], Streams, Stream) when is_list(Stream) ->
  [_,_,Width, Height] = string:tokens(binary_to_list(Info), ","),
  parse_announce(Announce, Streams, [{height, list_to_integer(Height)} | [{width, list_to_integer(Width)} | Stream]]);

parse_announce([{a, <<"control:trackid=", Track/binary>>} | Announce], Streams, Stream) when is_list(Stream) ->
  TrackId = list_to_integer(binary_to_list(Track)),
  parse_announce(Announce, Streams, [{track_id, TrackId} | Stream]);

parse_announce([{a, <<"fmtp:", Info/binary>>} | Announce], Streams, Stream) when is_list(Stream) ->
  {ok, Re} = re:compile("([^=]+)=(.*)"),
  [_, OptList] = string:tokens(binary_to_list(Info), " "),
  Opts = lists:map(fun(Opt) ->
    {match, [_, Key, Value]} = re:run(Opt, Re, [{capture, all, list}]),
    {binary_to_existing_atom(list_to_binary(Key), utf8), Value}
  end, string:tokens(OptList, ";")),

  case proplists:get_value(type, Stream) of
  video ->

  {value, {_, "1"}, Opts1} = lists:keytake('packetization-mode', 1, lists:keysort(1, Opts)),
  {value, {_, ProfileLevelId}, Opts2} = lists:keytake('profile-level-id', 1, Opts1),
  ProfileId = erlang:list_to_integer(string:sub_string(ProfileLevelId, 1, 2), 16),
  % ProfileIop = erlang:list_to_integer(string:sub_string(ProfileLevelId, 3, 4), 16),
  % <<Constraint1:1, Constraint2:1, Constraint3:1, 0:5>> = <<ProfileIop>>,
  % ?D({Constraint1, Constraint2, Constraint3}),
  LevelIdc = erlang:list_to_integer(string:sub_string(ProfileLevelId, 5, 6), 16),
  Opts3 = lists:keymerge(1, Opts2, [{profile, ProfileId}, {level, LevelIdc}]),

  {value, {_, Sprop}, Opts4} = lists:keytake('sprop-parameter-sets', 1, Opts3),
  Props = lists:map(fun(S) -> base64:decode(S) end, string:tokens(Sprop, ",")),
  Opts5 = [{parameter_sets, Props} | Opts4],
  parse_announce(Announce, Streams, Stream ++ Opts5);
  
  audio ->
  {value, {_, _Mode}, Opts1} = lists:keytake('mode', 1, lists:keysort(1, Opts)),
  {value, {_, Config}, Opts2} = lists:keytake('config', 1, Opts1),
  lists:keytake('sizelength', 1, Opts2),
  lists:keytake('indexlength', 1, Opts2),
  lists:keytake('indexdeltalength', 1, Opts2),
  
  parse_announce(Announce, Streams, [{config, Config} | Stream])
  end;

parse_announce([{a, _Info} | Announce], Streams, Stream) when is_list(Stream) ->
  parse_announce(Announce, Streams, Stream).
