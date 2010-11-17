%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2010 Max Lapshin
%%% @doc        Example of gen_server
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(srt_parser).
-author('Max Lapshin <max@maxidoors.ru>').

-include("../include/srt.hrl").

%% External API
-export([parse/1]).

-define(NewLine, "\r\n|\r|\n").
-define(EmptyLine, "\r\n\s*\r+\n+|\r\s*\r+|\n\s*\n+").


%%----------------------------------------------------------------------
%% @spec (SRT::binary()) -> {ok, [#srt_subtitle{}], More::binary}     |
%%                          {error, Reason}
%%
%% @doc Parse binary list of SRT
%% @end
%%----------------------------------------------------------------------

parse(SRT) when is_binary(SRT) ->
  try process_srts(re:split(SRT, ?EmptyLine), [])
  catch
    {parse_error, Srt} -> {error, {parse_error, Srt}}
  end.

process_srts(Items, Result) -> 
  case Items of
    [More|[]] -> {ok, lists:reverse(Result), More};
	[Item|Rest] -> process_srts(Rest, [srt_to_record(Item)|Result])
  end.

srt_to_record(Srt) -> 
  try
    {IdBin, TimeBin, Text} = get_fields(Srt),
    {FromBin, ToBin} = get_times(TimeBin),
    #srt_subtitle{
      id = binary_to_integer(IdBin), 
      from = parse_time(FromBin), 
      to = parse_time(ToBin), 
      text = Text
    }
  catch
    _:_ -> throw({parse_error, Srt})
  end.

get_fields(Srt) ->
  [IdBin|[TimeBin|Text]] = re:split(Srt, ?NewLine, [{parts,3}]),
  {IdBin, TimeBin, list_to_binary(Text)}.

get_times(TimeBin) ->
  [FromBin|[ToBin|_]] = re:split(TimeBin, " --> "),
  {FromBin, ToBin}.

parse_time(Time) ->
  [HBin|[MBin|[SBin|[MSBin|_]]]] = re:split(Time, ":|,"),
  H = binary_to_integer(HBin),
  M = binary_to_integer(MBin),
  S = binary_to_integer(SBin),
  MS = binary_to_integer(MSBin),
  H * 3600000 + M * 60000 + S * 1000 + MS.

binary_to_integer(Bin) ->
  list_to_integer(binary_to_list(Bin)).



-include_lib("eunit/include/eunit.hrl").

good_srt_1() ->
  <<"2
00:00:05,600 --> 00:00:10,200\r
-==http://www.ragbear.com==-\r
Present...\r
\r
3\r
00:00:10,200 --> 00:00:16,200\r
<i>Gossip Girl\r
Season 1 Episode 01</i>

4
00:00:22,100 --> 00:00:23,800
<i>HEY, UPPER EAST SIDERS,</i>">>.

good_srt_1_test() ->
  ?assertEqual({ok, [#srt_subtitle{id = 2, from = 5600, to = 10200, 
	    text = <<"-==http://www.ragbear.com==-\r\nPresent...">>},
      #srt_subtitle{id = 3, from = 10200, to = 16200, 
	    text = <<"<i>Gossip Girl\r\nSeason 1 Episode 01</i>">>}],
      <<"4\n00:00:22,100 --> 00:00:23,800\n<i>HEY, UPPER EAST SIDERS,</i>">>}, 
	parse(good_srt_1())).

good_srt_2() ->
  list_to_binary([good_srt_1(), <<"\n\n">>]).

good_srt_2_test() ->
  ?assertEqual({ok, [#srt_subtitle{id = 2, from = 5600, to = 10200, 
	    text = <<"-==http://www.ragbear.com==-\r\nPresent...">>},
      #srt_subtitle{id = 3, from = 10200, to = 16200, 
	    text = <<"<i>Gossip Girl\r\nSeason 1 Episode 01</i>">>},
      #srt_subtitle{id = 4, from = 22100, to = 23800, 
	    text = <<"<i>HEY, UPPER EAST SIDERS,</i>">>}],
      <<"">>}, 
	parse(good_srt_2())).

good_srt_3() -> 
  <<"1
00:00:20,000 --> 00:00:40,000


2
00:00:52,902 --> 00:00:57,635
<i>Before time began, there was the Cube.</i>">>.

good_srt_3_test() ->
  ?assertEqual({ok, [#srt_subtitle{id = 1, from = 20000, to = 40000, 
	    text = <<"">>}],
      <<"2\n00:00:52,902 --> 00:00:57,635\n<i>Before time began, there was the Cube.</i>">>}, 
	parse(good_srt_3())).

good_srt_4() -> 
  <<"2
00:00:05,600 --> 00:00:10,200
-==http://www.ragbear.com==-
Present...

3
00:00:10,200 --> 00:00:16,200

">>.

good_srt_4_test() ->
  ?assertEqual({ok, [#srt_subtitle{id = 2, from = 5600, to = 10200, 
	    text = <<"-==http://www.ragbear.com==-\nPresent...">>},
      #srt_subtitle{id = 3, from = 10200, to = 16200, 
	    text = <<"">>}],
      <<"">>}, 
	parse(good_srt_4())).


%invalid id
bad_srt_1() -> 
  <<"2
00:00:05,600 --> 00:00:10,200
-==http://www.ragbear.com==-
Present...

XX
00:00:10,200 --> 00:00:16,200
<i>Gossip Girl
Season 1 Episode 01</i>

">>.

bad_srt_1_test() ->
  ?assertEqual({error, {parse_error, 
    <<"XX\n00:00:10,200 --> 00:00:16,200\n<i>Gossip Girl\nSeason 1 Episode 01</i>">>}}, 
	parse(bad_srt_1())).

%invalid time
bad_srt_2() -> 
  <<"2
00:00:05,600 --> 00:00:10,200
-==http://www.ragbear.com==-
Present...

3
XX:XX:XX,XXX --> XX:XX:XX,XXX
<i>Gossip Girl
Season 1 Episode 01</i>

">>.

bad_srt_2_test() ->
  ?assertEqual({error, {parse_error, 
    <<"3\nXX:XX:XX,XXX --> XX:XX:XX,XXX\n<i>Gossip Girl\nSeason 1 Episode 01</i>">>}}, 
	parse(bad_srt_2())).

%invalid separator between "from" and "to" time
bad_srt_3() -> 
  <<"2
00:00:05,600 --> 00:00:10,200
-==http://www.ragbear.com==-
Present...

3
00:00:10,200 -- 00:00:16,200
<i>Gossip Girl
Season 1 Episode 01</i>

">>.

bad_srt_3_test() ->
  ?assertEqual({error, {parse_error, 
    <<"3\n00:00:10,200 -- 00:00:16,200\n<i>Gossip Girl\nSeason 1 Episode 01</i>">>}}, 
	parse(bad_srt_3())).

