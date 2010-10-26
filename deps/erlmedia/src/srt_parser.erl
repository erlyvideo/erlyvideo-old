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


%%----------------------------------------------------------------------
%% @spec (SRT::binary()) -> {ok, [#srt_subtitle{}], More::binary}     |
%%                          {error, Reason}
%%
%% @doc Parse binary list of SRT
%% @end
%%----------------------------------------------------------------------

parse(SRT) when is_binary(SRT) ->
  parse_srt(SRT, [], id).

parse_srt(SRT, Entries, State) ->
  case erlang:decode_packet(line, SRT, []) of
    {ok, Line, Rest} ->
      do_something_with_line,
      parse_srt(Rest, Entries, State);
    {more, undefined} ->
      {ok, lists:reverse(Entries), SRT}
  end.  
      


-include_lib("eunit/include/eunit.hrl").

test_srt() ->
  <<"2
00:00:05,600 --> 00:00:10,200
-==http://www.ragbear.com==-
Present...

3
00:00:10,200 --> 00:00:16,200
<i>Gossip Girl
Season 1 Episode 01</i>

4
00:00:22,100 --> 00:00:23,800
<i>HEY, UPPER EAST SIDERS,</i>">>.


parse_good_test() ->
  ?assertEqual({ok, [#srt_subtitle{id = 2, from = 5600, to = 10200, text = <<"-==http://www.ragbear.com==-\nPresent...">>},
                     #srt_subtitle{id = 3, from = 10200, to = 16200, text = <<"<i>Gossip Girl\nSeason 1 Episode 01</i>">>}],
                     <<"4\n00:00:22,100 --> 00:00:23,800\n<i>HEY, UPPER EAST SIDERS,</i>">>}, parse(test_srt())).
