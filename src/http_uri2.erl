%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%

-module(http_uri2).

-export([parse/1, parse_path_query/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
parse(AbsURI) ->
    case parse_scheme(AbsURI) of
	{error, Reason} ->
	    {error, Reason};
	{Scheme, Rest} ->
	    case (catch parse_uri_rest(Scheme, Rest)) of
		{UserInfo, Host, Port, Path, Query} ->
		    {Scheme, UserInfo, Host, Port, Path, Query};
		_  ->
		    {error, {malformed_url, AbsURI}}    
	    end
    end.

%%%========================================================================
%%% Internal functions
%%%========================================================================
parse_scheme(AbsURI) ->
    case split_uri(AbsURI, ":", {error, no_scheme}, 1, 1) of
	{error, no_scheme} ->
	    {error, no_scheme};
	{StrScheme, Rest} ->
		  {list_to_atom(http_util:to_lower(StrScheme)), Rest}
    end.

parse_uri_rest(Scheme, "//" ++ URIPart) ->

    {Authority, PathQuery} = 
	case split_uri(URIPart, "/", URIPart, 1, 0) of
	    Split = {_, _} ->
		Split;
	    URIPart ->
		case split_uri(URIPart, "\\?", URIPart, 1, 0) of
		    Split = {_, _} ->
			Split;
		    URIPart ->
			{URIPart,""}
		end
	end,
    
    {UserInfo, HostPort} = split_uri(Authority, "@", {"", Authority}, 1, 1),
    {Host, Port} = parse_host_port(Scheme, HostPort),
    {Path, Query} = parse_path_query(PathQuery),
    {UserInfo, Host, Port, Path, Query}.


parse_path_query(PathQuery) ->
    {Path, Query} =  split_uri(PathQuery, "\\?", {PathQuery, ""}, 1, 0),
    {path(Path), Query}.
    

parse_host_port(Scheme,"[" ++ HostPort) -> %ipv6
    DefaultPort = default_port(Scheme),
    {Host, ColonPort} = split_uri(HostPort, "\\]", {HostPort, ""}, 1, 1),
    {_, Port} = split_uri(ColonPort, ":", {"", DefaultPort}, 0, 1),
    {Host, int_port(Port)};

parse_host_port(Scheme, HostPort) ->
    DefaultPort = default_port(Scheme),
    {Host, Port} = split_uri(HostPort, ":", {HostPort, DefaultPort}, 1, 1),
    {Host, int_port(Port)}.
    
split_uri(UriPart, SplitChar, NoMatchResult, SkipLeft, SkipRight) ->
    case inets_regexp:first_match(UriPart, SplitChar) of
	{match, Match, _} ->
	    {string:substr(UriPart, 1, Match - SkipLeft),
	     string:substr(UriPart, Match + SkipRight, length(UriPart))}; 
	nomatch ->
	    NoMatchResult
    end.

default_port(http) ->
    80;
default_port(https) ->
    443;
default_port(rtmp) ->
    1935;
default_port(rtsp) ->
    554.

int_port(Port) when is_integer(Port) ->
    Port;
int_port(Port) when is_list(Port) ->
    list_to_integer(Port).

path("") ->
    "/";
path(Path) ->
    Path.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").

parse_http_1_test() ->
  ?assertEqual({http,[],"ya.ru",80,"/",[]},  http_uri2:parse("http://ya.ru/")).

parse_http_2_test() ->
  ?assertEqual({http,[],"ya.ru",8081,"/",[]},  http_uri2:parse("http://ya.ru:8081/")).

parse_http_3_test() ->
  ?assertEqual({https,[],"ya.ru",443,"/",[]},  http_uri2:parse("https://ya.ru/")).

parse_http_4_test() ->
  ?assertEqual({https,[],"ya.ru",8081,"/",[]},  http_uri2:parse("https://ya.ru:8081/")).

parse_http_5_test() ->
  ?assertEqual({https,[],"ya.ru",8081,"/",[]},  http_uri2:parse("https://ya.ru:8081/")).

parse_http_6_test() ->
  ?assertEqual({https,"root:password","ya.ru",8081,"/",[]},  http_uri2:parse("https://root:password@ya.ru:8081/")).

parse_http_7_test() ->
  ?assertEqual({http,[],"ya.ru",80,"/",[{"q", "question"}]},  http_uri2:parse("http://ya.ru/?q=question")).

parse_http_8_test() ->
  ?assertEqual({http,[],"ya.ru",80,"/",[{"q", "of life"}]},  http_uri2:parse("http://ya.ru/?q=of%20life")).

parse_http_9_test() ->
  ?assertEqual({http,[],"ya.ru",80,"/",[{"q", "of life"}, "start", {"a", "15"}]},  http_uri2:parse("http://ya.ru/?q=of%20life&start&a=15")).

parse_rtmp_1_test() ->
  ?assertEqual({rtmp,[],"ya.ru",1935,"/",[]},  http_uri2:parse("rtmp://ya.ru/")).

parse_rtsp_1_test() ->
  ?assertEqual({rtsp,[],"ya.ru",554,"/test/access",[]},  http_uri2:parse("rtsp://ya.ru/test/access")).


















