%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        Starts in-process http stream
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
-module(http_stream).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([get/2, get_with_body/2]).

open_socket(URL, Options) ->
  Timeout = proplists:get_value(timeout, Options, 3000),
  {_, _, Host, Port, _Path, _Query} = http_uri2:parse(URL),
  {_HostPort, Path} = http_uri2:extract_path_with_query(URL),
  
  ?D({http_connect, Host, Port, Path}),
  
  RequestPath = case proplists:get_value(send_hostpath, Options, false) of
    true -> URL;
    false -> Path
  end,

  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, http}, {active, false}], Timeout),
  ok = gen_tcp:send(Socket, "GET "++RequestPath++" HTTP/1.1\r\nHost: "++Host++":"++integer_to_list(Port)++"\r\n"),
  [gen_tcp:send(Socket, Key++": "++Value++"\r\n") || {Key,Value} <- proplists:get_value(headers, Options, [])],
  gen_tcp:send(Socket, "\r\n"),
  ok = inet:setopts(Socket, [{active, once}]),
  receive
    {http, Socket, {http_response, _Version, 200, _Reply}} ->
      {ok, Socket};
    {tcp_closed, Socket} ->
      {error, normal};
    {tcp_error, Socket, Reason} ->
      {error, Reason}
  after
    Timeout -> 
      gen_tcp:close(Socket),
      {error, timeout}
  end.


get_with_body(URL, Options) ->
  case get(URL, Options) of
    {ok, Headers, Socket} ->
      case proplists:get_value('Content-Length', Headers) of
        undefined -> 
          gen_tcp:close(Socket),
          {error, no_length};
        Length ->
          {ok, Body} = gen_tcp:recv(Socket, list_to_integer(Length)),
          gen_tcp:close(Socket),
          {ok, Headers, Body}
      end;
    Else ->
      Else
  end.  
        

get(URL, Options) ->
  Timeout = proplists:get_value(timeout, Options, 3000),
  case open_socket(URL, Options) of
    {ok, Socket} ->
      case wait_for_headers(Socket, [], Timeout) of
        {ok, Headers} ->
          ok = inet:setopts(Socket, [{active, false},{packet,raw}]),
          {ok, Headers, Socket};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.
  
wait_for_headers(Socket, Headers, Timeout) ->
  inet:setopts(Socket, [{active,once}]),
  receive
    {http, Socket, {http_header, _, Header, _, Value}} ->
      wait_for_headers(Socket, [{Header, Value}|Headers], Timeout);
    {http, Socket, http_eoh} ->
      {ok, lists:reverse(Headers)};
    {tcp_closed, Socket} ->
      {error, normal};
    {tcp_error, Socket, Reason} ->
      {error, Reason}
  after
    Timeout -> 
      gen_tcp:close(Socket),
      {error, Timeout}
  end.
  