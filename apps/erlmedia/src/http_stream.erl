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

-export([get/2, get_with_body/2, head/2]).

open_socket(URL, Options) ->
  MaxRedirects = proplists:get_value(max_redirects, Options, 5),
  make_request_with_redirect(URL, Options, MaxRedirects).
    
make_request_with_redirect(_URL, _Options, 0) ->
  {error, too_many_redirects};

make_request_with_redirect(URL, Options, RedirectsLeft) ->
  case make_raw_request(URL, Options) of
    {http, Socket, Code} when Code >= 200 andalso Code < 300 ->
      {ok, [{redirected_url, URL}], Socket};
    {http, Socket, Code} when Code == 301 orelse Code == 302 ->
      Timeout = proplists:get_value(timeout, Options, 3000),
      case wait_for_headers(Socket, [], Timeout) of
        {ok, Headers} ->
%          gen_tcp:close(Socket),
          Location = proplists:get_value('Location', Headers),
          NewURL = calculate_redirected_url(URL, Location),
          make_request_with_redirect(NewURL, Options, RedirectsLeft - 1);
        {error, Reason} ->
          {error, Reason}
      end;
    {http, _Socket, Code} ->
      {error, {http_code, Code}};
    {tcp_closed, _Socket} ->
      make_request_with_redirect(URL, lists:keydelete(socket, 1, Options), RedirectsLeft);
      % {error, tcp_closed};
    {error, Reason} ->
      {error, Reason}
  end.

calculate_redirected_url(URL, Location) ->
  case re:run(Location, "^/(.*)", [{capture,all_but_first,list}]) of
    {match, _} ->
      {match, [Host]} = re:run(URL, "^([^:]+://[^/]+)", [{capture,all_but_first,binary}]),
      iolist_to_binary(io_lib:format("~s~s", [Host, Location]));
    _ ->
      iolist_to_binary(Location)
  end.  

make_raw_request(URL, Options) ->
  Timeout = proplists:get_value(timeout, Options, 3000),
  Method = case proplists:get_value(method, Options, get) of
    Meth when is_atom(Meth) -> string:to_upper(erlang:atom_to_list(Meth))
  end,

  {_, _, Host, Port, _Path, _Query} = http_uri2:parse(URL),
  {_HostPort, Path} = http_uri2:extract_path_with_query(URL),
  
  RequestPath = case proplists:get_value(send_hostpath, Options, false) of
    true -> URL;
    false -> Path
  end,

  Socket = case proplists:get_value(socket, Options) of
    undefined ->
      {ok, NewSock} = gen_tcp:connect(Host, Port, [binary, {packet, http_bin}, {active, false}, {recbuf, 65536}, inet], Timeout),
      NewSock;
    OldSocket ->
      inet:setopts(OldSocket, [{packet,http_bin},{active,false}]),
      OldSocket
  end,
  
  Body = proplists:get_value(body, Options, ""),
  
  Headers = proplists:get_value(headers, Options, []) ++ 
  case proplists:get_value(range, Options) of
    {Start,End} -> [{"Range", lists:flatten(io_lib:format("bytes=~p-~p", [Start,End-1]))}];
    undefined -> []
  end ++
  case proplists:get_value(body, Options) of
    undefined -> 
      case proplists:get_value(method, Options) of
        post -> [{"Content-Length", "0"}];
        _ -> []
      end;
    Body -> [{"Content-Length", integer_to_list(iolist_size(Body))}]
  end ++ case proplists:get_value(basic_auth, Options) of
    undefined -> [];
    {Login,Password} -> [{"Authorization", "Basic "++base64:encode_to_string(lists:flatten(io_lib:format("~s:~s", [Login, Password])))}]
  end,
  PortSpec = case Port of
    80 -> "";
    _ -> ":"++integer_to_list(Port)
  end,
  Request = iolist_to_binary([Method, " ", RequestPath, " HTTP/1.1\r\nHost: ", Host, PortSpec, "\r\n",
  [io_lib:format("~s: ~s\r\n", [Key, Value]) || {Key,Value} <- Headers], "\r\n", Body]),
  % ?D({http_connect, Request}),
  
  ok = gen_tcp:send(Socket, Request),
  ok = inet:setopts(Socket, [{active, once}]),
  receive
    {http, Socket, {http_response, _Version, Code, _Reply}} ->
      {http, Socket, Code};
    {tcp_closed, Socket} ->
      {error, tcp_closed};
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
          case proplists:get_value('Transfer-Encoding', Headers) of
            <<"chunked">> ->
              Body = get_chunked_body(Socket),
              {ok, Headers, Body};
            _ ->
              case proplists:get_value('Connection', Headers) of
                <<"close">> ->
                  Body = get_plain_body(Socket),
                  {ok, Headers, Body};
                _ ->
                  gen_tcp:close(Socket),
                  {error, no_length}
              end
          end;
        Length ->
          {ok, Body} = gen_tcp:recv(Socket, to_i(Length)),
 %         gen_tcp:close(Socket),
          {ok, Socket, Headers, Body}
      end;
    Else ->
      Else
  end.  

to_i(L) when is_list(L) -> list_to_integer(L);
to_i(B) when is_binary(B) -> to_i(binary_to_list(B));
to_i(I) when is_number(I) -> I.
  

get_plain_body(Socket) ->
  get_plain_body(Socket, []).

get_plain_body(Socket, Acc) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Bin} -> get_plain_body(Socket, [Bin|Acc]);
    {error, closed} -> iolist_to_binary(lists:reverse(Acc))
  end.

get_chunked_body(Socket) ->
  get_chunked_body(Socket, []).

get_chunked_body(Socket, Acc) ->
  inet:setopts(Socket, [{packet,line}]),
  {ok, ChunkHeader} = gen_tcp:recv(Socket, 0),
  inet:setopts(Socket, [{packet,raw}]),
  case re:run(ChunkHeader, "^([\\d\\w]+)", [{capture,all_but_first, list}]) of
    {match, [C]} ->
      get_next_chunk(Socket, Acc, list_to_integer(C, 16));
    nomatch ->
      get_next_chunk(Socket, Acc, 0)  
  end.

get_next_chunk(_Socket, Acc, 0) ->
  iolist_to_binary(lists:reverse(Acc));

get_next_chunk(Socket, Acc, Length) ->
  {ok, Body} = gen_tcp:recv(Socket, Length),
  get_chunked_body(Socket, [Body|Acc]).

head(URL, Options) ->
  {ok, Headers, _Socket} = get(URL, [{method,head}|Options]),
%  gen_tcp:close(_Socket),
  {ok, Headers}.

get(URL, Options) ->
  Timeout = proplists:get_value(timeout, Options, 3000),
  case open_socket(URL, Options) of
    {ok, Headers1, Socket} ->
      case wait_for_headers(Socket, [], Timeout) of
        {ok, Headers} ->
          ok = inet:setopts(Socket, [{active, false},{packet,raw},{keepalive,true}]),
          {ok, Headers ++ Headers1, Socket};
        {error, Reason} -> {error, Reason}
      end;
    {error, Reason} -> {error, Reason}
  end.
  
wait_for_headers(Socket, Headers, Timeout) ->
  ok=inet:setopts(Socket, [{active,once}]),
  receive
    {http, Socket, {http_header, _, Header, _, Value}} ->
      wait_for_headers(Socket, [{Header, Value}|Headers], Timeout);
    {http, Socket, http_eoh} ->
      {ok, lists:reverse(Headers)};
    {tcp_closed, Socket} ->
      {error, tcp_closed};
    {tcp_error, Socket, Reason} ->
      {error, Reason}
  after
    Timeout -> 
      gen_tcp:close(Socket),
      {error, Timeout}
  end.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").


calculate_redirected_url_test() ->
  ?assertEqual(<<"http://ya.ru/145">>, calculate_redirected_url("http://ya.ru/", "/145")),
  ?assertEqual(<<"http://ya.ru/145">>, calculate_redirected_url("http://ya.ru/", "http://ya.ru/145")),
  ?assertEqual(<<"http://yahoo.com/145">>, calculate_redirected_url("http://ya.ru/", "http://yahoo.com/145")).
  






  
