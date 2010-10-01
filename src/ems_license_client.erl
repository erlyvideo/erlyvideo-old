%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2010 Max Lapshin
%%% @doc        Client of erlyvideo license server
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
-module(ems_license_client).
-author('Max Lapshin <max@maxidoors.ru>').

-export([make_request/0]).


make_request() ->
  License = case file:path_consult(["priv", "/etc/erlyvideo"], "license.txt") of
    {ok, Env, LicensePath} ->
      error_logger:info_msg("Reading license key from ~s", [LicensePath]),
      proplists:get_value(license, Env);
    {error, enoent} ->
      error_logger:info_msg("No license file found, working in public mode"),
      "default";
    {error, Reason} ->
      error_logger:error_msg("Invalid license key: ~p", [Reason]),
      "default"
  end,
  Command = "init",
  LicenseUrl = ems:get_var(license_url, "http://license.erlyvideo.tv/license"),
  {_, _Auth, Host, Port, Path, _Query} = http_uri2:parse(LicenseUrl),
  {ok, Sock} = gen_tcp:connect(Host, Port, [binary]),
  inet:setopts(Sock, [{packet,http},{active,false}]),
  Query = io_lib:format("GET ~s?key=~s&command=~s HTTP/1.1\r\nHost: ~s\r\n\r\n", [Path, License, Command, Host]),
  gen_tcp:send(Sock, Query),
  {ok, {http_response, _HTTPVersion, Code, _Status}} = gen_tcp:recv(Sock, 0),
  case Code of
    200 -> ok;
    _ -> erlang:error(unauthenticated_request)
  end,
  Headers = read_headers(Sock, []),
  Length = proplists:get_value('Content-Length', Headers),
  {ok, Bin} = gen_tcp:recv(Sock, Length),
  gen_tcp:close(Sock),
  {reply, _ProtoVersion, Modules} = erlang:binary_to_term(Bin),
  load_modules(Modules),
  ok.
  
read_headers(Sock, Headers) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, {http_header, _, 'Content-Length' = Header, _, Value}} ->
      read_headers(Sock, [{Header,list_to_integer(Value)}|Headers]);
    {ok, {http_header, _, Header, _, Value}} ->
      read_headers(Sock, [{Header,Value}|Headers]);
    {ok, http_eoh} ->
      inet:setopts(Sock, [{packet,raw}]),
      Headers
  end.
  
  
  
load_modules([]) -> 
  ok;
  
load_modules([{module,Module,Body}|Modules]) ->
  code:load_binary(Module, atom_to_list(Module)++".erl", Body),
  case erlang:function_exported(Module, ems_client_load, 0) of
    true -> Module:ems_client_load();
    false -> ok
  end,
  load_modules(Modules).

  