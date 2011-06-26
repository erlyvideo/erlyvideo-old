%%% @hidden
%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        erlyvideo http handler
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
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
-module(ems_http).
-export([start_listener/1, stop/0, handle_http/1, http/4, wwwroot/1, host/1]).
-include("../log.hrl").

  
% start misultin http server
start_listener(Port) when is_number(Port) ->
  misultin:start_link([{port, Port},{recbuf,65536},{max_connections,10240}, {loop, fun(Req) -> handle_http(Req) end}]).
	
  
  
% stop misultin
stop() ->
	misultin:stop().
	

host(Req) ->
  case proplists:get_value('Host', Req:get(headers)) of
    undefined -> default;
    Val -> ems:host(hd(string:tokens(Val, ":")))
  end.

% callback on request received
handle_http(Req) ->
  random:seed(now()),
  Host = host(Req),
  Method = Req:get(method),
  Path = Req:resource([urldecode]),
  Chain = ems:get_var(www_handlers, Host, [ems_http_rtmpt, {ems_http_file, "wwwroot"}]),  
  Headers = Req:get(headers),
  % ?D({http, Method, Path, Headers}),
  case Path  of
    ["iphone"| _] -> 
      try_handler(Chain, Host, Method, Path, Req);
    _Else ->
      admin_protect(Headers, Chain, Host, Method, Path, Req)
  end.

admin_protect(Headers, Chain, Host, Method, Path, Req) ->
  case ems:get_var(http_admin_password,Host,[]) of
    [] ->
      try_handler(Chain, Host, Method, Path, Req);
    AuthData -> 
      case http_auth(AuthData,Headers) of
        true -> 
          try_handler(Chain, Host, Method, Path, Req);
        false -> 
          Req:respond(401,[{'WWW-Authenticate', "Basic realm=My Realm"}],"Please login in system")
      end
  end.

http_auth(AuthData,Headers) ->
  Str = "Basic "++base64:encode_to_string(AuthData),
  case  proplists:get_value('Authorization',Headers) of
    Str  -> 
      true;
    _ -> 
      false
  end.

try_handler([], Host, Method, Path, Req) ->
  try_handler([?MODULE], Host, Method, Path, Req);

  
try_handler([Handler|Chain], Host, Method, Path, Req) ->
  try Handler:http(Host, Method, Path, Req) of
    unhandled ->
      try_handler(Chain, Host, Method, Path, Req);
    Else ->
      Else
  catch
    Class:Error ->
      ems_log:error(Host, "HTTP Error~n~p~n~p:~p:~p~n", [Path, Class, Error, erlang:get_stacktrace()]),
      Req:respond(500, [{"Content-Type", "text/plain"}], "500 Server Error~n~p~n~p:~p:~p~n", 
                       [Path, Class, Error, erlang:get_stacktrace()])
  end.  	

wwwroot(Host) ->
  ems:get_var(wwwroot, Host, ems:get_var(wwwroot, "wwwroot")).
  
  
% handle the 404 page not found
http(Host, _, Path, Req) ->
  ems_log:access(Host, "FAIL ~p ~s /~s", [Req:get(peer_addr), "-", string:join(Path, "/")]),
	Req:respond(404, [{"Content-Type", "text/plain"}], "404 Page not found. ~p: ~p", [Path, Req]).

