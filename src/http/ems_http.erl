% @hidden
-module(ems_http).
-export([start_link/1, stop/0, handle_http/1, http/4, wwwroot/1]).
-include("../include/ems.hrl").

  
% start misultin http server
start_link(Port) ->
	misultin:start_link([{port, Port}, {loop, fun handle_http/1}]).

% stop misultin
stop() ->
	misultin:stop().

% callback on request received
handle_http(Req) ->
  random:seed(now()),
  Host = ems:host(Req:host()),
  Method = Req:get(method),
  Path = Req:resource([urldecode]),
  Chain = ems:get_var(www_handlers, Host, [ems_http_templates, ems_http_rtmpt, ems_http_push, ems_http_mpegts, ems_http_flv, ems_http_file]),
  
  try_handler(Chain, Host, Method, Path, Req).

try_handler([], Host, Method, Path, Req) ->
  try_handler([?MODULE], Host, Method, Path, Req);

  
try_handler([Handler|Chain], Host, Method, Path, Req) ->
  try Handler:http(Host, Method, Path, Req) of
    unhandled -> 
      try_handler(Chain, Host, Method, Path, Req);
    Else ->
      Else
  catch
    exit:leave ->
      exit(leave);
    Class:Error ->
      ems_log:error(Host, "HTTP Error~n~p~n~p:~p:~p~n", [Path, Class, Error, erlang:get_stacktrace()]),
      Req:respond(500, [{"Content-Type", "text/plain"}], "500 Server Error~n~p~n~p:~p:~p~n", 
                       [Path, Class, Error, erlang:get_stacktrace()])
  end.  	

wwwroot(Host) ->
  ems:get_var(wwwroot, Host, ems:get_var(wwwroot, "wwwroot")).

  
http(Host, 'GET', ["stats.json"], Req) ->
  Stats = lists:map(fun({Name, Clients}) ->
    {Name, {struct, [{clients, length(Clients)}]}}
  end, media_provider:entries(Host)),
  Req:respond(200, [{"Content-Type", "application/json"}], [mochijson2:encode({struct, Stats}), "\n"]);
  

% handle the 404 page not found
http(_Host, _, Path, Req) ->
	Req:respond(404, [{"Content-Type", "text/plain"}], "404 Page not found. ~p: ~p", [Path, Req]).

