-module(ems_http).
-export([start_link/1, stop/0, handle_http/1]).

% start misultin http server
start_link(Port) ->
	misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req) end}]).

% stop misultin
stop() ->
	misultin:stop().

% callback on request received
handle_http(Req) ->	
  handle(Req:get(method), Req:resource([lowercase, urldecode]), Req).


handle('GET', [], Req) ->
  io:format("GET /~n"),
	Req:file("player/player.html");

handle('GET', ["Player.swf"], Req) ->
  io:format("GET /Player.swf~n"),
  Req:file("player/Player.swf"),
  Req:stream(close);
  
% handle the 404 page not found
handle(_, _, Req) ->
	Req:respond(404, [{"Content-Type", "text/plain"}], "404 Page not found.").
