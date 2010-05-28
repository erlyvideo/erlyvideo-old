-module(ems_rtsp).
-author('Max Lapshin <max@maxidoors.ru>').

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-export([record/2, announce/3]).

hostpath(URL) ->
  {ok, Re} = re:compile("rtsp://([^/]+)/(.*)$"),
  {match, [_, HostPort, Path]} = re:run(URL, Re, [{capture, all, binary}]),
  {ems:host(HostPort), Path}.


record(URL, Headers) -> 
  {Host, Path} = hostpath(URL),
  ?D({"RECORD", Host, Path, Headers}),
  {Module, Function} = ems:check_app(Host, auth, 3),
  
  case Module:Function(Host, rtsp, proplists:get_value('Authorization', Headers)) of
    undefined ->
      {error, authentication};
    _Session ->
      ems_log:access(Host, "RTSP RECORD ~s ~s", [Host, Path]),
      Media = media_provider:open(Host, Path, [{type, live}]),
      ems_media:set_source(Media, self()),
      {ok, Media}
  end.


announce(URL, Headers, _Body) ->
  {Host, Path} = hostpath(URL),
  ?D({"Announce", Host, Path, Headers}),
  {Module, Function} = ems:check_app(Host, auth, 3),
  
  case Module:Function(Host, rtsp, proplists:get_value('Authorization', Headers)) of
    undefined ->
      {error, authentication};
    Else ->
      ok
  end.

    