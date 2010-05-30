-module(ems_rtsp).
-author('Max Lapshin <max@maxidoors.ru>').

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-export([record/3, announce/3, describe/3, play/3]).

hostpath(URL) ->
  {ok, Re} = re:compile("rtsp://([^/]+)/(.*)$"),
  {match, [_, HostPort, Path]} = re:run(URL, Re, [{capture, all, binary}]),
  {ems:host(HostPort), Path}.


announce(URL, Headers, _Body) -> 
  {Host, Path} = hostpath(URL),
  ?D({"ANNOUNCE", Host, Path, Headers}),
  {Module, Function} = ems:check_app(Host, auth, 3),
  
  case Module:Function(Host, rtsp, proplists:get_value('Authorization', Headers)) of
    undefined ->
      {error, authentication};
    _Session ->
      Media = media_provider:open(Host, Path, [{type, live}]),
      ems_media:set_source(Media, self()),
      {ok, Media}
  end.


record(URL, Headers, _Body) ->
  {Host, Path} = hostpath(URL),
  ?D({"RECORD", Host, Path, Headers}),
  {Module, Function} = ems:check_app(Host, auth, 3),
  
  case Module:Function(Host, rtsp, proplists:get_value('Authorization', Headers)) of
    undefined ->
      {error, authentication};
    _Else ->
      ems_log:access(Host, "RTSP RECORD ~s ~s", [Host, Path]),
      ok
  end.

describe(_URL, _Headers, _Body) ->
  ok.

play(URL, Headers, _Body) ->
  {Host, Path} = hostpath(URL),
  ?D({"RECORD", Host, Path, Headers}),
  % {Module, Function} = ems:check_app(Host, auth, 3),
  ems_log:access(Host, "RTSP PLAY ~s ~s", [Host, Path]),
  {ok, Media} = media_provider:play(Host, Path, [{stream_id,1}]),
  {ok, Media}.
  