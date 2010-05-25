-module(ems_rtsp).
-author('Max Lapshin <max@maxidoors.ru>').

-export([record/1]).

hostpath(URL) ->
  {ok, Re} = re:compile("rtsp://([^/]+)/(.*)$"),
  {match, [_, HostPort, Path]} = re:run(URL, Re, [{capture, all, binary}]),
  {ems:host(HostPort), Path}.


record(URL) -> 
  {Host, Path} = hostpath(URL),
  % ?D({"RECORD", Host, Path}),
  ems_log:access(Host, "RTSP RECORD ~s ~s", [Host, Path]),
  Media = media_provider:open(Host, Path, [{type, live}]),
  ems_media:set_source(Media, self()),
  {ok, Media}.


