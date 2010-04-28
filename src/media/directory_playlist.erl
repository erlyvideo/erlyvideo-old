-module(directory_playlist).
-author('Max Lapshin <max@maxidoors.ru>').

-export([init/2, next/1, close/1]).

-record(directory_playlist, {
  path,
  host,
  files = []
}).


init(Host, Options) ->
  Path = proplists:get_value(path, Options),
  Wildcard = proplists:get_value(wildcard, Options),
  Files = filelib:wildcard(Wildcard, Path),
  #directory_playlist{path = Path, files = Files, host = Host}.

close(_) ->
  ok.

next(#directory_playlist{files = []}) ->
  eof;

next(#directory_playlist{files = [File|Files]} = Playlist) ->
  %[{type,file},{url,File},{name,File},{host,Host}]
  {Playlist#directory_playlist{files = Files}, File, []}.

