-module(erlyvideo_http_file, [Prefix]).
-author('Max Lapshin <max@maxidoors.ru>').

-export([check/3]).


check(_Host, Name, _Opts) ->
  case re:run(Name, Prefix) of
    {match, _} -> 
      % Cache = filename:join([file_media:file_dir(Host), "cache", Path]),
      [{type, file},{url,Name},{file_access,http_file}];
    _ -> 
      false
  end.
