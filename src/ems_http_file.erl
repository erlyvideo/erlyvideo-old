-module(ems_http_file).
-author('Max Lapshin <max@maxidoors.ru>').

-export([check/3]).


check(Host, Name, _Opts) ->
  {ok, Re} = re:compile("http://(.*)", [{capture,all,list}]),
  case re:run(Name, Re) of
    {match, [_, Path]} -> 
      Cache = filename:join([file_media:file_dir(Host), "cache", Path]),
      [{type, file},{url,Name},{access,http_file},{cache_file,Cache}];
    _ -> 
      false
  end.
