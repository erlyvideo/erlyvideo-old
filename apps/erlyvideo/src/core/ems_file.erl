-module(ems_file).
-author('Max Lapshin <max@maxidoors.ru>').

-export([read_file/1, read_file_info/1]).
-export([load_escript_files/0]).


read_file(Path) ->
  case file:read_file(Path) of
    {error, enoent} ->
      read_escript_file(Path);
    Else ->
      Else
  end.

read_escript_file(Path) ->  
  {ok, EscriptFiles} = application:get_env(erlyvideo, escript_files),
  case lists:keyfind(Path, 1, EscriptFiles) of
    false -> {error, enoent};
    {_Path, _Info, Bin} -> {ok, Bin}
  end.

read_file_info(Path) ->
  case file:read_file_info(Path) of
    {error, enoent} ->
      read_escript_file_info(Path);
    Else ->
      Else
  end.  

read_escript_file_info(Path) ->  
  {ok, EscriptFiles} = application:get_env(erlyvideo, escript_files),
  case lists:keyfind(Path, 1, EscriptFiles) of
    false -> {error, enoent};
    {_Path, Info, _Bin} -> {ok, Info}
  end.


load_escript_files() ->
  {ok, Opts} = escript:extract(escript:script_name(), [compile_source]),
  ArchiveBin = proplists:get_value(archive, Opts),
  zip:foldl(fun(InnerPath, GetInfo, GetBin, Acc) ->
    [{InnerPath, GetInfo(), GetBin()}|Acc]
  end, [], {escript:script_name(), ArchiveBin}).



  
  