%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Functions to detect different kinds of media, according to their urls
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
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
-module(media_detector).
-author('Max Lapshin <max@maxidoors.ru>').
-export([rewrite/3, http/3, rtsp/3, rtmp/3, ts_file/3, file/3, livestream/3, check_path/2]).


rewrite(Host, Name, Opts) when is_binary(Name) -> 
  rewrite(Host, binary_to_list(Name), Opts);
  
rewrite(Host, Name, _Opts) ->
  Rewrite = ems:get_var(rewrite, Host, []),
  case lists:keyfind(Name, 1, Rewrite) of
    false -> false;
    {_NameS, Type, URL} -> [{type, Type}, {url, URL}];
    {_NameS, Type, URL, Options} -> [{type, Type}, {url, URL} | Options]
  end.
  

http(_Host, Name, _Opts) ->
  {ok, Re} = re:compile("http://(.*)"),
  case re:run(Name, Re) of
    {match, _Captured} -> [{type, http},{url,Name}];
    _ -> false
  end.

rtsp(_Host, Name, _Opts) ->
  {ok, Re} = re:compile("rtsp://(.*)"),
  case re:run(Name, Re) of
    {match, _Captured} -> [{type, rtsp},{url,Name}];
    _ -> false
  end.


rtmp(_Host, Name, _Opts) ->
  {ok, Re} = re:compile("rtmp://(.*)"),
  case re:run(Name, Re) of
    {match, _Captured} -> [{type, rtmp},{url,Name}];
    _ -> false
  end.


ts_file(Host, Name, _Opts) ->
  case {check_path(Host, Name), mpegts_file_media:can_open_file(Name)} of
    {{true, Path}, true} -> [{type, mpegts_file},{life_timeout,0},{url,Path}];
    _ -> false
  end.

file(Host, Name, Opts) ->
  case check_path(Host, Name) of
    {true, Path} -> [{type, file}, {url, Path}];
    _ ->
      case check_path(Host, <<Name/binary, ".flv">>) of
        {true, Path} -> [{type, file}, {url, Path}];
        _ -> detect_prefixed_file(Host, Name, Opts)
      end
  end.

detect_prefixed_file(Host, <<"flv:", Name/binary>>, _Opts) ->
  case check_path(Host, Name) of
    {true, Path} -> [{type, file}, {url, Path}];
    _ -> 
      case check_path(Host, <<Name/binary, ".flv">>) of
        {true, Path} -> [{type, file}, {url, Path}];
        false -> false
      end
  end;

detect_prefixed_file(Host, <<"mp4:", Name/binary>>, _Opts) ->
  case check_path(Host, Name) of
    {true, Path} -> [{type, file}, {url, Path}];
    _ -> 
      case check_path(Host, <<Name/binary, ".mp4">>) of
        {true, Path} -> [{type, file}, {url, Path}];
        false -> false
      end
  end;
  
detect_prefixed_file(_Host, _Name, _Opts) ->
  false.
  
  
livestream(_Host, Name, Opts) ->
  case proplists:get_value(wait, Opts) of
    undefined ->
      false;
    _ ->
      [{type, live}, {url, Name}]
  end.



check_path(Host, Name) when is_binary(Name) ->
  check_path(Host, binary_to_list(Name));

check_path(Host, Name) ->
  check_paths(ems:get_var(file_dirs, Host, [file_media:file_dir(Host)]), Name).

check_paths([], _Name) ->
  false;

check_paths([undefined|Dirs], Name) ->
  check_paths(Dirs, Name);
  
check_paths([Dir|Dirs], Name) ->
  Path = ems:pathjoin(Dir, Name), 
  case filelib:is_regular(Path) of
    true -> {true, list_to_binary(Path)};
    false -> check_paths(Dirs, Name)
  end.
