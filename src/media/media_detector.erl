%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Functions to detect different kinds of media, according to their urls
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(media_detector).
-author('Max Lapshin <max@maxidoors.ru>').
-export([rewrite/3, http/3, rtsp/3, ts_file/3, file/3, livestream/3]).


rewrite(Host, Name, _Opts) ->
  Rewrite = ems:get_var(rewrite, Host, []),
  case lists:keyfind(binary_to_list(Name), 1, Rewrite) of
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


ts_file(Host, Name, _Opts) ->
  case {check_path(Host, Name), mpegts_file_media:can_open_file(Name)} of
    {true, true} -> [{type, mpegts_file},{life_timeout,0}];
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
  case file_media:file_dir(Host) of
    undefined -> false;
    Dir -> 
      Path = filename:join([Dir, Name]), 
      case filelib:is_regular(filename:join([Dir, Name])) of
        true -> {true, Path};
        false -> false
      end  
  end.
