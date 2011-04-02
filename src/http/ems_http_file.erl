%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        serving file by http
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
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
-module(ems_http_file, [DocRoot]).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../ems.hrl").
-include("../log.hrl").
-include_lib("kernel/include/file.hrl").

-export([http/4]).



http(Host, Method, Path, Req) when Method == 'GET' orelse Method == 'HEAD' ->
  Root1 = if
    is_list(DocRoot) -> DocRoot;
    is_atom(DocRoot) ->
      case code:lib_dir(DocRoot, wwwroot) of
        R when is_list(R) -> R;
        _ -> undefined
      end;
    true -> undefined
  end,
  
  Root = ems:expand_path(Root1),
  
  if
    is_list(Root) -> serve_file(Host, Method, Root, Path, Req);
    true -> unhandled
  end;    

http(_Host, _Method, _Path, _Req) ->
  unhandled.


serve_file(Host, Method, Root, Path, Req) ->
  FileName = filename:absname(ems:pathjoin([Root | Path])),
  case filelib:is_regular(FileName) of
    true when Method == 'GET' ->
      ems_log:access(Host, "GET ~p ~s /~s", [Req:get(peer_addr), "-", string:join(Path, "/")]),
      Req:file(FileName);
    true when Method == 'HEAD' ->
      {ok, #file_info{size = Size}} = file:read_file_info(FileName),
    	Req:stream(head, [{'Content-Length', Size}]),
      Req:stream(close);
    false ->
      AltPath = ems:pathjoin([FileName, "index.html"]),
      case filelib:is_regular(AltPath) of
        true ->
          ems_log:access(Host, "GET ~p ~s /~s", [Req:get(peer_addr), "-", string:join(Path, "/")]),
          Req:file(AltPath);
        false ->  
          unhandled
      end
  end.
  