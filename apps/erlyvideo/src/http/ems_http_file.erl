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
  Root = if
    is_list(DocRoot) -> DocRoot;
    is_atom(DocRoot) ->
      case code:lib_dir(DocRoot, wwwroot) of
        R when is_list(R) -> R;
        _ -> undefined
      end;
    true -> undefined
  end,
  
  Accept = proplists:get_value('Accept', Req:get(headers)),
  if
    Accept == "application/x-rtsp-tunnelled" -> serve_rtsp(Host, Method, Path, Req);
    is_list(Root) -> serve_file(Host, Method, Root, Path, Req);
    true -> unhandled
  end;    

http(_Host, _Method, _Path, _Req) ->
  ?D({unhandled, _Host, _Method, _Path}),
  unhandled.


serve_file(Host, Method, Root, Path, Req) ->
  FullPath = ems:pathjoin([Root | Path]),
  case serve_file_from_disk(Host, Method, FullPath, Req) of
    unhandled ->
      serve_file_from_disk(Host, Method, ems:pathjoin(FullPath, "/index.html"), Req);
    Else ->
      Else
  end.

serve_file_from_disk(Host, Method, Path, Req) ->
  FileName = filename:absname(Path),
  io:format("serve ~p~n",[FileName]),
  case filelib:is_regular(FileName) of
    true when Method == 'GET' ->
      ems_log:access(Host, "GET ~p ~s /~s", [Req:get(peer_addr), "-", string:join(Path, "/")]),
      Req:file(FileName);
    true when Method == 'HEAD' ->
      {ok, #file_info{size = Size}} = file:read_file_info(FileName),
    	Req:stream(head, [{'Content-Length', Size}]),
      ems_log:access(Host, "HEAD ~p ~s /~s", [Req:get(peer_addr), "-", string:join(Path, "/")]),
      Req:stream(close);
    false ->
      serve_file_from_escript(Host, Method, Path, Req)
  end.

serve_file_from_escript(Host, Method, Path, Req) ->
  io:format("escript file ~p~n", [Path]),
  case ems_file:read_file_info(Path) of
    {ok, #file_info{size = Size}} ->
      {ok, Bin} = ems_file:read_file(Path),
      Headers = [{'Content-Type', misultin_utility:get_content_type(Path)}, {'Content-Length', integer_to_list(Size)}],
      ems_log:access(Host, "~s ~p ~s /~s", [Method, Req:get(peer_addr), "-", Path]),
      case Method of
        'HEAD' ->
          Req:respond(200, Headers, []);
        'GET' ->
			    Req:respond(200, Headers, Bin)
			end;
		_Else ->
		  unhandled
	end.	

serve_rtsp(_Host, _Method, _Path, Req) ->
  % ?D({rtsp, Host, Path, Req:get(headers), Req:get(body)}),
  Req:respond(200, [{'Content-Type', "application/x-rtsp-tunnelled"},{'Cache-Control', "no-store"},{'Pragma',"no-cache"},{'Connection', "close"}], ["\n"]).
