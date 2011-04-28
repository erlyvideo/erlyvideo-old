%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2011 Max Lapshin
%%% @doc        Erlyvideo API suitable for admin interface
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
-module(ems_http_erlyvideo_api).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../log.hrl").
-include_lib("kernel/include/file.hrl").
-export([http/4]).



http(Host, 'GET', ["erlyvideo", "api", "filelist"], Req) ->
  RawList = case file:list_dir(file_media:file_dir(Host)) of
    {ok, FL} -> FL;
    {error, Error} -> 
      error_logger:error_msg("Invalid file_dir directory: ~p (~p)~n", [file_media:file_dir(Req:host()), Error]),
      []
  end,
  
  Allowed = [".mp4", ".m4a", ".flv", ".ts", ".mp3"],
  
  FileList = lists:foldr(fun(Path, List) ->
    case lists:member(filename:extension(Path), Allowed) of
      true ->
        BinPath = unicode:characters_to_binary(Path),
        [[{id,BinPath},{text,BinPath},{leaf,true}]|List];
      _ ->
        List
    end
  end, [], lists:sort(RawList)),
  
  Req:ok([{'Content-Type', "application/json"}], [mochijson2:encode(FileList), "\n"]);


http(Host, 'GET', ["erlyvideo", "api", "streams"], Req) ->
  Streams = [ clean_values([{name,Name}|Info]) || {Name, _Pid, Info} <- media_provider:entries(Host)],
  Req:ok([{'Content-Type', "application/json"}], [mochijson2:encode([{streams,Streams}]), "\n"]);

http(Host, 'GET', ["erlyvideo", "api", "stream" | Path], Req) ->
  Name = string:join(Path, "/"),
  case media_provider:find(Host, Name) of
    {ok, Media} ->
      Info = ems_media:full_info(Media),
      Req:ok([{'Content-Type', "application/json"}], [mochijson2:encode([{stream,Info}]), "\n"]);
    undefined ->
      Req:respond(500, [{'Content-Type', "application/json"}], [mochijson2:encode([{error, unknown}]), "\n"])
  end;

http(_Host, 'GET', ["erlyvideo","api","licenses"], Req) -> 
  case ems_license_client:list() of
    {ok, List} -> 
      Info = [Project || {project, Project} <- List],
      Req:ok([{'Content-Type', "application/json"}], [mochijson2:encode([{licenses, Info}]), "\n"]);
    {error, notfound} ->
      Req:respond(404, [{'Content-Type', "application/json"}], [mochijson2:encode([{error, notfound}]), "\n"]);
    _Else ->
      Req:respond(500, [{'Content-Type', "application/json"}], [mochijson2:encode([{error, unknown}]), "\n"])
  end;

http(_Host, 'POST', ["erlyvideo","api","licenses"], Req) ->
  Reply = case ems_license_client:load() of
   ok -> ok;
   {error, Reason} -> Reason
  end,
  Req:ok([{'Content-Type', "application/json"}], [mochijson2:encode([{state,Reply}]),"\n"]);


http(_, _, _, _) ->
  unhandled.
  

clean_values(Info) ->
  clean_values(lists:ukeysort(1, lists:reverse(Info)), []).
  
clean_values([], Acc) ->
  lists:keysort(1, Acc);
  
clean_values([{Key,Value}|Info], Acc) when is_binary(Value) ->
  case mochijson2:json_bin_is_safe(Value) of
    true -> clean_values(Info, [{Key,Value}|Acc]);
    false -> clean_values(Info, Acc)
  end;

clean_values([{_Key, Value}|Info], Acc) when is_tuple(Value) ->
  clean_values(Info, Acc);
  
clean_values([{K,V}|Info], Acc) ->
  clean_values(Info, [{K,V}|Acc]).


