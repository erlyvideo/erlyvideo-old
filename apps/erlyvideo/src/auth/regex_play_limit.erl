%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        trusted login
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
-module(regex_play_limit, [Regex, Count]).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("rtmp/include/rtmp.hrl").
-include("../rtmp/rtmp_session.hrl").
-include("../log.hrl").

-export([play/2]).



play(#rtmp_session{host = Host} = State, #rtmp_funcall{args = [null, FullName | Args]} = _AMF) ->
  {Name, _Options} = apps_streaming:parse_play(FullName, Args),
  case re:run(Name, Regex) of
    nomatch -> unhandled;
    {match, _} ->
      ClientsCount = lists:sum(lists:map(fun({Entry, _Pid, Info}) ->
        case re:run(Entry, Regex) of
          nomatch -> 0;
          {match, _} -> proplists:get_value(client_count, Info)
        end
      end, media_provider:entries(Host))),
      if 
        ClientsCount < Count -> unhandled;
        true -> ?D({too_many_connections, Regex, ClientsCount, "limit is", Count}), State
      end  
  end.

