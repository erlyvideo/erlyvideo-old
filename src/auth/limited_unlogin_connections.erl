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
-module(limited_unlogin_connections, [Timer]).
-author('Ilya Shcherbak <ilya@erlyvideo.org>').
-include_lib("../log.hrl").
-include_lib("../../include/rtmp_session.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-export([pauseRaw/2,play/2]).

parse_start_args(RawArgs) ->
  [null, FullName|Args] = RawArgs,
  {_RawName, Args2} = http_uri2:parse_path_query(FullName),
  Options1 = apps_streaming:extract_play_args(Args),
  Options2 = apps_streaming:extract_url_args(Args2),
  Options = lists:ukeymerge(1,Options2,Options1),
  proplists:get_value("start ", Options).

play(#rtmp_session{user_id = undefined} = _State, Funcall) ->
  Start = parse_start_args(Funcall#rtmp_funcall.args),
  case Start of
    Num when Num >= Timer  ->
     {ok, _Ref} = timer:send_after(0, self(),exit),
     unhandled;
    _ -> 
      {ok,_Ref} = timer:send_after(Timer, self(), exit),
      unhandled
 end;

play(_State, _Funcall) ->
  unhandled.
 
pauseRaw(#rtmp_session{user_id = 123} = State, _Funcall) -> 
 State;

pauseRaw(_Session,_Funcall_) -> 
  unhandled.
