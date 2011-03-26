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
-module(anonymous_time_limit, [Timer]).
-author('Ilya Shcherbak <ilya@erlyvideo.org>').
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("../log.hrl").
-include_lib("../../include/rtmp_session.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-export([pauseRaw/2, play/2, seek/2]).


play(#rtmp_session{user_id = undefined} = Session, #rtmp_funcall{args = [null,FullName|Args]}) ->
  {_Name,Options} = apps_streaming:parse_play(FullName, Args),
  case proplists:get_value(start, Options) of
    undefined ->
      timer:send_after(Timer, exit),
      unhandled;
    Num when is_integer(Num) andalso Num >= Timer ->
      Session;
    Num when is_integer(Num) andalso Num >= Timer ->
      timer:send_after(Timer - Num, exit),
      unhandled
 end;

play(_State, _Funcall) ->
  unhandled.
 
pauseRaw(#rtmp_session{user_id = undefined} = State, _Funcall) -> 
 State;

pauseRaw(_Session,_Funcall_) -> 
  unhandled.

seek(#rtmp_session{user_id = undefined} = State, _Funcall) -> 
 State;

seek(_Session,_Funcall_) -> 
  unhandled.
