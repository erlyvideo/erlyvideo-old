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
-module(anonymous_seek_protection).
-author('Ilya Shcherbak <ilya@erlyvideo.org>').
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("../log.hrl").
-include_lib("../rtmp/rtmp_session.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-export([pauseRaw/2, seek/2]).

 
pauseRaw(#rtmp_session{user_id = undefined} = State, _Funcall) ->
  ?D(reject_forbidden_pause),
  State;

pauseRaw(#rtmp_session{} = Session,_Funcall_) ->
  case get(auth_play_limit) of
    undefined -> unhandled;
    _ -> ?D(reject_forbidden_pause), Session
  end.

seek(#rtmp_session{user_id = undefined} = State, _Funcall) -> 
  ?D(reject_forbidden_seek),
  State;

seek(#rtmp_session{} = Session,_Funcall_) -> 
  case get(auth_play_limit) of
    undefined -> unhandled;
    _ -> ?D(reject_forbidden_seek), Session
  end.
