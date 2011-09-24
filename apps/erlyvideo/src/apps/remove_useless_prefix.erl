%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2011 Max Lapshin
%%% @doc        RTMP functions that cleanup play command from useless adobe prefixes
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
-module(remove_useless_prefix).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../log.hrl").
-include_lib("rtmp/include/rtmp.hrl").

-export([play/2, getStreamLength/2]).

play(Session, #rtmp_funcall{args = [null, <<"flv:", FullName/binary>> | Args]} = AMF) ->
  {unhandled, Session, AMF#rtmp_funcall{args = [null, FullName, Args]}};

play(Session, #rtmp_funcall{args = [null, <<"mp4:", FullName/binary>> | Args]} = AMF) ->
  {unhandled, Session, AMF#rtmp_funcall{args = [null, FullName, Args]}};

play(Session, #rtmp_funcall{args = [null, <<"mp3:", FullName/binary>> | Args]} = AMF) ->
  {unhandled, Session, AMF#rtmp_funcall{args = [null, FullName, Args]}};

play(Session, #rtmp_funcall{args = [null, <<"*flv:", FullName/binary>> | Args]} = AMF) ->
  {unhandled, Session, AMF#rtmp_funcall{args = [null, FullName, Args]}};
  
play(_Session, _AMF) ->
  unhandled.
  
getStreamLength(Session, #rtmp_funcall{args = [null, <<"flv:", FullName/binary>> | Args]} = AMF) ->
  {unhandled, Session, AMF#rtmp_funcall{args = [null, FullName, Args]}};

getStreamLength(Session, #rtmp_funcall{args = [null, <<"mp4:", FullName/binary>> | Args]} = AMF) ->
  {unhandled, Session, AMF#rtmp_funcall{args = [null, FullName, Args]}};

getStreamLength(Session, #rtmp_funcall{args = [null, <<"mp3:", FullName/binary>> | Args]} = AMF) ->
  {unhandled, Session, AMF#rtmp_funcall{args = [null, FullName, Args]}};

getStreamLength(Session, #rtmp_funcall{args = [null, <<"*flv:", FullName/binary>> | Args]} = AMF) ->
  {unhandled, Session, AMF#rtmp_funcall{args = [null, FullName, Args]}};

getStreamLength(_Session, _AMF) ->
  unhandled.

