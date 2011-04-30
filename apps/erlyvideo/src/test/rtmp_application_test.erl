%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTMP functions that support playing
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
-module(rtmp_application_test).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../log.hrl").
-include_lib("rtmp/include/rtmp.hrl").

-export([amf3_typed_response/2]).

amf3_typed_response(State, #rtmp_funcall{args = [null | Args]} = AMF) ->
  ?D({zz, Args}),
  Objects = [{object,<<"com.erlyvideo.User">>,[{id,3},{age,35},{name,<<"John Doe">>}]},
             {object,<<"com.erlyvideo.User">>,[{id,4},{age,34},{name,<<"Bob Bobob">>}]}],
  rtmp_session:reply(State, AMF#rtmp_funcall{args = [null, Objects]}),
  ?D({sent_reply, rtmp_socket:getopts(rtmp_session:get(State, socket), amf_version), Objects}),
  State.


