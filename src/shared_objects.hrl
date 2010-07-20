%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        shared objects constants
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information.
%%% @end
%%%
%%% This file is part of erlang-rtmp.
%%% 
%%% erlang-rtmp is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlang-rtmp is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlang-rtmp.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
%% RTMP shared object
-define(SO_CONNECT,              1).
-define(SO_DISCONNECT,           2).
-define(SO_SET_ATTRIBUTE,        3).
-define(SO_UPDATE_DATA,          4).
-define(SO_UPDATE_ATTRIBUTE,     5).
-define(SO_SEND_MESSAGE,         6).
-define(SO_STATUS,               7).
-define(SO_CLEAR_DATA,           8).
-define(SO_DELETE_DATA,          9).
-define(SO_DELETE_ATTRIBUTE,    10).
-define(SO_INITIAL_DATA,        11).

