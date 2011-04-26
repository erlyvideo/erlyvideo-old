%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        Example of RTMP plugin
%%% 
%%% This example intercepts "connect" function calls, made by flash client and checks login/password
%%% 
%%% Mention, that it is STRICTLY PROHIBITED to use internal erlyvideo structures outside of public include 
%%% folders, because they can change in any release, including minor. Your code will be broken.
%%% 
%%% Use accessor methods.
%%% 
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
-module(rtmp_example_plugin).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("rtmp/include/rtmp.hrl").

-export([connect/2]).

connect(Session, #rtmp_funcall{args = [_, Login, Password]}) ->
  case {Login, Password} of
    {"user", "passw0rt"} -> rtmp_session:accept_connection(Session);
    _ -> rtmp_session:reject_connection(Session)
  end.
