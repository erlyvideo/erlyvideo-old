%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2011 Max Lapshin
%%% @doc        test helper module for erlyvideo
%%% 
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlmedia is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlmedia is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlmedia.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(ems_test_helper).
-author('Max Lapshin <max@maxidoors.ru>').


-compile(export_all).

file_dir() ->
  code:lib_dir(erlyvideo, test) ++ "/files".

file_path(File) ->
  filename:join(file_dir(), File).