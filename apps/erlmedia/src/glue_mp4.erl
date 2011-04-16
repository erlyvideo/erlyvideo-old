%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2011 Max Lapshin
%%% @doc        Proper mp4 gluer
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%%
%%% This file is part of erlmedia.
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
-module(glue_mp4).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include("../include/mp4.hrl").
-include("../include/video_frame.hrl").

-export([glue/2]).

glue(Files, _Output) ->
  _Medias = [read_mp4(File) || File <- Files],
  ok.


read_mp4(Path) ->
  {ok, F} = file:open(Path, [read,binary]),
  {ok, Reader} = mp4_reader:open({file, F}, []),
  First = mp4_reader:first(Reader),
  {Reader, Frames} = read_frames(Reader, First, []),
  {Reader, Frames}.

read_frames(Reader, Id, Frames) ->
  case mp4_reader:read_frame(Reader, Id) of
    #video_frame{flavor = config, next_id = Next} -> read_frames(Reader, Next, Frames)
  end.