%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Template of format reader
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
-module(jpeg_reader).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(gen_format).
-include("../include/video_frame.hrl").
-include("log.hrl").


-export([init/2, read_frame/2, properties/1, seek/3, can_open_file/1, write_frame/2]).

can_open_file(Name) when is_binary(Name) ->
  can_open_file(binary_to_list(Name));

can_open_file(Name) ->
  lists:member(filename:extension(Name), [".ext"]).

write_frame(_Device, _Frame) -> 
  erlang:error(unsupported).


-record(media, {
  body,
  reader
}).

init(Reader, Options) ->
  Body = undefined,
  Media = #media{body = Body},
  {ok, Media}.



properties(#media{} = Media) -> 
  [].


first(Media) ->
  0.




read_frame(MediaInfo, undefined) ->
  read_frame(MediaInfo, first(MediaInfo));

read_frame(#media{} = Media, Key) ->
  DTS = 0,
  PTS = 0,
  Next = 0,
  #video_frame{next_id = Next, dts = DTS, pts = DTS};

read_frame(_, eof) ->
  eof.




seek(#media{} = Media, before, Timestamp) when Timestamp =< 0 ->
  {first(Media), 0};

seek(#media{} = Media, before, Timestamp) ->
  Key = 0,
  DTS = 0,
  {Key, DTS}.

