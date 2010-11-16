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
  lists:member(filename:extension(Name), [".jpg", ".jpeg"]).

write_frame(_Device, _Frame) -> 
  erlang:error(unsupported).


-record(media, {
  body,
  reader
}).

init({Access,Device} = Reader, Options) ->
  Size = proplists:get_value(size,Options),
  ?D({open,Reader,Size}),
  {ok, Body} = Access:pread(Device, 0, Size),
  Media = #media{body = Body},
  {ok, Media}.



properties(#media{}) -> 
  [{duration,10000}].


first(_Media) ->
  0.



read_frame(#media{} = Media, undefined) ->
  read_frame(Media, first(Media));


read_frame(#media{body = Body}, Key) ->
  DTS = Key,
  #video_frame{next_id = Key + 1000, dts = DTS, pts = DTS, codec = mjpeg, body = Body, content = video, flavor = keyframe};

read_frame(_, eof) ->
  eof.




seek(#media{} = Media, Timestamp, _Options) when Timestamp =< 0 ->
  {first(Media), 0};

seek(#media{} = _Media, _Timestamp, _Options) ->
  Key = 0,
  DTS = 0,
  {Key, DTS}.

