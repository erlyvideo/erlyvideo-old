%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        ASF reader for erlyvideo
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org</a> for more information
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
-module(asf_reader).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/video_frame.hrl").
-include("../include/media_info.hrl").
-include("log.hrl").

-behaviour(gen_format).
-export([init/2, read_frame/2, media_info/1, properties/1, seek/3, can_open_file/1, write_frame/2]).

-record(asf_media, {
  asf
}).

can_open_file(Name) when is_binary(Name) ->
  can_open_file(binary_to_list(Name));

can_open_file(Name) ->
  lists:member(filename:extension(Name), [".asf", ".wmv", ".wma"]).


write_frame(_Device, _Frame) -> 
  erlang:error(unsupported).


init(Reader, Options) ->
  {ok, ASF} = asf:open(Reader, Options),
  {ok, #asf_media{asf = ASF}}.

properties(_Media) -> [].

media_info(_Media) -> #media_info{}.



read_frame(_Media, _Key) ->
  undefined.

seek(_Media, _Timestamp, _Options) -> undefined.




