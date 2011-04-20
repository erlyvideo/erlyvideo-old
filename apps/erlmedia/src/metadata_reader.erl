%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        MP3 reader for erlyvideo
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
-module(metadata_reader).
-author('Ilya Shcherbak <tthread@gmail.com>').
-include("log.hrl").
-include_lib("kernel/include/file.hrl").
-export([id3v2_get_tags/4]).

id3v2_get_tags(Module, Device, Size, Cur) ->
  id3v2_get_tags(Module, Device, Size, Cur, []).

id3v2_get_tags(Module, Device, Size, Cur,  List)  ->
  case id3v2_get_one_tag(Module,Device,Cur) of
    {FrameID,Payload,NewCur} when NewCur < Size -> 
      id3v2_get_tags(Module, Device, Size, NewCur, lists:merge(List,[{FrameID,Payload}]));
    Else ->
      List
  end.
    
id3v2_get_one_tag(Module,Device,Cur) ->
  case id3v2_get_info(Module,Device,Cur) of
     [FrameID,Long] when Long =/= 0 -> 
       case Module:pread(Device,Cur+10,Long) of
         {ok,<<Encode,Order:16,Payload/binary>>} ->
           {FrameID,Payload,Cur+Long+10};
         Else -> {error,Else}
       end;
      Else -> {error,Else}
  end.

id3v2_get_info(Module,Device,Cur) ->
  case id3v2_get_frameId(Module,Device,Cur) of 
    [84|_] = FrameID ->  
      case Module:pread(Device,Cur+4,6) of 
        {ok, <<Size:32,Flag:16>>} ->
           [FrameID,Size];
        Else -> {error,Else}
      end;
    Else -> {error, Else}
  end.

id3v2_get_frameId(Module,Device,Cur) -> 
  case Module:pread(Device,Cur,4) of
    {ok, <<FrameID:32>>} ->
      binary:bin_to_list(<<FrameID:32>>);
    Else -> {error,Else}
  end.
