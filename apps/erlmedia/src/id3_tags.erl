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
-export([decode/1]).


decode(<<"ID3", _Info: 24, _:1,S1:7,_:1,S2:7,_:1,S3:7,_:1,S4:7, Body/binary>>) ->
  <<Size:28>> = <<S1:7,S2:7,S3:7,S4:7>>,
  case Size - lists:flatlength(binary:bin_to_list(<<Body/binary>>)) of
    Value when Value =< 100 -> 
      {Metadata,Rest} = id3v2_get_tags(Body,[]),
      {ok,Metadata,Rest};
    Value when Value > 100 ->
      {more,Size}
  end;

decode(<<Body/binary>>) ->
  {notfound,Body}.
        
id3v2_get_tags(<<Body/binary>>, List)  ->
  case id3v2_get_one_tag(Body) of
    {FrameID,Size,Payload} -> 
      Offset = Size + 10,
      <<_Prev:Offset/binary,NewBody/binary>> = <<Body/binary>>,
      id3v2_get_tags(NewBody,lists:merge(List,[{FrameID,Payload}]));
    _Else ->
      {List,Body}
  end.
    
id3v2_get_one_tag(<<"T",FrameIDBin:24,Size:32,_Flag:16,PayloadBin:Size/binary,_/binary>>) ->
  FrameID = binary:bin_to_list(<<"T",FrameIDBin:24>>),
  <<_Encode,Payload/binary>> = <<PayloadBin:Size/binary>>,
  {FrameID, Size, Payload};

id3v2_get_one_tag(<<_FrameIDBin:32,Payload/binary>>) ->
  {error,Payload}.

