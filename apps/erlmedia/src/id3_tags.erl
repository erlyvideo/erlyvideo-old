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
-module(id3_tags).
-author('Ilya Shcherbak <tthread@gmail.com>').
-include("log.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([decode/1]).


decode(<<"ID3", _Info: 24, _:1,S1:7,_:1,S2:7,_:1,S3:7,_:1,S4:7, Body/binary>>) ->
  <<Size:28>> = <<S1:7,S2:7,S3:7,S4:7>>,
  case Size - size(Body) of
    Value when Value =< 10 -> 
      {Metadata,Rest} = id3v2_get_tags(Body,[]),
      {ok,Metadata,Rest};
    Value when Value > 10 ->
      {more,Size}
  end;

decode(<<Body/binary>>) ->
  {notfound,Body}.

get_encoding_from_bom(OrderByte) ->
  {Bom,_Number} = unicode:bom_to_encoding(OrderByte),
  Bom.

split_metaTags([],Body) ->
  Body;

split_metaTags([Head|Tail],<<Body/binary>>) ->
  NewTag = case is_list(Head) of
   true ->  binary:list_to_bin(Head);
   false -> Head
  end,
  Size = size(NewTag),
  split_metaTags(Tail,<<Body/binary,NewTag:Size/binary,"-">>).

get_textTags(List,[]) ->
  split_metaTags(List,<<>>);

get_textTags(List,[{FrameID,<<OrderByte:16,Body/binary>>}|Tail]) ->
  Result = case FrameID of
    "TALB" -> lists:merge(List,[unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))]);
    "TCON" -> lists:merge(List,[unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))]);
    "TIT2" -> lists:merge(List,[unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))]);
    "TPE1" -> lists:merge(List,[unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))]);
    "TRCK" -> lists:merge(List,[unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))]);
    "TYER" -> lists:merge(List,[unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))]);
    _Else -> lists:merge(List,[])
  end,
  get_textTags(Result,Tail);

get_textTags(List,[_FrameWithoutHeader|Tail]) ->
   get_textTags(List,Tail).


        
id3v2_get_tags(<<Body/binary>>, List)  ->
  case id3v2_get_one_tag(Body) of
    {FrameID,Size,Payload} -> 
      Offset = Size + 10,
      <<_Prev:Offset/binary,NewBody/binary>> = <<Body/binary>>,
      id3v2_get_tags(NewBody,lists:merge(List,[{FrameID,Payload}]));
    _Else ->
      Metadata = get_textTags([],List),
      {{name,Metadata},Body}
  end.
    
id3v2_get_one_tag(<<"T",FrameIDBin:24,Size:32,_Flag:16,PayloadBin:Size/binary,_/binary>>) ->
  FrameID = binary:bin_to_list(<<"T",FrameIDBin:24>>),
  <<_Encode,Payload/binary>> = <<PayloadBin:Size/binary>>,
  {FrameID, Size, Payload};

id3v2_get_one_tag(<<_FrameIDBin:32,Payload/binary>>) ->
  {error,Payload}.

decoding_test() ->
  Body = <<73,68,51,3,0,0,0,0,0,100,84,65,76,66,0,0,0,101,0,
         0,1,255,254,66,0,111,0,120,0,101,0,100,0,32,0,83,
         0,101,0,116,0,44,0,32,0,86,0,111,0,108,0,117,0,
         109,0,101,0,32,0,49,0,32,0,40,0,65,0,116,0,108,0,
         97,0,110,0,116,0,105,0,99,0,32,0,55,0,53,0,54,0,
         55,0,45,0,56,0,50,0,49,0,52,0,52,0,45,0,50,0,41,0,
         32,0,91,0,52,0,99,0,100,0,93,0,84,80,69,49,0,0,0,
         27,0,0,1,255,254,76,0,101,0,100,0,32,0,90,0,101,0,
         112,0,112,0,101,0,108,0,105,0,110,0,84,73,84,50,0,
         0,0,51,0,0,1,255,254,66,0,97,0,98,0,101,0,32,0,73,
         0,39,0,109,0,32,0,71,0,111,0,110,0,110,0,97,0,32>>,

 ?assertMatch({ok,[{"TALB",_Body1},{"TPE1",_Body2}],_Rest},decode(Body)).
