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
-module(mp3_reader).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("../../include/ems.hrl").

-behaviour(gen_format).
-export([init/1, read_frame/2, properties/1, seek/3, can_open_file/1, write_frame/2]).

-record(media_info, {
  reader,
  offset = 0,
  version,
  header_size = 0,
  format,
  duration
}).


can_open_file(Name) when is_binary(Name) ->
  can_open_file(binary_to_list(Name));

can_open_file(Name) ->
  filename:extension(Name) == ".mp3".


write_frame(_Device, _Frame) -> 
  erlang:error(unsupported).




%%--------------------------------------------------------------------
%% @spec (IoDev::iodev()) -> {ok, IoSize::integer(), Header::header()} | {error,Reason::atom()}
%% @doc Read flv file and load its frames in memory ETS
%% @end 
%%--------------------------------------------------------------------
init(Reader) ->
  {ok, read_header(#media_info{reader = Reader})}.


read_header(#media_info{reader = {Module,Device}} = Media) -> 
  case Module:pread(Device, 0, 10) of
    {ok, <<"ID3", Major, Minor, Unsync:1, Extended:1, _Experimental:1, Footer:1, 0:4, 
           _:1, S1:7, _:1, S2:7, _:1, S3:7, _:1, S4:7>>} ->
      <<Size:28>> = <<S1:7, S2:7, S3:7, S4:7>>,
      Media1 = Media#media_info{format = id3, version = {Major, Minor}},
      Offset = sync(<<>>, Media1, Size + 10),
      Media1#media_info{header_size = Offset};
    {ok, <<"ID3", _/binary>>} ->
      ?D({id3,unsupported}),
      erlang:error(unknown_mp3);
    {ok, <<2#11111111111:11, _:5, _/binary>>} ->
      Media#media_info{format = raw, header_size = 0};
    {ok, Binary} ->
      Offset = sync(Binary, Media, 0),
      Media#media_info{header_size = Offset}
  end.  


sync(<<2#11111111111:11, _:5, _/binary>>, Media, Offset) ->
  Offset;
  
sync(<<_, Binary/binary>>, Media, Offset) ->
  sync(Binary, Media, Offset + 1);
  
sync(<<>>, #media_info{reader = {Module, Device}} = Media, Offset) ->
  case Module:pread(Device, Offset, 256) of
    {ok, Block} ->
      sync(Block, Media, Offset);
    eof ->
      eof
  end.
    

first(#media_info{header_size = Size}) ->
  {Size,0}.

properties(#media_info{}) -> [].


seek(#media_info{}, _BeforeAfter, _Timestamp) ->
  undefined.


% Reads a tag from IoDev for position Pos.
% @param IoDev
% @param Pos
% @return a valid video_frame record type

read_frame(Media, undefined) ->
  read_frame(Media, first(Media));

read_frame(Media, {eof, _N}) ->
  eof;

read_frame(#media_info{reader = {Module,Device}} = Media, {Offset, N}) ->
  case Module:pread(Device, Offset, mp3:header_size()) of
    eof -> eof;
    {ok, <<2#11111111111:11, _:5, _/binary>> = Header} ->
      DTS = N*1000*1024 div 44100,
      Length = mp3:frame_length(Header),
      {ok, Frame} = Module:pread(Device, Offset, Length),
      #video_frame{
        content  = audio,
        dts      = DTS,
        pts      = DTS,
        codec    = mp3,
        flavor   = frame,
        sound    = {stereo, bit16, rate44},
        body     = Frame,
        next_id  = {Offset+Length, N+1}
      };
    {ok, Binary} ->
      Offset1 = sync(Binary, Media, Offset),
      read_frame(Media, {Offset1,N})
  end.

