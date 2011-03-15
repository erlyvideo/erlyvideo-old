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
-include("../include/video_frame.hrl").
-include("../include/media_info.hrl").
-include("../include/mp3.hrl").
-include("log.hrl").
-include_lib("kernel/include/file.hrl").


-behaviour(gen_format).
-export([init/2, read_frame/2, media_info/1, properties/1, seek/3, can_open_file/1, write_frame/2]).

-record(mp3_media, {
  reader,
  offset = 0,
  version,
  header_size = 0,
  format,
  duration,
  header,
  path,
  channels,
  sample_rate,
  samples
}).


can_open_file(Name) when is_binary(Name) ->
  can_open_file(binary_to_list(Name));

can_open_file(Name) ->
  filename:extension(Name) == ".mp3".


write_frame(_Device, _Frame) -> 
  erlang:error(unsupported).




%%--------------------------------------------------------------------
%% @spec (IoDev::iodev(), Options) -> {ok, State} | {error,Reason::atom()}
%% @doc Read flv file and load its frames in memory ETS
%% @end 
%%--------------------------------------------------------------------
init(Reader, Options) ->
  {ok, read_header(#mp3_media{reader = Reader, path = proplists:get_value(url, Options)})}.


read_header(#mp3_media{reader = {Module,Device}} = Media) -> 
  case Module:pread(Device, 0, 10) of
    {ok, <<"ID3", Major, Minor, _Unsync:1, _Extended:1, _Experimental:1, _Footer:1, 0:4, 
           _:1, S1:7, _:1, S2:7, _:1, S3:7, _:1, S4:7>>} ->
      <<Size:28>> = <<S1:7, S2:7, S3:7, S4:7>>,
      Media1 = Media#mp3_media{format = id3, version = {Major, Minor}},
      Offset = sync(<<>>, Media1, Size + 10),
      read_properties(Media1#mp3_media{header_size = Offset});
    {ok, <<"ID3", _/binary>>} ->
      ?D({id3,unsupported}),
      erlang:error(unknown_mp3);
    {ok, <<2#11111111111:11, _:5, _/binary>>} ->
      read_properties(Media#mp3_media{format = raw, header_size = 0});
    {ok, Binary} ->
      Offset = sync(Binary, Media, 0),
      read_properties(Media#mp3_media{header_size = Offset})
  end.  


sync(<<2#11111111111:11, _:5, _/binary>>, _Media, Offset) ->
  Offset;
  
sync(<<_, Binary/binary>>, Media, Offset) ->
  sync(Binary, Media, Offset + 1);
  
sync(<<>>, #mp3_media{reader = {Module, Device}} = Media, Offset) ->
  case Module:pread(Device, Offset, 256) of
    {ok, Block} ->
      sync(Block, Media, Offset);
    eof ->
      eof
  end.
    
read_properties(#mp3_media{reader = {Module,Device}, header_size = Offset, path = Path} = Media) ->
  {ok, Header} = Module:pread(Device, Offset, mp3:header_size()),
  Length = mp3:frame_length(Header),
  {ok, Body} = Module:pread(Device, Offset, Length),
  {ok, MP3, <<>>} = mp3:read(Body),
  #mp3_frame{samples = Samples, channels = Channels, sample_rate = SampleRate} = MP3,
  Duration = case Module of
    file when is_list(Path) orelse is_binary(Path) -> 
      case filelib:is_file(Path) of
        true ->
          {ok, FileInfo} = file:read_file_info(Path),
          N = FileInfo#file_info.size div Length,
          N*1000*MP3#mp3_frame.samples div MP3#mp3_frame.sample_rate;
        false ->
          undefined
      end;
    _ ->
      undefined
  end,
  Media#mp3_media{header = MP3#mp3_frame{body = body}, samples = Samples, duration = Duration, sample_rate = SampleRate, channels = Channels}.



media_info(#mp3_media{duration = Duration} = Media) ->
  AudioStream = #stream_info{
    content = audio,
    stream_id = 1,
    codec = mp3,
    config = undefined,
    params = #audio_params{channels = Media#mp3_media.channels, sample_rate = Media#mp3_media.sample_rate}
  },
  
  #media_info{
    flow_type = file,
    audio = [AudioStream],
    video = [],
    metadata = [],
    duration = Duration
  }.
  

first(#mp3_media{header_size = Size}) ->
  {Size,0}.
  
  
duration(Media) ->
  duration(Media, first(Media), 0).

duration(Media, Key, DTS) ->
  case read_frame(Media, Key) of
    eof -> DTS;
    #video_frame{next_id = NextKey, dts = NextDTS} -> duration(Media, NextKey, NextDTS)
  end.

properties(#mp3_media{duration = undefined} = Media) -> properties(Media#mp3_media{duration = duration(Media)});
properties(#mp3_media{duration = Duration}) -> [{duration, Duration}].


seek(#mp3_media{} = Media, undefined, _Options) ->
  {first(Media), 0};

seek(#mp3_media{} = Media, Timestamp, _Options) ->
  ?D({"mp3 seek", Timestamp}),
  find_frame(Media, Timestamp, first(Media), undefined).

find_frame(Media, Max, Key, Retval) ->
  case read_frame(Media, Key) of
    eof -> 
      Retval;
    #video_frame{dts = DTS} when DTS > Max ->
      Retval;
    #video_frame{dts = DTS, next_id = NextKey} ->
      find_frame(Media, Max, NextKey, {Key, DTS})
  end.
      
  


% Reads a tag from IoDev for position Pos.
% @param IoDev
% @param Pos
% @return a valid video_frame record type

read_frame(Media, undefined) ->
  read_frame(Media, first(Media));

read_frame(_Media, {eof, _N}) ->
  eof;

read_frame(#mp3_media{reader = {Module,Device}} = Media, {Offset, N}) ->
  case Module:pread(Device, Offset, mp3:header_size()) of
    eof -> eof;
    {ok, <<2#11111111111:11, _:5, _/binary>> = Header} ->
      Length = mp3:frame_length(Header),
      {ok, Body} = Module:pread(Device, Offset, Length),
      {ok, MP3, <<>>} = mp3:read(Body),
      DTS = N*1000*MP3#mp3_frame.samples div MP3#mp3_frame.sample_rate,
      % MP3 = mp3:read(Frame),
      #video_frame{
        content  = audio,
        dts      = DTS,
        pts      = DTS,
        codec    = mp3,
        flavor   = frame,
        sound    = {stereo, bit16, rate44},
        body     = Body,
        next_id  = {Offset+Length, N+1}
      };
    {ok, Binary} ->
      Offset1 = sync(Binary, Media, Offset),
      read_frame(Media, {Offset1,N})
  end.

