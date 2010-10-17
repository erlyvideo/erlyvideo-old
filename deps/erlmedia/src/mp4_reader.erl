%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        ISO Media mp4 reader
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
-module(mp4_reader).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(gen_format).
-include("../include/video_frame.hrl").
-include("../include/mp4.hrl").
-include("log.hrl").


-export([init/2, read_frame/2, properties/1, seek/3, can_open_file/1, write_frame/2]).
-export([track_for_bitrate/2, track_for_language/2]).

-define(FRAMESIZE, 8).

can_open_file(Name) when is_binary(Name) ->
  can_open_file(binary_to_list(Name));

can_open_file(Name) ->
  lists:member(filename:extension(Name), [".mp4", ".f4v", ".mov"]).

write_frame(_Device, _Frame) -> 
  erlang:error(unsupported).




init(Reader, Options) -> 
  {ok, MP4Media} = mp4:open(Reader),
  Tracks = tuple_to_list(MP4Media#mp4_media.tracks),

  Bitrates = [Bitrate || #mp4_track{bitrate = Bitrate, content = Content} <- Tracks, Content == video],
  Languages = [Lang || #mp4_track{language = Lang, content = Content} <- Tracks, Content == audio],
  ?D({"Opened mp4. Bitrates:", Bitrates, "Langs:", Languages}),
  
  ?D({"ZZ", [Track#mp4_track{frames = frames} || Track <- Tracks]}),

  {ok, MP4Media#mp4_media{options = Options}}.



% track_by_language([], _) -> {undefined, 0};
% track_by_language([Track|_], undefined) -> {Track, mp4:frame_count(Track)};
% track_by_language([Track|_] = Tracks, Language) -> track_by_language(Tracks, Language, Track).
% 
% track_by_language([#mp4_track{language = Lang} = Track|_], Lang, _Default) -> ?D({"Selected track", Lang}), {Track, mp4:frame_count(Track)};
% track_by_language([_|Tracks], Lang, Default) -> track_by_language(Tracks, Lang, Default);
% track_by_language([], _Lang, Default) -> {Default, mp4:frame_count(Default)}.
% 



properties(#mp4_media{additional = Additional, width = Width, height = Height, duration = Duration} = MP4Media) -> 
  Tracks = tuple_to_list(MP4Media#mp4_media.tracks),
  Bitrates = [Bitrate || #mp4_track{bitrate = Bitrate, content = Content} <- Tracks, Content == video],
  Languages = [list_to_binary(Language) || #mp4_track{language = Language, content = Content} <- Tracks, Content == audio],
  [{width, Width}, 
   {height, Height},
   {type, file},
   {duration, Duration},
   {bitrates, Bitrates},
   {languages, Languages}] ++ Additional.



track_for_bitrate(#mp4_media{tracks = Tracks}, Bitrate) ->
  find_track(Tracks, #mp4_track.bitrate, Bitrate, video).

track_for_language(#mp4_media{tracks = Tracks}, Language) ->
  find_track(Tracks, #mp4_track.language, Language, audio).

find_track(Tracks, Pos, Value, Content) ->
  find_track(Tracks, Pos, Value, 1, Content, undefined).
  
find_track(Tracks, _Pos, _Value, Index, _Content, Default) when Index > size(Tracks) ->
  Default;
  
find_track(Tracks, Pos, Value, Index, _Content, _Default) when element(Pos,element(Index,Tracks)) == Value ->
  Index;

find_track(Tracks, Pos, Value, Index, Content, _Default) when (element(Index,Tracks))#mp4_track.content == Content ->
  find_track(Tracks, Pos, Value, Index+1, Content, Index);

find_track(Tracks, Pos, Value, Index, Content, Default) ->
  find_track(Tracks, Pos, Value, Index + 1, Content, Default).


first(Media) ->
  first(Media, 0, 0).

first(#mp4_media{} = Media, Id, DTS) when is_number(Id) ->
  Video = track_for_bitrate(Media, undefined),
  Audio = track_for_language(Media, undefined),
  first(Media, {Id,Audio,Video}, DTS);

first(#mp4_media{tracks = Tracks}, {_Id,Audio,Video} = Id, DTS) ->
  AudioConfig = (element(Audio,Tracks))#mp4_track.decoder_config,
  VideoConfig = (element(Video,Tracks))#mp4_track.decoder_config,

  case {AudioConfig, VideoConfig} of
    {undefined,undefined} -> Id;
    {undefined,_} -> {video_config,Id,DTS};
    {_,_} -> {audio_config,Id,DTS}
  end.




codec_config({video,TrackID}, #mp4_media{tracks = Tracks}) ->
  #mp4_track{data_format = Codec, decoder_config = Config} = element(TrackID, Tracks),
  #video_frame{
   	content = video,
   	flavor  = config,
		dts     = 0,
		pts     = 0,
		body    = Config,
		codec   = Codec
	};

codec_config({audio,TrackID}, #mp4_media{tracks = Tracks}) ->
  #mp4_track{data_format = Codec, decoder_config = Config} = element(TrackID, Tracks),
  #video_frame{       
   	content = audio,
   	flavor  = config,
		dts     = 0,
		pts     = 0,
		body    = Config,
	  codec	  = Codec,
	  sound   = {stereo, bit16, rate44}
	}.



read_frame(MediaInfo, undefined) ->
  read_frame(MediaInfo, first(MediaInfo));

read_frame(#mp4_media{tracks = Tracks} = Media, {audio_config, {_Id,Audio,Video} = Pos, DTS}) ->
  Frame = codec_config({audio,Audio}, Media),
  Next = case (element(Video,Tracks))#mp4_track.decoder_config of
    undefined -> Pos;
    _ -> {video_config,Pos, DTS}
  end,
  Frame#video_frame{next_id = Next, dts = DTS, pts = DTS};

read_frame(MediaInfo, {video_config, {_Id,_Audio,Video} = Pos, DTS}) ->
  Frame = codec_config({video,Video}, MediaInfo),
  Frame#video_frame{next_id = Pos, dts = DTS, pts = DTS};

read_frame(_, eof) ->
  eof;

read_frame(#mp4_media{} = Media, Id) ->
  #mp4_frame{offset = Offset, size = Size, content = Content, next_id = Next} = Frame = mp4:read_frame(Media, Id),

	case read_data(Media, Offset, Size) of
		{ok, Data, _} ->
		  VideoFrame = video_frame(Content, Frame, Data),
		  VideoFrame#video_frame{next_id = Next};
    eof -> eof;
    {error, Reason} -> {error, Reason}
  end.
  

read_data(#mp4_media{reader = {M, Dev}} = Media, Offset, Size) ->
  case M:pread(Dev, Offset, Size) of
    {ok, Data} ->
      {ok, Data, Media};
    Else -> Else
  end.
  

video_frame(video, #mp4_frame{dts = DTS, keyframe = Keyframe, pts = PTS, codec = Codec}, Data) ->
  #video_frame{
   	content = video,
		dts     = DTS,
		pts     = PTS,
		body    = Data,
		flavor  = case Keyframe of
		  true ->	keyframe;
		  _ -> frame
	  end,
		codec   = Codec
  };  

video_frame(audio, #mp4_frame{dts = DTS, codec = Codec}, Data) ->
  #video_frame{       
   	content = audio,
		dts     = DTS,
		pts     = DTS,
  	body    = Data,
  	flavor  = frame,
	  codec	  = Codec,
	  sound	  = {stereo, bit16, rate44}
  }.



seek(#mp4_media{} = Media, before, Timestamp) when Timestamp == 0 ->
  {first(Media), 0};

seek(#mp4_media{duration = Duration}, _, Timestamp) when Timestamp > Duration ->
  undefined;

seek(#mp4_media{} = Media, before, Timestamp) ->
  Video = track_for_bitrate(Media, undefined),
  Audio = track_for_language(Media, undefined),
  case mp4:seek(Media, Video, Timestamp) of
    {Id, DTS} -> {{Id,Audio,Video}, DTS};
    undefined -> undefined
  end.

