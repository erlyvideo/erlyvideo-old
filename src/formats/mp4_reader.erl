-module(mp4_reader).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(gen_format).
-include("../../include/media_info.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/mp4.hrl").
-include("../../include/ems.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


-export([build_index_table/1, read_header/1]).

-export([init/1, read_frame/2, metadata/1, seek/3, first/1]).

-define(FRAMESIZE, 8).

codec_config(video, #media_info{video_codec = VideoCodec} = MediaInfo) ->
  Config = decoder_config(video, MediaInfo),
  % ?D({"Video config", Config}),
  #video_frame{       
   	type          = video,
   	decoder_config = true,
		dts           = 0,
		pts           = 0,
		body          = Config,
		frame_type    = keyframe,
		codec_id      = VideoCodec
	};

codec_config(audio, #media_info{audio_codec = AudioCodec} = MediaInfo) ->
  Config = decoder_config(audio, MediaInfo),
  % ?D({"Audio config", aac:decode_config(Config)}),
  #video_frame{       
   	type          = audio,
   	decoder_config = true,
		dts           = 0,
		pts           = 0,
		body          = Config,
	  codec_id	= AudioCodec,
	  sound_type	  = stereo,
	  sound_size	  = bit16,
	  sound_rate	  = rate44
	}.


first(Media) ->
  first(Media, 0).

first(#media_info{audio_config = A}, Id) when A =/= undefined ->
  {audio_config, Id};

first(#media_info{video_config = V}, Id) when V =/= undefined ->
  {video_config, Id};

first(_, Id) ->
  Id.


lookup_frame(video, #media_info{video_track = VTs}) -> element(1,VTs);
lookup_frame(audio, #media_info{audio_track = ATs}) -> element(1,ATs).


read_frame(MediaInfo, {audio_config, Pos}) ->
  ?D({"Send audio config", Pos}),
  Frame = codec_config(audio, MediaInfo),
  Frame#video_frame{next_id = {video_config,Pos}};

read_frame(MediaInfo, {video_config,Pos}) ->
  ?D({"Send video config", Pos}),
  Frame = codec_config(video, MediaInfo),
  Frame#video_frame{next_id = Pos};

read_frame(_, eof) ->
  eof;

read_frame(#media_info{frames = Frames} = MediaInfo, Id) when Id*?FRAMESIZE < size(Frames)->
  % [{Id, Type, FrameId}] = ets:lookup(Frames, Id),
  FrameOffset = Id*?FRAMESIZE,
  <<_:FrameOffset/binary, Id:32, BinType:1, FrameId:31, _/binary>> = Frames,
  Type = case BinType of
    1 -> video;
    0 -> audio
  end,

  FrameTable = lookup_frame(Type, MediaInfo),
  Frame = mp4:read_frame(FrameTable, FrameId),
  #mp4_frame{offset = Offset, size = Size} = Frame,
  % Next = case ets:next(Frames, Id) of
  %   '$end_of_table' -> eof;
  %   NextId -> NextId
  % end,
  Next = if
    (Id+1)*?FRAMESIZE == size(Frames) -> eof;
    true -> Id + 1
  end,
    
  
	case read_data(MediaInfo, Offset, Size) of
		{ok, Data, _} -> 
		  VideoFrame = video_frame(Type, Frame, Data),
		  VideoFrame#video_frame{next_id = Next};
    eof -> eof;
    {error, Reason} -> {error, Reason}
  end.
  

read_data(#media_info{device = IoDev} = MediaInfo, Offset, Size) ->
  case file:pread(IoDev, Offset, Size) of
    {ok, Data} ->
      {ok, Data, MediaInfo};
    Else -> Else
  end.
  

seek(#media_info{} = Media, before, Timestamp) when Timestamp == 0 ->
  {first(Media), 0};
  
seek(#media_info{video_track = VTs, frames = Frames} = Media, Direction, Timestamp) ->
  FrameTable = element(1,VTs),
  case mp4:seek(FrameTable, Direction, Timestamp) of
    {VideoID, NewTimestamp} ->
      ID = find_by_frameid(Frames, video, VideoID),
      {first(Media, ID),NewTimestamp};
    undefined ->
      undefined
  end.


find_by_frameid(Frames, video, VideoID) ->
  find_by_frameid(Frames, 1, VideoID);
  
find_by_frameid(Frames, Type, FrameID) ->
  case Frames of
    <<ID:32, Type:1, FrameID:31, _/binary>> -> ID;
    <<_:64, Rest/binary>> -> find_by_frameid(Rest, Type, FrameID);
    <<>> -> undefined
  end.

  
  

video_frame(video, #mp4_frame{dts = DTS, keyframe = Keyframe, pts = PTS}, Data) ->
  #video_frame{
   	type          = video,
		dts           = DTS,
		pts           = PTS,
		body          = Data,
		frame_type    = case Keyframe of
		  true ->	keyframe;
		  _ -> frame
	  end,
		codec_id      = h264
  };  

video_frame(audio, #mp4_frame{dts = DTS}, Data) ->
  #video_frame{       
   	type          = audio,
		dts           = DTS,
		pts           = DTS,
  	body          = Data,
	  codec_id	    = aac,
	  sound_type	  = stereo,
	  sound_size	  = bit16,
	  sound_rate	  = rate44
  }.



init(#media_info{header = undefined} = MediaInfo) -> 
  Info1 = MediaInfo,
  ?D("Going to read header"),
  % eprof:start(),
  % eprof:start_profiling([self()]),
  {Time, {ok, Info2}} = timer:tc(?MODULE, read_header, [Info1]),
  {Time2, {ok, Info3}} = timer:tc(?MODULE, build_index_table, [Info2]),
  ?D({"Time to parse moov and build index", round(Time/1000), round(Time2/1000), Info2#media_info.duration}),
  % eprof:total_analyse(),
  % eprof:stop(),
  {ok, Info3}.

read_header(#media_info{device = Device} = MediaInfo) -> 
  Reader = {file, Device},
  {ok, Mp4Media} = mp4:read_header(Reader),
  #mp4_media{width = Width, height = Height, audio_tracks = ATs, video_tracks = VTs, seconds = Seconds} = Mp4Media,
  ?D({"Opened mp4 file with following video tracks:", [Bitrate || #mp4_track{bitrate = Bitrate} <- VTs], "and audio tracks", [Language || #mp4_track{language = Language} <- ATs]}),
  AT = hd(ATs),
  VT = hd(VTs),
  #mp4_track{decoder_config = AC} = AT,
  #mp4_track{decoder_config = VC} = VT,
  Info1 = MediaInfo#media_info{header = Mp4Media, width = Width, height = Height,            
                       audio_config = AC, video_config = VC, 
                       audio_track = list_to_tuple(ATs), video_track = list_to_tuple(VTs), duration = Seconds},
  {ok, Info1}.



build_index_table(#media_info{video_track = VTs, audio_track = ATs} = MediaInfo) ->
  Video = element(1, VTs),
  Audio = element(1, ATs),
  VideoCount = mp4:frame_count(Video),
  AudioCount = mp4:frame_count(Audio),
  % Index = ets:new(index_table, [ordered_set]),
  Index = <<>>,
  BuiltIndex = build_index_table(Video, 0, VideoCount, Audio, 0, AudioCount, Index, 0),
  {ok, MediaInfo#media_info{frames = BuiltIndex}}.


build_index_table(_Video, VC, VC, _Audio, AC, AC, Index, _ID) ->
  Index;

build_index_table(Video, VC, VC, Audio, AudioID, AudioCount, Index, ID) ->
  % ets:insert(Index, {ID, audio, AudioID}),
  build_index_table(Video, VC, VC, Audio, AudioID+1, AudioCount, <<Index/binary, ID:32, 0:1, AudioID:31>>, ID+1);

build_index_table(Video, VideoID, VideoCount, Audio, AC, AC, Index, ID) ->
  % ets:insert(Index, {ID, video, VideoID}),
  build_index_table(Video, VideoID + 1, VideoCount, Audio, AC, AC, <<Index/binary, ID:32, 1:1, VideoID:31>>, ID+1);


build_index_table(Video, VideoID, VideoCount, Audio, AudioID, AudioCount, Index, ID) ->
  AFrame = mp4:read_frame(Audio, AudioID),
  VFrame = mp4:read_frame(Video, VideoID),
  case {VFrame#mp4_frame.dts, AFrame#mp4_frame.dts} of
    {VDTS, ADTS} when VDTS < ADTS ->
      % ets:insert(Index, {ID, video, VideoID}),
      build_index_table(Video, VideoID + 1, VideoCount, Audio, AudioID, AudioCount, <<Index/binary, ID:32, 1:1, VideoID:31>>, ID+1);
    {_VDTS, _ADTS} ->
      % ets:insert(Index, {ID, audio, AudioID}),
      build_index_table(Video, VideoID, VideoCount, Audio, AudioID + 1, AudioCount, <<Index/binary, ID:32, 0:1, AudioID:31>>, ID+1)
  end.


metadata(#media_info{width = Width, height = Height, duration = Duration, audio_track = ATs, video_track = VTs}) -> 
  Bitrates = [Bitrate || #mp4_track{bitrate = Bitrate} <- tuple_to_list(VTs)],
  Languages = [list_to_binary(Language) || #mp4_track{language = Language} <- tuple_to_list(ATs)],
  [{width, Width}, 
   {height, Height}, 
   {duration, Duration/1000},
   {bitrates, Bitrates},
   {languages, Languages}].


decoder_config(video, #media_info{video_config = DecoderConfig}) -> DecoderConfig;
decoder_config(audio, #media_info{audio_config = DecoderConfig}) -> DecoderConfig.
