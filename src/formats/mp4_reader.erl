-module(mp4_reader).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(gen_format).
-include("../../include/media_info.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/mp4.hrl").
-include("../../include/ems.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


-export([build_index_table/1, read_header/1]).

-export([init/1, read_frame/2, metadata/1, codec_config/2, seek/3, first/1]).


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


first(_) ->
  0.


lookup_frame(video, #media_info{video_track = FrameTable}) -> FrameTable;
lookup_frame(audio, #media_info{audio_track = FrameTable}) -> FrameTable.

read_frame(#media_info{frames = Frames} = MediaInfo, Id) ->
  [{Id, Type, FrameId}] = ets:lookup(Frames, Id),

  FrameTable = lookup_frame(Type, MediaInfo),
  Frame = mp4:read_frame(FrameTable, FrameId),
  #mp4_frame{offset = Offset, size = Size} = Frame,
  Next = case ets:next(Frames, Id) of
    '$end_of_table' -> done;
    NextId -> NextId
  end,
  
	case read_data(MediaInfo, Offset, Size) of
		{ok, Data, _} -> {video_frame(Type, Frame, Data), Next};
    eof -> done;
    {error, Reason} -> {error, Reason}
  end.
  

read_data(#media_info{device = IoDev} = MediaInfo, Offset, Size) ->
  case file:pread(IoDev, Offset, Size) of
    {ok, Data} ->
      {ok, Data, MediaInfo};
    Else -> Else
  end.
  
seek(#media_info{video_track = FrameTable, frames = Frames}, before, Timestamp) ->
  Ids = ets:select(FrameTable, ets:fun2ms(fun(#mp4_frame{id = Id, dts = FrameTimestamp, keyframe = true} = _Frame) when FrameTimestamp =< Timestamp ->
    {Id, FrameTimestamp}
  end)),
  case lists:reverse(Ids) of
    [{VideoID, NewTimestamp} | _] ->
      [Item] = ets:select(Frames, ets:fun2ms(fun({ID, video, VideoFrameID}) when VideoID == VideoFrameID -> 
        {ID, NewTimestamp}
      end)),
      Item;
    _ -> undefined
  end;

seek(#media_info{video_track = FrameTable, frames = Frames}, 'after', Timestamp) ->
  Ids = ets:select(FrameTable, ets:fun2ms(fun(#mp4_frame{id = Id, dts = FrameTimestamp, keyframe = true} = _Frame) when FrameTimestamp >= Timestamp ->
    {Id, FrameTimestamp}
  end)),
  case Ids of
    [{VideoID, NewTimestamp} | _] ->
      [Item] = ets:select(Frames, ets:fun2ms(fun({ID, video, VideoFrameID}) when VideoID == VideoFrameID -> 
        {ID, NewTimestamp}
      end)),
      Item;
    _ -> undefined
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
  #mp4_media{width = Width, height = Height, audio_config = AC, video_config = VC, 
             audio_track = AT, video_track = VT, seconds = Seconds} = Mp4Media,
  Info1 = MediaInfo#media_info{header = Mp4Media, width = Width, height = Height,            
                       audio_config = AC, video_config = VC, 
                       audio_track = AT, video_track = VT, duration = Seconds},
  {ok, Info1}.



build_index_table(#media_info{video_track = Video, audio_track = Audio} = MediaInfo) ->
  VideoCount = mp4:frame_count(Video),
  AudioCount = mp4:frame_count(Audio),
  Index = ets:new(index_table, [ordered_set]),
  build_index_table(Video, 0, VideoCount, Audio, 0, AudioCount, Index, 0),
  {ok, MediaInfo#media_info{frames = Index}}.


build_index_table(_Video, VC, VC, _Audio, AC, AC, Index, _ID) ->
  Index;

build_index_table(Video, VC, VC, Audio, AudioID, AudioCount, Index, ID) ->
  ets:insert(Index, {ID, audio, AudioID}),
  build_index_table(Video, VC, VC, Audio, AudioID+1, AudioCount, Index, ID+1);

build_index_table(Video, VideoID, VideoCount, Audio, AC, AC, Index, ID) ->
  ets:insert(Index, {ID, video, VideoID}),
  build_index_table(Video, VideoID + 1, VideoCount, Audio, AC, AC, Index, ID+1);


build_index_table(Video, VideoID, VideoCount, Audio, AudioID, AudioCount, Index, ID) ->
  AFrame = mp4:read_frame(Audio, AudioID),
  VFrame = mp4:read_frame(Video, VideoID),
  case {VFrame#mp4_frame.dts, AFrame#mp4_frame.dts} of
    {VDTS, ADTS} when VDTS < ADTS ->
      ets:insert(Index, {ID, video, VideoID}),
      build_index_table(Video, VideoID + 1, VideoCount, Audio, AudioID, AudioCount, Index, ID+1);
    {_VDTS, _ADTS} ->
      ets:insert(Index, {ID, audio, AudioID}),
      build_index_table(Video, VideoID, VideoCount, Audio, AudioID + 1, AudioCount, Index, ID+1)
  end.


metadata(#media_info{width = Width, height = Height, duration = Duration}) -> 
  [{width, Width}, 
   {height, Height}, 
   {duration, Duration/1000}].


decoder_config(video, #media_info{video_config = DecoderConfig}) -> DecoderConfig;
decoder_config(audio, #media_info{audio_config = DecoderConfig}) -> DecoderConfig.
