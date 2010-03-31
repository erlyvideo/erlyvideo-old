-module(mp4_reader).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(gen_format).
-include_lib("erlyvideo/include/media_info.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include("../../include/ems.hrl").
-include("../../include/mp4.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


-export([build_index_table/1, read_header/1]).

-export([init/1, read_frame/2, metadata/1, codec_config/2, seek/2, first/1]).


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


first(#media_info{frames = Frames}) ->
  ets:first(Frames).


lookup_frame(video, #media_info{video_track = FrameTable}, Id) ->
  [Frame] = ets:lookup(FrameTable, Id),
  Frame;

lookup_frame(audio, #media_info{audio_track = FrameTable}, Id) ->
  [Frame] = ets:lookup(FrameTable, Id),
  Frame.

read_frame(#media_info{frames = Frames} = MediaInfo, Id) ->
  [{Id, Type, FrameId}] = ets:lookup(Frames, Id),
  Frame = lookup_frame(Type, MediaInfo, FrameId),
  #mp4_frame{offset = Offset, size = Size} = Frame,
  Next = case ets:next(Frames, Id) of
    '$end_of_table' -> done;
    NextID -> NextID
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
  
seek(#media_info{video_track = FrameTable, frames = Frames}, Timestamp) ->
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
  {Time2, Info3} = timer:tc(?MODULE, build_index_table, [Info2]),
  ?D({"Time to parse moov and build index", round(Time/1000), round(Time2/1000), Info2#media_info.seconds}),
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
                       audio_track = AT, video_track = VT, seconds = Seconds},
  {ok, Info1}.



build_index_table(#media_info{video_track = Video, audio_track = Audio} = MediaInfo) ->
  Index = ets:new(index, [ordered_set]),
  build_index_table(Video, ets:first(Video), Audio, ets:first(Audio), Index, 0),
  MediaInfo#media_info{frames = Index}.


build_index_table(_Video, '$end_of_table', _Audio, '$end_of_table', Index, _ID) ->
  Index;

build_index_table(Video, '$end_of_table', Audio, AudioID, Index, ID) ->
  [AFrame] = ets:lookup(Audio, AudioID),
  ets:insert(Index, {ID, audio, AFrame#mp4_frame.id}),
  build_index_table(Video, '$end_of_table', Audio, ets:next(Audio, AudioID), Index, ID+1);

build_index_table(Video, VideoID, Audio, '$end_of_table', Index, ID) ->
  [VFrame] = ets:lookup(Video, VideoID),
  ets:insert(Index, {ID, video, VFrame#mp4_frame.id}),
  build_index_table(Video, ets:next(Video, VideoID), Audio, '$end_of_table', Index, ID+1);


build_index_table(Video, VideoID, Audio, AudioID, Index, ID) ->
  [VFrame] = ets:lookup(Video, VideoID),
  [AFrame] = ets:lookup(Audio, AudioID),
  case {VFrame#mp4_frame.dts, AFrame#mp4_frame.dts} of
    {VDTS, ADTS} when VDTS =< ADTS ->
      ets:insert(Index, {ID, video, VFrame#mp4_frame.id}),
      build_index_table(Video, ets:next(Video, VideoID), Audio, AudioID, Index, ID+1);
    _ ->
      ets:insert(Index, {ID, audio, AFrame#mp4_frame.id}),
      build_index_table(Video, VideoID, Audio, ets:next(Audio, AudioID), Index, ID+1)
  end.


metadata(#media_info{width = Width, height = Height, seconds = Duration}) -> 
  [{width, Width}, 
   {height, Height}, 
   {duration, Duration/1000}].


decoder_config(video, #media_info{video_config = DecoderConfig}) -> DecoderConfig;
decoder_config(audio, #media_info{audio_config = DecoderConfig}) -> DecoderConfig.
