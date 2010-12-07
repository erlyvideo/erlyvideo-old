-module(mp4_writer).
-author('Max Lapshin <max@maxidoors.ru>').
-include_lib("../include/flv.hrl").
-include_lib("../include/mp4.hrl").
-include_lib("../include/video_frame.hrl").
-include("log.hrl").

-export([write/2, write/3, pack_language/1]).
-export([init/2, handle_frame/2, close/1]).

-export([pack_compositions/1]).

-record(convertor, {
  options = [],
  out_mp4,
  out_mp4_path,
  url,
  write_offset,
  mdat_offset,
  duration,

  audio_frames = [],
  audio_config,

  video_frames = [],
  video_config
}).

-undef(D).
-define(D(X), io:format("~p:~p ~p~n", [?MODULE, ?LINE, X])).

-define(H264_SCALE, 24).
-define(AUDIO_SCALE, 44.1).


write(InFlvPath, OutMp4Path) ->
  write(InFlvPath, OutMp4Path, []).

write(InFlvPath, OutMp4Path, Options) ->
  {ok, InFlv} = file:open(InFlvPath, [read, binary, {read_ahead, 100000}, raw]),
  {#flv_header{}, ReadOffset} = flv:read_header(InFlv),
  Reader = fun(Key) ->
    flv:read_frame(InFlv, Key)
  end,
  {ok, Convertor} = init(OutMp4Path, Options),
  Convertor1 = write_mp4(Reader, ReadOffset, Convertor),
  close(Convertor1),
  file:close(InFlv),
  ok.


write_mp4(Reader, ReadOffset, Convertor) ->
  case handle_frame(Reader(ReadOffset), Convertor) of
    {ok, NextOffset, Convertor1} ->
      write_mp4(Reader, NextOffset, Convertor1);
    {ok, Convertor1} ->
      Convertor1
  end.
      


init(OutMp4Path, Options) ->
  {ok, OutMp4} = file:open(OutMp4Path, [write, binary, raw]),
  URL = proplists:get_value(url, Options, list_to_binary(OutMp4Path)),

  Mp4Header = mp4_header(),
  {ok, MdatOffset} = mp4_write(OutMp4, Mp4Header ++ proplists:get_value(mp4, Options, []), 0),
  ?D({add_mp4, proplists:get_value(mp4, Options, [])}),
  % ?D({mdat_offset, MdatOffset}),
  {ok, WriteOffset} = mp4_write(OutMp4, {mdat, <<>>}, MdatOffset),
  % ?D({write_offset, WriteOffset}),
  
  {ok, #convertor{out_mp4_path = OutMp4Path, 
             options = Options,
             write_offset = WriteOffset,
             mdat_offset = MdatOffset,
             out_mp4 = OutMp4,
             url = URL}}.
  
close(#convertor{out_mp4 = OutMp4}) ->
  file:close(OutMp4).
  



  
    
handle_frame(#video_frame{next_id = NextOffset, flavor = command}, Convertor) ->
  {ok, NextOffset, Convertor};
    
handle_frame(#video_frame{next_id = NextOffset, flavor = config, content = video, body = Config}, Convertor) ->
  ?D("mp4_writer got video config"),
  {ok, NextOffset, Convertor#convertor{video_config = Config}};

handle_frame(#video_frame{next_id = NextOffset, flavor = config, content = audio, body = Config}, Convertor) ->
  ?D("mp4_writer got audio config"),
  {ok, NextOffset, Convertor#convertor{audio_config = Config}};

handle_frame(#video_frame{next_id = NextOffset, body = Body, content = video} = Frame, 
             #convertor{write_offset = WriteOffset, out_mp4 = OutMp4, video_frames = Video} = Convertor) ->     
  ok = file:pwrite(OutMp4, WriteOffset, Body),
  {ok, NextOffset, Convertor#convertor{write_offset = WriteOffset + size(Body), 
                                 video_frames = [Frame#video_frame{body = {WriteOffset,size(Body)}}|Video]}};

handle_frame(#video_frame{next_id = NextOffset, body = Body, content = audio} = Frame,
             #convertor{write_offset = WriteOffset, out_mp4 = OutMp4, audio_frames = Audio} = Convertor) ->
  ok = file:pwrite(OutMp4, WriteOffset, Body),
  {ok, NextOffset, Convertor#convertor{write_offset = WriteOffset + size(Body), 
                                 audio_frames = [Frame#video_frame{body = {WriteOffset,size(Body)}}|Audio]}};

handle_frame(#video_frame{next_id = NextOffset}, Convertor) ->
  {ok, NextOffset, Convertor};

handle_frame(eof, #convertor{write_offset = WriteOffset, out_mp4 = OutMp4, mdat_offset = MdatOffset} = Convertor) ->
  file:pwrite(OutMp4, MdatOffset, <<(WriteOffset - MdatOffset):32>>),
  {ok, write_moov(Convertor)}.
  
  
sorted(Frames) ->
  lists:reverse(lists:keysort(#video_frame.dts, Frames)).
  
write_moov(#convertor{out_mp4 = OutMp4, write_offset = WriteOffset, video_frames = Video, audio_frames = Audio} = Convertor) ->
  [#video_frame{dts = DTS}|_] = Video,
  Moov = {moov, moov(Convertor#convertor{duration = DTS, video_frames = sorted(Video), audio_frames = sorted(Audio)})},
  {ok, End} = mp4_write(OutMp4, Moov, WriteOffset),
  Convertor#convertor{write_offset = End}.

mp4_header() ->
  [
    {ftyp, [<<"isom", 512:32>>, [<<"isom", "iso2", "avc1", "mp42">>]]},
    {free, <<>>}
  ].

mp4_write(File, [Atom|Atoms], Offset)	->
  {ok, NewOffset} = mp4_write(File, Atom, Offset),
  mp4_write(File, Atoms, NewOffset);

mp4_write(_File, [], Offset)	->
  {ok, Offset};
  
mp4_write(File, {_AtomName, _Content} = Atom, Offset)	->
  Bin = mp4_serialize(Atom),
  ok = file:pwrite(File, Offset, Bin),
  {ok, Offset + iolist_size(Bin)}.

mp4_serialize(Bin) when is_binary(Bin) ->
  Bin;
  
mp4_serialize(Number) when is_integer(Number) ->
  <<Number:32>>;
  
mp4_serialize(List) when is_list(List) ->
  mp4_serialize(List, []);
  
mp4_serialize({AtomName, Content}) ->
  Bin = iolist_to_binary(mp4_serialize(Content)),
  % ?D({AtomName, size(Bin) + 8}),
  [<<(size(Bin) + 8):32>>, atom_to_binary(AtomName, latin1), Bin].
  
  
mp4_serialize([], Acc) ->
  lists:reverse(Acc);

mp4_serialize([Atom|List], Acc) ->
  Bin = mp4_serialize(Atom),
  mp4_serialize(List, [Bin | Acc]).


esds_serialize(Bin) when is_binary(Bin) ->
  Bin;

esds_serialize(List) when is_list(List) ->
  [esds_serialize(Entry) || Entry <- List];

esds_serialize({AtomName, Content}) ->
  Bin = iolist_to_binary(esds_serialize(Content)),
  [<<AtomName, (size(Bin))>>, Bin].
  

%%%% Content part


tracks(Convertor) ->
  video_track(Convertor) ++ audio_track(Convertor).
  
video_track(#convertor{video_frames = []}) -> [];
video_track(#convertor{video_frames = RevVideo, duration = DTS, url = URL} = Convertor) ->
  Duration = round(DTS*?H264_SCALE),
	CTime = mp4_now(),
	MTime = mp4_now(),
  [ {trak, [
    {tkhd, pack_video_tkhd(Convertor)},
    {edts, [
      {elst, [<<0:32>>, pack_elst(Convertor)]}
    ]},
    {mdia, [
      {mdhd, <<0, 0:24, CTime:32, MTime:32, (?H264_SCALE*1000):32, Duration:32, 0:16, 0:16>>},
      {hdlr, <<0:32, 0:32, "vide", 0:96, "VideoHandler", 0>>},
      {minf, [
        {vmhd, <<1:32, 0:16, 0:16, 0:16, 0:16>>},
        {dinf, {dref, [<<0:32, 1:32>>, {'url ', [<<0, 1:24>>, URL]}]}},
        {stbl, [
          {stsd, [<<0:32, 1:32>>, {avc1, pack_video_config(Convertor)}]},
          {stsc, pack_chunk_sizes(RevVideo)},
          {stco, pack_chunk_offsets(RevVideo)},
          {stts, pack_durations(RevVideo)},
          {stsz, pack_sizes(RevVideo)},
          % {ctts, pack_compositions(RevVideo)},
          {stss, pack_keyframes(RevVideo)}
        ]}
      ]}
    ]}
  ]}].

uuid_atom() ->
  <<16#6b6840f2:32, 16#5f244fc5:32, 16#ba39a51b:32, 16#cf0323f3:32, 0:32>>.

audio_track(#convertor{audio_frames = []}) -> [];
audio_track(#convertor{audio_frames = RevAudio, duration = DTS} = Convertor) ->
  Duration = round(DTS*?AUDIO_SCALE),
  [ {trak, [
    {tkhd, pack_audio_tkhd(Convertor)},
    {mdia, [
      {mdhd, <<0, 0:24, 0:32, 0:32, (round(?AUDIO_SCALE*1000)):32, Duration:32, 0:1, (pack_language(eng))/bitstring, 0:16>>},
      {hdlr, <<0:32, 0:32, "soun", 0:96, "SoundHandler", 0>>},
      {minf, [
        {smhd, <<0:32, 0:16, 0:16>>},
        {dinf, {dref, [<<0:32, 1:32>>, {'url ', <<0, 1:24>>}]}},
        {stbl, [
          {stsd, [<<0:32, 1:32>>, pack_audio_config(Convertor)]},
          {stsc, pack_chunk_sizes(RevAudio)},
          {stco, pack_chunk_offsets(RevAudio)},
          {stts, pack_durations(RevAudio)},
          {stsz, pack_sizes(RevAudio)},
          {stss, pack_keyframes(RevAudio)}
        ]}
      ]}
    ]}
  ]}].


moov(#convertor{} = Convertor) ->
  [ {mvhd, pack_mvhd(Convertor)} ] ++ tracks(Convertor).

mp4_now() ->
  calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - 
  calendar:datetime_to_gregorian_seconds({{1904,1,1},{0,0,0}}).


pack_language(Lang) when is_binary(Lang) ->
  pack_language(binary_to_list(Lang));

pack_language(Lang) when is_atom(Lang) ->
  pack_language(atom_to_list(Lang));

pack_language([L1, L2, L3]) ->
  <<(L1 - 16#60):5, (L2 - 16#60):5, (L3 - 16#60):5>>.
  
pack_elst(Convertor) ->
  Duration = round(duration(Convertor)),
  MediaTime = 2002, 
  MediaRate = 1,
  MediaFrac = 0,
  <<1:32, Duration:32, MediaTime:32, MediaRate:16, MediaFrac:16>>.

pack_chunk_sizes(_VChunks) ->
  <<0:32, 1:32, 1:32, 1:32, 1:32>>.
 
pack_chunk_offsets(VChunks) ->
  [<<0:32, (length(VChunks)):32>>, [<<Offset:32>> || #video_frame{body = {Offset,_}} <- lists:reverse(VChunks)]].
 

next_track_id(#convertor{video_frames = []}) -> 2;
next_track_id(#convertor{audio_frames = []}) -> 2;
next_track_id(_) -> 3.

duration(#convertor{video_frames = [#video_frame{dts = DTS1}|_], audio_frames = [#video_frame{dts = DTS2}|_]}) ->
  if
    DTS1 > DTS2 -> DTS1;
    true -> DTS2
  end;
duration(#convertor{video_frames = [#video_frame{dts = DTS}|_]}) ->
  DTS;

duration(#convertor{audio_frames = [#video_frame{dts = DTS}|_]}) ->
  DTS.

pack_mvhd(#convertor{} = Convertor) ->
  CTime = mp4_now(),
  MTime = CTime,
  TimeScale = 1000,
  Duration = round(duration(Convertor)),
  Rate = 1,
  RateDelim = 0,
  Volume = 1,
  VolumeDelim = 0,
  Reserved1 = 0,
  Matrix = <<0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0>>,
  Reserved2 = <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
  NextTrackId = next_track_id(Convertor),
  <<0:32, CTime:32, MTime:32, TimeScale:32, Duration:32, Rate:16, RateDelim:16,
        Volume, VolumeDelim, 0:16, Reserved1:64, Matrix:36/binary, Reserved2:24/binary, NextTrackId:32>>.



pack_video_tkhd(Convertor) -> pack_tkhd(Convertor, video).
pack_audio_tkhd(Convertor) -> pack_tkhd(Convertor, audio).

pack_tkhd(#convertor{duration = DTS, video_config = Config}, Track) ->
	Flags = 15,
	CTime = mp4_now(),
	MTime = mp4_now(),
	TrackID = case Track of
	  video -> 1;
	  audio -> 2
	end,
	Reserved1 = 0,
	Duration = round(DTS),
	Reserved2 = 0,
	Layer = 0,
	AlternateGroup = 0,
	Volume = case Track of
	  video -> 0;
	  audio -> 1
	end,
	VolDelim = 0,
  Reserved3 = 0,
  Matrix = <<0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0>>,
  
  
  {Width, Height} = case Track of
    video ->
      Meta = h264:metadata(Config),
      {proplists:get_value(width, Meta), proplists:get_value(height, Meta)};
    audio ->
      {0, 0}
  end,
  
  WidthDelim = 0,
  HeightDelim = 0,
  <<0, Flags:24, CTime:32, MTime:32, TrackID:32, Reserved1:32, Duration:32, Reserved2:64, 
  Layer:16, AlternateGroup:16, Volume, VolDelim, Reserved3:16, Matrix/binary, 
  Width:16, WidthDelim:16, Height:16, HeightDelim:16>>.

pack_audio_config(#convertor{audio_config = undefined, audio_frames = [#video_frame{codec = speex}|_]}) ->
  {'spx ', <<>>};

pack_audio_config(#convertor{audio_config = undefined}) ->
  ?D({"no audio config"}),
  <<>>;


pack_audio_config(#convertor{audio_config = Config}) ->
  Reserved = <<0,0,0,0,0,0>>,
  RefIndex = 1,
  SoundVersion = 0,
  Unknown = <<0,0,0,0,0,0>>,
  
  {_AACType, _SampleRate, ChannelsCount, _SamplesPerFrame} = aac:pack_config(aac:decode_config(Config)),
  
  SampleSize = 16,
  PacketSize = 0,
  % SampleRate = 44100,
  CompressionId = 0,

  ESID = 2,
  StreamDependence = 0,
  HaveUrl = 0,
  OCRStream = 0,
  StreamPriority = 0,
  ESDescr = <<ESID:16, StreamDependence:1, HaveUrl:1, OCRStream:1, StreamPriority:5>>,
  
  ObjectType = 64,
  StreamType = 5,
  UpStream = 0,
  Reserved3 = 1,
  BufferSizeDB = 0,
  MaxBitrate = 126035,
  AvgBitrate = 0,
  ConfigDescr = <<ObjectType, StreamType:6, UpStream:1, Reserved3:1, BufferSizeDB:24, MaxBitrate:32, AvgBitrate:32>>,
  
  MP4A = <<Reserved:6/binary, RefIndex:16, SoundVersion:16, Unknown:6/binary, ChannelsCount:16, SampleSize:16, 
            CompressionId:16, PacketSize:16, (round(?AUDIO_SCALE*1000)):16, 0:16>>,
  ESDS = {?MP4ESDescrTag, [ESDescr,
     {?MP4DecConfigDescrTag, [
         ConfigDescr, {?MP4DecSpecificDescrTag, Config}
      ]},
     {?MP4Unknown6Tag, <<2>>}]
   },
   
  {mp4a, [MP4A,{esds, [<<0:32>>, esds_serialize(ESDS)]}]}.




pack_video_config(#convertor{video_config = Config}) ->
  Meta = h264:metadata(Config),
  Width = proplists:get_value(width, Meta),
  Height = proplists:get_value(height, Meta),
  
  Reserved = <<0,0,0,0,0,0>>,
  RefIndex = 1,
  CodecStreamVersion = 0,
  CodecStreamRevision = 0,
  HorizRes = 72,
  VertRes = 72,
  DataSize = 0,
  FrameCount = 1,
  Compressor = <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
  Reserved1 = 24,
  Reserved2 = 16#FFFF,
  Bin = <<Reserved:6/binary, RefIndex:16, CodecStreamVersion:16, CodecStreamRevision:16, 
        0:32, 0:32, 0:32, Width:16, Height:16,
        HorizRes:16, 0:16, VertRes:16, 0:16, DataSize:32, FrameCount:16, 
        (size(Compressor)), Compressor:31/binary,
        Reserved1:16, Reserved2:16>>,
  
  [Bin, {avcC, Config}, {uuid, uuid_atom()}].
  
pack_durations(ReverseFrames) ->	
	Durations = pack_durations(ReverseFrames, []),
	List = [<<1:32, Duration:32>> || Duration <- Durations],
	[<<0:32, (length(List)):32>>, List].

pack_durations([#video_frame{dts = DTS}, #video_frame{dts = DTS, content = Content} = F|ReverseFrames], Acc) ->
	pack_durations([F#video_frame{dts = DTS - 1}|ReverseFrames], [round(scale_for_content(Content)) | Acc]);

pack_durations([#video_frame{dts = DTS1}, #video_frame{dts = DTS2, content = Content} = F|ReverseFrames], Acc) when DTS1 > DTS2 ->
	pack_durations([F|ReverseFrames], [round((DTS1 - DTS2)*scale_for_content(Content)) | Acc]);

pack_durations([#video_frame{}], Acc) ->
	[0 | Acc].

scale_for_content(video) -> ?H264_SCALE;
scale_for_content(audio) -> ?AUDIO_SCALE.
  
pack_sizes(ReverseFrames) ->
  pack_sizes(<<0:32, 0:32, (length(ReverseFrames)):32>>, lists:reverse(ReverseFrames)).

pack_sizes(Bin, []) ->
  Bin;
  
pack_sizes(Bin, [#video_frame{body = {_Offset,BodySize}}|Frames]) ->
  pack_sizes(<<Bin/binary, BodySize:32>>, Frames).


pack_keyframes(ReverseFrames) ->
  List = pack_keyframes(lists:reverse(ReverseFrames), [], 1),
  [<<0:32, (length(List)):32>>, [<<Number:32>> || Number <- List]].
  
pack_keyframes([#video_frame{flavor = keyframe}|Frames], Acc, Number) ->
  pack_keyframes(Frames, [Number|Acc], Number + 1);

pack_keyframes([#video_frame{}|Frames], Acc, Number) ->
  pack_keyframes(Frames, Acc, Number + 1);
  
pack_keyframes([], Acc, _) ->
  lists:reverse(Acc).


pack_compositions(ReverseFrames) ->
  [<<0:32, (length(ReverseFrames)):32>>, compositions(ReverseFrames, [])].
  
  
compositions([], Acc) ->
  Acc;
compositions([#video_frame{dts = DTS, pts = PTS, content = Content}|Frames], Acc) when PTS >= DTS ->
  compositions(Frames, [<<1:32, (round((PTS - DTS)*scale_for_content(Content))):32>>|Acc]).

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").

esds_serialize_test_() ->
  [?_assertEqual(<<5,1,2>>, iolist_to_binary(esds_serialize({?MP4DecSpecificDescrTag, <<2>>}))),
  ?_assertEqual(<<3,6,0,0,0,5,1,2>>, iolist_to_binary(esds_serialize({?MP4ESDescrTag, [<<0,0>>, <<0>>, {?MP4DecSpecificDescrTag, <<2>>}]})))].

mp4_serialize1_test_() ->
  [?_assertEqual(<<8:32, "free">>, iolist_to_binary(mp4_serialize({free, <<>>}))),
  ?_assertEqual(<<16:32, "ftyp", "isom", "mp42">>, iolist_to_binary(mp4_serialize({ftyp, [<<"isom">>, <<"mp42">>]}))),
  ?_assertEqual(<<16:32, "ftyp", 5:32, 100:32>>, iolist_to_binary(mp4_serialize({ftyp, [5, 100]})))].

pack_durations_test() ->
  Frames = [#video_frame{dts = 2, content = video}, #video_frame{dts = 1, content = video}, #video_frame{dts = 0, content = video}],
  ?assertEqual(<<0:32, 3:32, 1:32, 0:32, 1:32, 24:32, 1:32, 24:32>>, iolist_to_binary(pack_durations(Frames))).

pack_glue_durations_test() ->
  Frames = [#video_frame{dts = 2, content = video}, #video_frame{dts = 2, content = video}, #video_frame{dts = 0, content = video}],
  ?assertEqual(<<0:32, 3:32, 1:32, 0:32, 1:32, 24:32, 1:32, 24:32>>, iolist_to_binary(pack_durations(Frames))).


pack_keyframes_test() ->
  Frames = [#video_frame{}, #video_frame{}, #video_frame{flavor = keyframe}, #video_frame{}, #video_frame{flavor = keyframe}],
  ?assertEqual(<<0:32, 2:32, 1:32, 3:32>>, iolist_to_binary(pack_keyframes(Frames))).
  
