%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010-2011 Max Lapshin
%%% @doc        Module to write H.264
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
% 
% Two pass method:
% 
% 1. open media
% 2. write header
% 3. read frame by frame, skip pwrite, store info in list
% 4. when eof happens, write frame list to moov
% 5. calculate moov size
% 6. append moov size to all frame offsets
% 7. write again moov to output
% 8. rewind media
% 9. calculate total mdat size
% 10. write total mdat size
% 11. read frame by frame, write frame to output
% 
% 
% One pass method:
% 
% 1. write header
% 2. open file
% 3. remember mdat start position
% 4. wait for frames
% 5. dump frame content to disk, store info in list
% 6. when eof happens, write mdat size into the beginning
% 7. write moov to disk
% 
% So, media writer should be one of two choices should provide two methods:
% 
% Writer(Offset, Bin) — random position write media for one pass method
% or
% Writer(Bin) — stream media for two pass method
% 
%%% This file is part of erlmedia.
%%% 
%%% erlmedia is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlmedia is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlmedia.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(mp4_writer).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/flv.hrl").
-include("../include/mp4.hrl").
-include("../include/mp3.hrl").
-include("../include/aac.hrl").
-include("../include/video_frame.hrl").
-include("log.hrl").

-export([write/2, write/3, pack_language/1, dump_media/2]).
-export([init/2, handle_frame/2, write_frame/2]).

-export([pack_compositions/1]).

-record(convertor, {
  method,
  options = [],
  writer,
  url,
  write_offset,
  header_end,
  duration,
  min_dts,
  max_dts,

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
  OutMp4 = file:open(OutMp4Path, [write, binary, raw]),
  {ok, Convertor} = init(fun(Offset, Bin) ->
    ok = file:pwrite(OutMp4, Offset, Bin)
  end, Options),
  _Convertor1 = write_mp4(Reader, ReadOffset, Convertor),
  file:close(OutMp4),
  file:close(InFlv),
  ok.


% 1294735440 .. 1294736280
%
dump_media(undefined, _Options) ->
  {ok, Pid} = media_provider:open(default, "zzz"),
  {ok, Out} = file:open("out.mp4", [append, binary, raw]),
  Start = 1294735443380,
  End = 1294736280000,
  % End = 1294735448480,
  dump_media(Pid, [{writer, fun(_Offset, Bin) ->
    file:write(Out, Bin)
  end}, {from, Start}, {to, End}]),
  file:close(Out),
  ok;

dump_media(Media, Options) when is_pid(Media) ->
  ems_media:subscribe(Media, Options),
  
  Writer = proplists:get_value(writer, Options),
    
  {ok, Converter} = mp4_writer:init(Writer, [{method,two_pass}]),
  From = proplists:get_value(from, Options),
  To = proplists:get_value(to, Options),
  
  {StartPos, _} = ems_media:seek_info(Media, From, []),
  {ok, Converter1} = dump_media_2pass(Media, Converter, StartPos, To),
  {ok, Converter2} = shift_and_write_moov(Converter1),

  {StartPos, _} = ems_media:seek_info(Media, From, []),
  {ok, _Converter3} = dump_media_2pass(Media, Converter2, StartPos, To),
  ok.


dump_media_2pass(Media, Writer, Pos, End) ->
  case ems_media:read_frame(Media, Pos) of
    eof ->
      {ok, Writer};
    #video_frame{dts = DTS} when DTS >= End ->
      {ok, Writer};
    #video_frame{next_id = NewPos} = Frame ->  
      {ok, Writer1} = handle_frame(Frame, Writer),
      dump_media_2pass(Media, Writer1, NewPos, End)
  end.

  


write_mp4(Reader, ReadOffset, Convertor) ->
  case handle_frame(Reader(ReadOffset), Convertor) of
    {ok, NextOffset, Convertor1} ->
      write_mp4(Reader, NextOffset, Convertor1);
    {ok, Convertor1} ->
      Convertor1
  end.
      

init(Writer, Options) ->
  {ok, HeaderEnd} = mp4_write(Writer, 0, mp4_header() ++ proplists:get_value(mp4, Options, [])),
  Method = proplists:get_value(method, Options),
  ?D({add_mp4, proplists:get_value(mp4, Options, [])}),
  % ?D({header_end, HeaderEnd}),
  
  {ok, WriteOffset} = case Method of
    one_pass -> mp4_write(Writer, HeaderEnd, {mdat, <<>>});
    two_pass -> {ok, HeaderEnd}
  end,
  % ?D({write_offset, WriteOffset}),
  
  {ok, #convertor{options = Options,
             method = Method,
             write_offset = WriteOffset,
             header_end = HeaderEnd,
             writer = Writer,
             url = proplists:get_value(url, Options, <<>>)}}.
  
  


write_frame(Frame, Writer) ->
  handle_frame(Frame, Writer).
    
handle_frame(#video_frame{flavor = command}, Convertor) ->
  {ok, Convertor};
    
handle_frame(#video_frame{flavor = config, content = video, body = Config}, Convertor) ->
  ?D("mp4_writer got video config"),
  {ok, Convertor#convertor{video_config = Config}};

handle_frame(#video_frame{flavor = config, content = audio, body = Config}, Convertor) ->
  ?D("mp4_writer got audio config"),
  {ok, Convertor#convertor{audio_config = Config}};

handle_frame(#video_frame{codec = mp3, body = Body}, #convertor{audio_config = undefined} = Convertor) ->
  {ok, #mp3_frame{} = Config, _} = mp3:read(Body),
  ?D("mp4_writer got audio config"),
  {ok, Convertor#convertor{audio_config = Config}};
  

handle_frame(#video_frame{content = metadata}, Convertor) ->
  {ok, Convertor};

handle_frame(#video_frame{body = Body} = Frame,
             #convertor{write_offset = WriteOffset, writer = Writer, method = one_pass} = Convertor) ->
  Writer(WriteOffset, Body),
  {ok, Convertor1} = append_frame_to_list(Frame, Convertor),
  {ok, Convertor1#convertor{write_offset = WriteOffset + size(Body)}};

handle_frame(#video_frame{body = Body} = Frame,
             #convertor{write_offset = WriteOffset, method = two_pass} = Convertor) ->
  {ok, Convertor1} = append_frame_to_list(Frame, Convertor),
  {ok, Convertor1#convertor{write_offset = WriteOffset + size(Body)}};

handle_frame(#video_frame{body = Body},
             #convertor{write_offset = WriteOffset, writer = Writer, method = two_pass2} = Convertor) ->
  Writer(WriteOffset, Body),
  {ok, Convertor#convertor{write_offset = WriteOffset + size(Body)}};

handle_frame(eof, #convertor{write_offset = WriteOffset, writer = Writer, header_end = HeaderEnd, method = one_pass} = Convertor) ->
  Writer(HeaderEnd, <<(WriteOffset - HeaderEnd):32>>),
  {ok, write_moov(sort_frames(Convertor))}.


append_frame_to_list(#video_frame{dts = DTS} = Frame, #convertor{min_dts = Min} = C) when Min == undefined orelse Min > DTS ->
  append_frame_to_list(Frame, C#convertor{min_dts = DTS});

append_frame_to_list(#video_frame{dts = DTS} = Frame, #convertor{max_dts = Max} = C) when Max == undefined orelse Max < DTS ->
  append_frame_to_list(Frame, C#convertor{max_dts = DTS});

append_frame_to_list(#video_frame{body = Body, content = video} = Frame, 
             #convertor{write_offset = WriteOffset, video_frames = Video} = Convertor) ->
  {ok, Convertor#convertor{video_frames = [Frame#video_frame{body = {WriteOffset,size(Body)}}|Video]}};

append_frame_to_list(#video_frame{body = Body, content = audio} = Frame,
             #convertor{write_offset = WriteOffset, audio_frames = Audio} = Convertor) ->
  {ok, Convertor#convertor{audio_frames = [Frame#video_frame{body = {WriteOffset,size(Body)}}|Audio]}}.


shift_and_write_moov(#convertor{writer = Writer, header_end = HeaderEnd, write_offset = WriteOffset, method = two_pass} = Convertor) ->
  MdatSize = WriteOffset - HeaderEnd,
  Convertor0 = sort_frames(Convertor),
  #convertor{write_offset = MoovOffset} = Convertor1 = write_moov(Convertor0#convertor{write_offset = HeaderEnd, writer = fun(_, _) -> ok end}),
  MoovSize = MoovOffset - HeaderEnd,
  MdatHeaderSize = 8,
  Convertor2 = append_chunk_offsets(Convertor1, MoovSize + MdatHeaderSize),
  
  #convertor{write_offset = MoovOffset} = Convertor3 = write_moov(Convertor2#convertor{write_offset = HeaderEnd, writer = Writer}),
  MoovSize = MoovOffset - HeaderEnd,
  
  Writer(MoovOffset, <<(MdatSize+MdatHeaderSize):32, "mdat">>),
  {ok, Convertor3#convertor{method = two_pass2, write_offset = MoovOffset + MdatHeaderSize}}.
  
  
append_chunk_offsets(#convertor{video_frames = Video, audio_frames = Audio} = Convertor, Shift) ->
  Video1 = [Frame#video_frame{body = {Offset+Shift,Size}} || #video_frame{body = {Offset,Size}} = Frame <- Video],
  Audio1 = [Frame#video_frame{body = {Offset+Shift,Size}} || #video_frame{body = {Offset,Size}} = Frame <- Audio],
  Convertor#convertor{video_frames = Video1, audio_frames = Audio1}.
  
sorted(Frames) ->
  lists:sort(fun
    (#video_frame{dts = DTS1}, #video_frame{dts = DTS2}) when DTS1 >= DTS2 -> true;
    (#video_frame{dts = DTS, pts = PTS1}, #video_frame{dts = DTS, pts = PTS2}) when PTS1 >= PTS2 -> true;
    (_, _) -> false
  end, Frames).
  
sort_frames(#convertor{video_frames = Video, audio_frames = Audio} = Convertor) ->
  Convertor#convertor{video_frames = sorted(Video), audio_frames = sorted(Audio)}.
  
write_moov(#convertor{writer = Writer, write_offset = WriteOffset, min_dts = Min, max_dts = Max} = Convertor) ->
  Duration = round(Max - Min),
  Convertor1 = Convertor#convertor{duration = Duration},
  Moov = {moov, moov(Convertor1)},
  {ok, End} = mp4_write(Writer, WriteOffset, Moov),
  Convertor1#convertor{write_offset = End}.

mp4_header() ->
  [
    {ftyp, [<<"isom", 512:32>>, [<<"isom", "iso2", "avc1", "mp42">>]]},
    {free, <<>>}
  ].

mp4_write(Writer, Offset, [Atom|Atoms])	->
  {ok, NewOffset} = mp4_write(Writer, Offset, Atom),
  mp4_write(Writer, NewOffset, Atoms);

mp4_write(_Writer, Offset, [])	->
  {ok, Offset};
  
mp4_write(Writer, Offset, {_AtomName, _Content} = Atom)	->
  Bin = mp4_serialize(Atom),
  Writer(Offset, Bin),
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
          {ctts, pack_compositions(RevVideo)},
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
  
pack_elst(#convertor{duration = Duration}) ->
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


pack_mvhd(#convertor{duration = Duration} = Convertor) ->
  CTime = mp4_now(),
  MTime = CTime,
  TimeScale = 1000,
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

pack_tkhd(#convertor{duration = Duration, video_config = Config}, Track) ->
	Flags = 15,
	CTime = mp4_now(),
	MTime = mp4_now(),
	TrackID = case Track of
	  video -> 1;
	  audio -> 2
	end,
	Reserved1 = 0,
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


pack_audio_config(#convertor{audio_config = Config, audio_frames = [#video_frame{codec = Codec}|_]}) ->
  Reserved = <<0,0,0,0,0,0>>,
  RefIndex = 1,
  SoundVersion = 0,
  Unknown = <<0,0,0,0,0,0>>,

  {ObjectType, ChannelsCount} = case Codec of
    aac ->
      #aac_config{channel_count = AACChannels} = aac:decode_config(Config),
      {64, AACChannels};
    mp3 ->
      #mp3_frame{channels = Channels} = Config,
      {107, Channels}
  end,

  
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
  
  StreamType = 5,
  UpStream = 0,
  Reserved3 = 1,
  BufferSizeDB = 0,
  MaxBitrate = 126035,
  AvgBitrate = 0,
  ConfigDescr = <<ObjectType, StreamType:6, UpStream:1, Reserved3:1, BufferSizeDB:24, MaxBitrate:32, AvgBitrate:32>>,
  

  MP4A = <<Reserved:6/binary, RefIndex:16, SoundVersion:16, Unknown:6/binary, ChannelsCount:16, SampleSize:16, 
            CompressionId:16, PacketSize:16, (round(?AUDIO_SCALE*1000)):16, 0:16>>,
            
  DescrTag = case Codec of
    aac -> [ConfigDescr, {?MP4DecSpecificDescrTag, Config}];
    mp3 -> ConfigDescr
  end,
  ESDS = {?MP4ESDescrTag, [ESDescr,
     {?MP4DecConfigDescrTag, DescrTag},
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

pack_durations([#video_frame{dts = DTS1}, #video_frame{dts = DTS2, content = Content} = F|ReverseFrames], []) when DTS1 > DTS2 ->
  Duration = round((DTS1 - DTS2)*scale_for_content(Content)),
	pack_durations([F|ReverseFrames], [Duration, Duration]);


pack_durations([#video_frame{dts = DTS, pts = PTS}, #video_frame{dts = DTS1, content = Content} = F|ReverseFrames], Acc) when DTS =< DTS1->
	pack_durations([F#video_frame{dts = DTS - 1, pts = PTS - DTS1 + DTS}|ReverseFrames], [round(scale_for_content(Content)) | Acc]);

pack_durations([#video_frame{dts = DTS1}, #video_frame{dts = DTS2, content = Content}], Acc) when DTS1 > DTS2 ->
	[round((DTS1 - DTS2)*scale_for_content(Content)) | Acc];

pack_durations([#video_frame{dts = DTS1}, #video_frame{dts = DTS2, content = Content} = F|ReverseFrames], Acc) when DTS1 > DTS2 ->
	pack_durations([F|ReverseFrames], [round((DTS1 - DTS2)*scale_for_content(Content)) | Acc]).


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
  Frames = [#video_frame{dts = 2, pts = 1, content = video}, #video_frame{dts = 1, pts = 2, content = video}, #video_frame{dts = 0, pts = 0, content = video}],
  ?assertEqual(<<0:32, 3:32, 1:32, 0:32, 1:32, 24:32, 1:32, 24:32>>, iolist_to_binary(pack_durations(Frames))).

pack_glue_durations_test() ->
  Frames = [#video_frame{dts = 2, pts = 2, content = video}, #video_frame{dts = 2, pts = 1, content = video}, #video_frame{dts = 0, pts = 0, content = video}],
  ?assertEqual(<<0:32, 3:32, 1:32, 0:32, 1:32, 24:32, 1:32, 24:32>>, iolist_to_binary(pack_durations(Frames))).


pack_keyframes_test() ->
  Frames = [#video_frame{}, #video_frame{}, #video_frame{flavor = keyframe}, #video_frame{}, #video_frame{flavor = keyframe}],
  ?assertEqual(<<0:32, 2:32, 1:32, 3:32>>, iolist_to_binary(pack_keyframes(Frames))).
  
