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
-module(mp4_multi_writer).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/flv.hrl").
-include("../include/mp4.hrl").
-include("../include/mp3.hrl").
-include("../include/aac.hrl").
-include("../include/video_frame.hrl").
-include("../include/media_info.hrl").
-include("log.hrl").

-export([write/2, write/3, pack_language/1, dump_media/1]).
-export([init/2, handle_frame/2, write_frame/2]).
-export([pack_durations/1]).
-export([mp4_serialize/1]).

-export([test_dump/0, dump_by_spec/2]).

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
  tracks = []
}).

-record(writer_track, {
  content,
  id,
  frames = [],
  duration,
  timescale,
  config
}).

-undef(D).
-define(D(X), io:format("~p:~p ~p~n", [?MODULE, ?LINE, X])).

-define(H264_SCALE, 24).
% -define(AUDIO_SCALE, 44.1).


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
test_dump() ->
  Specification = [
    "../flussonic/priv/bunny_mbr_200.mp4@1",
    "../flussonic/priv/bunny_mbr_700.mp4@1",
    "../flussonic/priv/bunny_mbr_1500.mp4@1",
    "../flussonic/priv/bunny.mp4@2"
  ],

  {ok, Out} = file:open("zzz.mp4", [binary, write]),
  
  Writer = fun(_Offset, Bin) ->
    file:write(Out, Bin)
  end,
  
  dump_by_spec(Specification, [{writer,Writer}]),
  file:close(Out),
  ok.

dump_by_spec(Specification, Options) ->
  Readers = lists:map(fun(Spec) ->
    [Path, Track] = string:tokens(Spec, "@"),
    TrackId = list_to_integer(Track),
    {ok, F} = file:open(Path, [read, binary]),
    {ok, R} = mp4:open({file, F}, []),
    fun(Id) -> 
      case mp4:read_frame(R, {track, TrackId, Id}) of
        eof -> eof;
        Fr -> Fr#video_frame{next_id = if Id == config -> 0; true -> Id+1 end}
      end
    end      
  end, Specification),
  
  % StartPos = [config || _ <- Specification],
  StartPos = {1, config},
  
  Reader = fun({R, K}) ->
    Fr = read_multi_reader(Readers, R, K),
    % if Fr == eof -> ?D({read, R, K, eof}); true -> ?D({read, R, K, Fr#video_frame.dts}) end,
    Fr
  end,
  
  dump_media([{reader, Reader}, {start_pos, StartPos}, {no_autosplit, true}|Options]),
  ok.


read_multi_reader(Readers, R, K) ->
  case (lists:nth(R, Readers))(K) of
    eof when R == length(Readers) -> eof;
    eof ->
      Fr = (lists:nth(R+1, Readers))(config),
      Fr#video_frame{next_id = {R+1, 0}, stream_id = R+1};
    #video_frame{next_id = Next} = Fr ->
      Fr#video_frame{next_id = {R, Next}, stream_id = R}
  end.


%
% 1. Pass reader that sends frames from several sources with different stream_id
% 2. Read each of them one by one
% 3. Assign number to frames from each reader by wrapping in a function
% 4. Separate audio and video frames from readers into different tracks
%
% Calling code should:
% 1. Collect instructions to open different mp4 files.
% 2. Split them to different Reader functions
% 3. Pass to mp4_writer


dump_media(Options) ->
  Writer = proplists:get_value(writer, Options),
  Reader1 = proplists:get_value(reader, Options),
  Reader = case proplists:get_value(no_autosplit, Options, false) of
    true -> Reader1;
    _ -> fun(Id) ->
      case Reader1(Id) of
        #video_frame{content = video} = F -> F#video_frame{stream_id = 1};
        #video_frame{content = audio} = F -> F#video_frame{stream_id = 2};
        Else -> Else
      end
    end    
  end,
  Header = proplists:get_value(header, Options),
  StartPos = proplists:get_value(start_pos, Options),
    
  if is_function(Header) -> Header(undefined); true -> ok end,

  {ok, Converter} = mp4_writer:init(Writer, [{method,two_pass}]),
  {ok, Converter1} = dump_media_2pass(Reader, Converter, StartPos),
  {ok, Converter2} = shift_and_write_moov(Converter1),
  {ok, _Converter3} = dump_media_2pass(Reader, Converter2, StartPos),
  ok.


dump_media_2pass(Reader, Writer, Pos) ->
  case Reader(Pos) of
    eof ->
      {ok, Writer};
    #video_frame{next_id = NewPos} = Frame ->
      % ?D({read, round(Frame#video_frame.dts), Frame#video_frame.flavor, Frame#video_frame.codec}),
      {ok, Writer1} = handle_frame(Frame, Writer),
      dump_media_2pass(Reader, Writer1, NewPos)
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
  % ?D({add_mp4, proplists:get_value(mp4, Options, [])}),
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

handle_frame(#video_frame{codec = empty}, Convertor) ->
  {ok, Convertor};
    
handle_frame(#video_frame{flavor = config, content = Content, body = Config, stream_id = StreamId}, #convertor{tracks = Tracks} = Convertor) ->
  Track = case lists:keyfind(StreamId, #writer_track.id, Tracks) of
    false -> #writer_track{content = Content, id = StreamId};
    T -> T
  end,
  % ?D("mp4_writer got video config"),
  {ok, Convertor#convertor{tracks = lists:keystore(StreamId, #writer_track.id, Tracks, Track#writer_track{config = Config})}};

% handle_frame(#video_frame{codec = mp3, body = Body}, #convertor{audio_config = undefined} = Convertor) ->
%   {ok, #mp3_frame{} = Config, _} = mp3:read(Body),
%   % ?D("mp4_writer got audio config"),
%   {ok, Convertor#convertor{audio_config = Config}};
  

handle_frame(#video_frame{content = metadata}, Convertor) ->
  {ok, Convertor};

handle_frame(#video_frame{} = Frame,
             #convertor{write_offset = WriteOffset, writer = Writer, method = one_pass} = Convertor) ->
  Body = flv_video_frame:to_tag(Frame),             
  Writer(WriteOffset, Body),
  {ok, Convertor1} = append_frame_to_list(Frame, Convertor),
  {ok, Convertor1#convertor{write_offset = WriteOffset + size(Body)}};

handle_frame(#video_frame{} = Frame,
             #convertor{write_offset = WriteOffset, method = two_pass} = Convertor) ->
  Body = flv_video_frame:to_tag(Frame),             
  {ok, Convertor1} = append_frame_to_list(Frame, Convertor),
  {ok, Convertor1#convertor{write_offset = WriteOffset + size(Body)}};

handle_frame(#video_frame{} = Frame,
             #convertor{write_offset = WriteOffset, writer = Writer, method = two_pass2} = Convertor) ->
  Body = flv_video_frame:to_tag(Frame),             
  Writer(WriteOffset, Body),
  {ok, Convertor#convertor{write_offset = WriteOffset + size(Body)}};

handle_frame(eof, #convertor{write_offset = WriteOffset, writer = Writer, header_end = HeaderEnd, method = one_pass} = Convertor) ->
  Writer(HeaderEnd, <<(WriteOffset - HeaderEnd):32>>),
  {ok, write_moov(sort_frames(Convertor))}.


append_frame_to_list(#video_frame{dts = DTS} = Frame, #convertor{min_dts = Min} = C) when Min == undefined orelse Min > DTS ->
  append_frame_to_list(Frame, C#convertor{min_dts = DTS});

append_frame_to_list(#video_frame{dts = DTS} = Frame, #convertor{max_dts = Max} = C) when Max == undefined orelse Max < DTS ->
  append_frame_to_list(Frame, C#convertor{max_dts = DTS});

append_frame_to_list(#video_frame{body = Body, codec = Codec, stream_id = StreamId, content = Content} = Frame, 
             #convertor{write_offset = WriteOffset, tracks = Tracks} = Convertor) ->
  #writer_track{frames = Frames} = Track = case lists:keyfind(StreamId, #writer_track.id, Tracks) of
    false -> #writer_track{content = Content, id = StreamId};
    T -> T
  end,
  Track1 = Track#writer_track{frames = [Frame#video_frame{body = {WriteOffset + flv:content_offset(Codec),size(Body)}}|Frames]},
  {ok, Convertor#convertor{tracks = lists:keystore(StreamId, #writer_track.id, Tracks, Track1)}}.


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
  
  
append_chunk_offsets(#convertor{tracks = Tracks} = Convertor, Shift) ->
  Tracks1 = lists:map(fun(#writer_track{frames = Frames} = Track) ->
    Track#writer_track{frames = [Frame#video_frame{body = {Offset+Shift,Size}} || #video_frame{body = {Offset,Size}} = Frame <- Frames]}
  end, Tracks),
  Convertor#convertor{tracks = Tracks1}.
  
sorted(Frames) ->
  lists:sort(fun
    (#video_frame{dts = DTS1}, #video_frame{dts = DTS2}) when DTS1 >= DTS2 -> true;
    (#video_frame{dts = DTS, pts = PTS1}, #video_frame{dts = DTS, pts = PTS2}) when PTS1 >= PTS2 -> true;
    (_, _) -> false
  end, Frames).
  
sort_frames(#convertor{tracks = Tracks} = Convertor) ->
  Tracks1 = [T#writer_track{frames = sorted(Frames)} || #writer_track{frames = Frames} = T <- Tracks],
  Convertor#convertor{tracks = Tracks1}.
  
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


tracks(#convertor{tracks = Tracks}) ->
  [track(Track) || Track <- Tracks].

track(#writer_track{content = video} = Track) -> video_track(Track);
track(#writer_track{content = audio} = Track) -> audio_track(Track).
  
video_track(#writer_track{frames = []}) -> [];
video_track(#writer_track{frames = RevVideo1} = Track1) ->
	CTime = mp4_now(),
	MTime = mp4_now(),
	RevVideo = normalize_h264_durations(RevVideo1),
	Duration = lists:sum([D || #video_frame{dts = D} <- RevVideo]),
	Track = Track1#writer_track{duration = Duration, timescale = ?H264_SCALE*1000},
  [ {trak, [
    {tkhd, pack_tkhd(Track)},
    {edts, [
      {elst, [<<0:32>>, pack_elst(Track)]}
    ]},
    {mdia, [
      {mdhd, <<0, 0:24, CTime:32, MTime:32, (?H264_SCALE*1000):32, Duration:32, 0:16, 0:16>>},
      {hdlr, <<0:32, 0:32, "vide", 0:96, "VideoHandler", 0>>},
      {minf, [
        {vmhd, <<1:32, 0:16, 0:16, 0:16, 0:16>>},
        {stbl, [
          {stsd, [<<0:32, 1:32>>, {avc1, pack_config(Track)}]},
          {stsc, pack_chunk_sizes(RevVideo)},
          {stco, pack_chunk_offsets(RevVideo)},
          {stts, pack_durations(RevVideo)},
          {stsz, pack_sizes(RevVideo)},
          {stss, pack_keyframes(RevVideo)}
        ] ++ case is_ctts_required(RevVideo) of
          true -> [{ctts, pack_compositions(RevVideo)}];
          false -> []
        end}
      ]}
    ]}
  ]}].

uuid_atom() ->
  <<16#6b6840f2:32, 16#5f244fc5:32, 16#ba39a51b:32, 16#cf0323f3:32, 0:32>>.

audio_track(#writer_track{frames = []}) -> [];
audio_track(#writer_track{frames = RevAudio1, config = Config} = Track1) ->
  Track2 = case Track1#writer_track.frames of
    [#video_frame{codec = mp3, body = Body}|_] ->
      {ok, #mp3_frame{sample_rate = SampleRate} = Config, _} = mp3:read(Body),
      Track1#writer_track{timescale = SampleRate, config = Config};
    [#video_frame{codec = aac}|_] ->
      #aac_config{sample_rate = SampleRate} = aac:decode_config(Config),
      Track1#writer_track{timescale = SampleRate}
  end,
  Duration = round(length(RevAudio1)*1024 / Track2#writer_track.timescale),
  RevAudio = normalize_aac_durations(RevAudio1),
	Track = Track2#writer_track{duration = Duration},
  [ {trak, [
    {tkhd, pack_tkhd(Track)},
    {mdia, [
      {mdhd, <<0, 0:24, 0:32, 0:32, SampleRate:32, Duration:32, 0:1, (pack_language(eng))/bitstring, 0:16>>},
      {hdlr, <<0:32, 0:32, "soun", 0:96, "SoundHandler", 0>>},
      {minf, [
        {smhd, <<0:32, 0:16, 0:16>>},
        {dinf, {dref, [<<0:32, 1:32>>, {'url ', <<0, 1:24>>}]}},
        {stbl, [
          {stsd, [<<0:32, 1:32>>, pack_config(Track)]},
          {stsc, pack_chunk_sizes(RevAudio)},
          {stco, pack_chunk_offsets(RevAudio)},
          {stts, pack_durations(RevAudio)},
          {stsz, pack_sizes(RevAudio)},
          {stss, pack_keyframes(RevAudio)}
        ]}
      ]}
    ]}
  ]}].


normalize_aac_durations(RevAudio) ->
  [F#video_frame{dts = 1024, pts = undefined} || #video_frame{} = F <- RevAudio].


normalize_h264_durations(RevVideo) ->
  Durations1 = lists:reverse(dts_to_durations(RevVideo)),
  Timescale = ?H264_SCALE*1000,
  Total = lists:sum(Durations1),
  Avg = Total / length(Durations1),
  FPS = round(Timescale / Avg),  
  Ideal = round(Timescale / FPS),
  IdealDuration = Ideal*length(Durations1),
  TotalDeviation = round(abs(Total - IdealDuration)*100 / Total),
  Deviation = [round(abs(D - Ideal)*100 / Ideal) || D <- Durations1],
  % ?D({normalize, {time, Total}, {avg, Avg,Timescale}, {avg_abs,Avg*1000 / Timescale}, {fps, FPS, Ideal}, length(Durations1),
  % {deviation, lists:min(Deviation), lists:max(Deviation)}}),
  MaxDeviation = lists:max(Deviation),
  % ?D({normalize, {avg, Avg,MaxDeviation}, {ideal, Ideal, Ideal / Timescale}, {duration,Total},{total,IdealDuration,TotalDeviation}}),
  Durations2 = if MaxDeviation < 6 andalso TotalDeviation < 5 -> [Ideal || _ <- Durations1];
    true -> Durations1
  end,
  Durations = Durations2,
  lists:zipwith(fun(D, #video_frame{dts = DTS, pts = PTS} = Frame) ->
    Frame#video_frame{dts = D, pts = round((PTS - DTS)*?H264_SCALE)}
  end, Durations, RevVideo).

dts_to_durations(Frames) ->
  dts_to_durations(Frames, []).

dts_to_durations([#video_frame{dts = DTS1}, #video_frame{dts = DTS2} = F|ReverseFrames], []) when DTS1 > DTS2 ->
  Duration = round((DTS1 - DTS2)*?H264_SCALE),
	dts_to_durations([F|ReverseFrames], [Duration, Duration]);

dts_to_durations([#video_frame{dts = DTS, pts = PTS}, #video_frame{dts = DTS1} = F|ReverseFrames], Acc) when DTS =< DTS1->
	dts_to_durations([F#video_frame{dts = DTS - 1, pts = PTS - DTS1 + DTS}|ReverseFrames], [round(?H264_SCALE) | Acc]);

dts_to_durations([#video_frame{dts = DTS1}, #video_frame{dts = DTS2}], Acc) when DTS1 > DTS2 ->
	[round((DTS1 - DTS2)*?H264_SCALE) | Acc];

dts_to_durations([#video_frame{dts = DTS1}, #video_frame{dts = DTS2} = F|ReverseFrames], Acc) when DTS1 > DTS2 ->
	dts_to_durations([F|ReverseFrames], [round((DTS1 - DTS2)*?H264_SCALE) | Acc]).





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
  
pack_elst(#writer_track{duration = Duration}) ->
  MediaTime = 2002, 
  MediaRate = 1,
  MediaFrac = 0,
  <<1:32, Duration:32, MediaTime:32, MediaRate:16, MediaFrac:16>>.

pack_chunk_sizes(_VChunks) ->
  <<0:32, 1:32, 1:32, 1:32, 1:32>>.
 
pack_chunk_offsets(VChunks) ->
  [<<0:32, (length(VChunks)):32>>, [<<Offset:32>> || #video_frame{body = {Offset,_}} <- lists:reverse(VChunks)]].
 


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
  NextTrackId = length(Convertor#convertor.tracks) + 1,
  <<0:32, CTime:32, MTime:32, TimeScale:32, Duration:32, Rate:16, RateDelim:16,
        Volume, VolumeDelim, 0:16, Reserved1:64, Matrix:36/binary, Reserved2:24/binary, NextTrackId:32>>.



pack_tkhd(#writer_track{duration = Duration, config = Config, content = Content, id = TrackID}) ->
	Flags = 15,
	CTime = mp4_now(),
	MTime = mp4_now(),
	Reserved1 = 0,
	Reserved2 = 0,
	Layer = 0,
	AlternateGroup = 0,
	Volume = case Content of
	  video -> 0;
	  audio -> 1
	end,
	VolDelim = 0,
  Reserved3 = 0,
  Matrix = <<0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0>>,
  
  
  {Width, Height} = case Content of
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

pack_config(#writer_track{config = undefined, frames = [#video_frame{codec = speex}|_]}) ->
  {'spx ', <<>>};

pack_config(#writer_track{config = undefined} = T) ->
  ?D({"no config on track", T#writer_track.id, T#writer_track.content}),
  <<>>;


pack_config(#writer_track{content = audio, config = Config, frames = [#video_frame{codec = Codec}|_]}) when Codec == mp3 orelse Codec == aac ->
  Reserved = <<0,0,0,0,0,0>>,
  RefIndex = 1,
  SoundVersion = 0,
  Unknown = <<0,0,0,0,0,0>>,

  {ObjectType, ChannelsCount} = case Codec of
    aac ->
      #aac_config{channel_count = AACChannels, sample_rate = SampleRate} = aac:decode_config(Config),
      {64, AACChannels};
    mp3 ->
      #mp3_frame{channels = Channels, sample_rate = SampleRate} = Config,
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
            CompressionId:16, PacketSize:16, SampleRate:16, 0:16>>,
            
  DescrTag = case Codec of
    aac -> [ConfigDescr, {?MP4DecSpecificDescrTag, Config}];
    mp3 -> ConfigDescr
  end,
  ESDS = {?MP4ESDescrTag, [ESDescr,
     {?MP4DecConfigDescrTag, DescrTag},
     {?MP4Unknown6Tag, <<2>>}]
   },
   
  {mp4a, [MP4A,{esds, [<<0:32>>, esds_serialize(ESDS)]}]};




pack_config(#writer_track{content = video, config = Config}) ->
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

pack_durations([]) ->	
  <<0:32, 0:32>>;
  
pack_durations(ReverseFrames) ->
	Durations1 = lists:reverse([D || #video_frame{dts = D} <- ReverseFrames]),
  % Durations2 = normalize_durations(Durations1, Content),
	Durations = Durations1,

  List1 = collapse_durations(Durations),
  List = [<<Count:32, Duration:32>> || {Count,Duration} <- List1],
	[<<0:32, (length(List)):32>>, List].

collapse_durations([Duration|Durations]) ->
  collapse_durations(Durations, 1, Duration, []).

collapse_durations([], Count, Duration, Acc) ->
  lists:reverse([{Count,Duration}|Acc]);
  
collapse_durations([Duration|Durations], Count, Duration, Acc) ->
  collapse_durations(Durations, Count + 1, Duration, Acc);

collapse_durations([NewDuration|Durations], Count, Duration, Acc) ->
  collapse_durations(Durations, 1, NewDuration, [{Count,Duration}|Acc]).

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


is_ctts_required(ReverseFrames) ->
  length([true || #video_frame{dts = DTS, pts = PTS} <- ReverseFrames, PTS - DTS > 1]) > 0.

pack_compositions(ReverseFrames) ->
  [<<0:32, (length(ReverseFrames)):32>>, compositions(ReverseFrames, [])].
  
  
compositions([], Acc) ->
  Acc;
% Here already PTS - DTS * Scale is stored  
compositions([#video_frame{pts = PTS}|Frames], Acc) ->
  compositions(Frames, [<<1:32, PTS:32>>|Acc]).

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
  ?assertEqual(<<0:32, 3:32, 1:32, 24:32, 1:32, 24:32, 1:32, 24:32>>, iolist_to_binary(pack_durations(Frames))).

% pack_glue_durations_test() ->
%   Frames = [#video_frame{dts = 2, pts = 2, content = video}, #video_frame{dts = 2, pts = 1, content = video}, #video_frame{dts = 0, pts = 0, content = video}],
%   ?assertEqual(<<0:32, 3:32, 1:32, 24:32, 1:32, 24:32, 1:32, 24:32>>, iolist_to_binary(pack_durations(Frames))).


pack_keyframes_test() ->
  Frames = [#video_frame{}, #video_frame{}, #video_frame{flavor = keyframe}, #video_frame{}, #video_frame{flavor = keyframe}],
  ?assertEqual(<<0:32, 2:32, 1:32, 3:32>>, iolist_to_binary(pack_keyframes(Frames))).
  
