%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2010 Max Lapshin
%%% @doc        MP4 decoding module
%%% 
%%% This is how ttxt subtitle track is translated
%%% [<<"onCuePoint">>,
%%%     [{<<"name">>,<<"onCuePoint">>},
%%%     {<<"time">>,16.0},
%%%     {<<"parameters">>,
%%%     [{<<"text">>, <<"But then you see the light...">>}]},
%%%      {<<"type">>,<<"navigation">>}]]
%%% 
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
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

-module(mp4).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../include/mp4.hrl").
-include("../include/srt.hrl").
-include("../include/video_frame.hrl").
-include("log.hrl").

-export([ftyp/2, moov/2, mvhd/2, trak/2, tkhd/2, mdia/2, mdhd/2, stbl/2, stsd/2, esds/2, avcC/2]).
-export([btrt/2, stsz/2, stts/2, stsc/2, stss/2, stco/2, co64/2, smhd/2, minf/2, ctts/2, udta/2]).
-export([mp4a/2, mp4v/2, avc1/2, s263/2, samr/2, free/2, wave/2]).
-export([hdlr/2, vmhd/2, dinf/2, dref/2, 'url '/2, 'pcm '/2, 'spx '/2, '.mp3'/2]).
-export([meta/2, ilst/2, covr/2, data/2, nam/2, alb/2]).

-export([abst/2, asrt/2, afrt/2]).

-export([extract_language/1,get_coverart/1]).
-export([parse_atom/2]).
-export([video_frame/2, video_frame/3]).

-record(esds, {
  object_type,
  stream_type,
  upstream,
  buffer_size,
  max_bitrate,
  avg_bitrate,
  specific
}).


-export([mp4_desc_length/1, open/2, read_frame/2, frame_count/1, seek/4, seek/5, mp4_read_tag/1]).
-export([keyframes/3]).
-export([dump/1]).

-define(FRAMESIZE, 32).


dump(Path) when is_list(Path) ->
  {ok, Bin} = file:read_file(Path),
  dump(Bin);
  
dump(Bin) when is_binary(Bin) ->
  dump_list(Bin, 0).

dump_list(<<>>, _) ->
  ok;

dump_list(Bin, Indent) ->
  Rest = dump_atom(Bin, Indent),
  dump_list(Rest, Indent).

dump_atom(<<1:32, Atom:4/binary, Length:64, Data/binary>>, Indent) ->
  AtomLen = Length - 16,
  <<Content:AtomLen/binary, Rest/binary>> = Data,
  dump_atom(binary_to_atom(Atom, latin1), Content, Indent),
  Rest;
  

dump_atom(<<Length:32, Atom:4/binary, Data/binary>>, Indent) when Length =/= 1 ->
  AtomLen = Length - 8,
  <<Content:AtomLen/binary, Rest/binary>> = Data,
  dump_atom(binary_to_atom(Atom, latin1), Content, Indent),
  Rest;

dump_atom(<<>>, _) ->
  <<>>.
  

dump_atom(abst = Atom, Content, Indent) ->
  {Format, Args, <<SegmentCount, SubContent/binary>>} = atom_dump_info(Atom, Content),
  indented_dump(Atom, Indent, Format, Args),
  SegmentCount == 1 orelse throw(non_one_segment_count),
  <<FragmentCount, FragmentTables/binary>> = lists:foldl(fun
    (0, Bin) -> Bin;
    (_N, Bin) -> dump_atom(Bin, Indent+1)
  end, SubContent, lists:seq(1, SegmentCount)),

  lists:foldl(fun
    (0, Bin) -> Bin;
    (_N, Bin) -> dump_atom(Bin, Indent+1)
  end, FragmentTables, lists:seq(1,FragmentCount));

dump_atom(Atom, Content, Indent) ->
  {Format, Args, SubContent} = atom_dump_info(Atom, Content),
  indented_dump(Atom, Indent, Format, Args),
  dump_list(SubContent, Indent+1).

indented_dump(Atom, Indent, Format, Args) ->
  Tab = [" " || _N <- lists:seq(1,Indent)],
  io:format(lists:flatten([Tab, "~p ", Format, "~n"]), [Atom|Args]).
  
  
atom_dump_info(abst, ABST) ->
  <<_Version, 0:24, _BootstrapVersion:32, Profile:2, Live:1, Update:1, 0:4, Timescale:32, Duration:64, _TimeOffset:64, 
    MovieId, ServerEntryCount, QualityCount, DrmData, MetaData, Rest/binary>> = ABST,
  MovieId == 0 orelse throw({abst,non_null_movie_id}),
  ServerEntryCount == 0 orelse throw({abst,server_entry}),
  QualityCount == 0 orelse throw({abst,quality}),
  DrmData == 0 orelse throw({abst,drm}),
  MetaData == 0 orelse throw({abst,meta}),
  {"profile=~p,live=~p,update=~p,timescale=~p,duration=~p", [Profile, Live, Update, Timescale,Duration], Rest};

atom_dump_info(asrt, <<0:32, QualityCount, SegmentCount:32, ASRT/binary>>) ->
  QualityCount == 0 orelse throw({asrt,non_zero_quality}),
  
  {Rest, Seg1} = lists:foldl(fun(_N, {<<FirstSegment:32, Fragments:32, R/binary>>, Acc}) -> 
    {R, [{FirstSegment,Fragments}|Acc]} 
  end, {ASRT, []}, lists:seq(1, SegmentCount)),
  Segments = lists:reverse(Seg1),
  
  {"segments=~p,~p", [SegmentCount, Segments], Rest};

atom_dump_info(afrt, <<0:32, Timescale:32, QualityCount, TotalFragments:32, Rest/binary>>) ->
  {_Rest, Info} = dump_afrt_fragments(Rest, TotalFragments, []),
  {"timescale=~p,quality_count=~p,total_fragments=~p,~p ~n    (~p)~n    rest=~p", [Timescale, QualityCount, TotalFragments, length(Info), Info, _Rest], <<>>};

atom_dump_info(afra, <<0:32, _LongIDs:1, LongOffsets:1, GlobalEntries:1, _:5, _Timescale:32, EntryCount:32, Rest1/binary>>) ->
  {_Rest2, AfraEntries} = dump_afra_entries(Rest1, LongOffsets, EntryCount, []),
  {"global_entries=~p,afra_entries=~p (~p)", [GlobalEntries,EntryCount, AfraEntries], <<>>};

atom_dump_info(moof, Rest) ->
  {"", [], Rest};

atom_dump_info(mfhd, <<0:32, SequenceNumber:32>>) ->
  {"sequence=~p", [SequenceNumber], <<>>};

atom_dump_info(traf, Rest) ->
  {"", [], Rest};

atom_dump_info(tfhd, <<0, Flags:24, TrackId:32, Rest/binary>>) ->
  <<_:7, DurationEmpty:1, _, _:2, SampleFlagsPresent:1, SampleSizePresent:1, SampleDurationPresent:1, _:1, 
  DescriptionPresent:1, DataOffsetPresent:1>> = <<Flags:24>>,
  {BaseDataOffset, Rest1} = extract_number(Rest, 64, DataOffsetPresent),
  {SampleDescriptionIndex, Rest2} = extract_number(Rest1, 32, DescriptionPresent),
  {DefaultSampleDuration, Rest3} = extract_number(Rest2, 32, SampleDurationPresent),
  {DefaultSampleSize, Rest4} = extract_number(Rest3, 32, SampleSizePresent),
  {DefaultSampleFlags, _Rest5} = extract_number(Rest4, 32, SampleFlagsPresent),
  
  {"duration_empty=~p,flags=~p,size=~p,duration=~p,description=~p,offset=~p,track=~p", [
    DurationEmpty, DefaultSampleFlags, DefaultSampleSize, DefaultSampleDuration, SampleDescriptionIndex, BaseDataOffset, TrackId], <<>>};

atom_dump_info(trun, <<0, Flags:24, SampleCount:32, Rest1/binary>>) ->
  <<_:16, _:5, FirstSampleFlagPresent:1, _:1, DataOffsetPresent:1>> = <<Flags:24>>,
  {DataOffset, Rest2} = extract_number(Rest1, 32, DataOffsetPresent),
  {_FirstSampleFlag, Rest3} = extract_number(Rest2, 32, FirstSampleFlagPresent),
  TrunEntries = dump_trun_entries(Rest3, Flags, SampleCount, []),
  
  Dump = [io_lib:format("~n       duration=~p,size=~p,flags=~p,ctime=~p", [D,S,F,C]) || [{duration,D},{size,S},{flags,F},{ctime,C}] <- TrunEntries],
  {"sample_count=~p,data_offset=~p~s", [SampleCount, DataOffset, lists:flatten(Dump)], <<>>};
      
atom_dump_info(mdat, <<Info:10/binary, _/binary>> = Content) ->
  file:write_file("mmm.flv", <<(flv:header())/binary, Content/binary>>),
  {"mdat=~p,~p", [size(Content), Info], <<>>};
  
atom_dump_info(_, _) ->
  {"", [], <<>>}.

extract_number(Bin, _Size, 0) -> {undefined, Bin};
extract_number(Bin, Size, _) -> <<Data:Size, R0/binary>> = Bin, {Data, R0}.


dump_afrt_fragments(Rest, TotalFragments, List) when length(List) >= TotalFragments ->
  {Rest, lists:reverse(List)};

dump_afrt_fragments(<<Frag:32, Time:64, 0:32, _Discontinuity, Rest/binary>>, TotalFragments, List) ->
  dump_afrt_fragments(Rest, TotalFragments, [{Frag,Time,0}|List]);
  
dump_afrt_fragments(<<Frag:32, Time:64, Duration:32, Rest/binary>>, TotalFragments, List) ->
  dump_afrt_fragments(Rest, TotalFragments, [{Frag,Time,Duration}|List]).


dump_afra_entries(Rest, _, 0, List) ->
  {Rest, lists:reverse(List)};

dump_afra_entries(<<Time:64, Offset:32, Rest/binary>>, 0, Count, List) ->
  dump_afra_entries(Rest, 0, Count - 1, [{Time,Offset}|List]);

dump_afra_entries(<<Time:64, Offset:64, Rest/binary>>, 1, Count, List) ->
  dump_afra_entries(Rest, 1, Count - 1, [{Time,Offset}|List]).


dump_trun_entries(_, _, 0, List) ->
  lists:reverse(List);
  
dump_trun_entries(Bin, Flags, SampleCount, List) ->
  {SampleDuration, Rest1} = extract_number(Bin, 32, Flags band 16#0100),
  {SampleSize, Rest2} = extract_number(Rest1, 32, Flags band 16#0200),
  {SampleFlags, Rest3} = extract_number(Rest2, 32, Flags band 16#0400),
  {SampleCompositionTimeOffset, Rest3} = extract_number(Rest2, 32, Flags band 16#0800),
  Info = [{duration,SampleDuration},{size,SampleSize},{flags,SampleFlags},{ctime,SampleCompositionTimeOffset}],
  dump_trun_entries(Rest3, Flags, SampleCount - 1, [Info|List]).

open(Reader, Options) ->
  {_T1, {ok, Mp4Media}} = timer:tc(fun() -> read_header(Reader) end),
  #mp4_media{tracks = Tracks} = Mp4Media1 = read_srt_files(Mp4Media, proplists:get_value(url, Options)),
  {_T2, Index} = timer:tc(fun() -> build_index(Tracks) end),
  % ?D({mp4_init, {read_header,T1},{build_index,T2}}),
  {ok, Mp4Media1#mp4_media{index = Index, reader = Reader, tracks = list_to_tuple(Tracks)}}.

get_coverart(Reader) ->
  {ok, MP4_Media} = read_header(#mp4_media{}, Reader, 0),
  case proplists:get_value(cover_art,MP4_Media#mp4_media.additional,undefined) of
    undefined -> <<>>;
    Bin -> Bin
  end.

read_header(Reader) ->
  read_header(#mp4_media{}, Reader, 0).


read_srt_files(Media, Url) when is_binary(Url) ->
  read_srt_files(Media, binary_to_list(Url));

read_srt_files(Media, Url) ->
  case filelib:is_file(Url) of
    true ->
      Wildcard = filename:dirname(Url) ++ "/" ++ filename:basename(Url, ".mp4") ++ ".*" ++ ".srt",
      lists:foldl(fun(SrtFile, Mp4Media) ->
         read_srt_file(Mp4Media, SrtFile)
      end, Media, filelib:wildcard(Wildcard));
    _ ->
      Media
  end.


read_srt_file(#mp4_media{tracks = Tracks, duration = Duration} = Media, SrtFile) ->
  {match,[Lang]} = re:run(SrtFile, "\\.(\\w+)\\.srt", [{capture,all_but_first,list}]),
  Subtitles = parse_srt_file(SrtFile),
  SrtFrames = subtitles_to_mp4_frames(Subtitles),
  Track = #mp4_track{codec = srt, content = text, track_id = length(Tracks)+1, timescale = 1, 
  duration = Duration, language = list_to_binary(Lang), frames = SrtFrames},
  Media#mp4_media{tracks = Tracks ++ [Track]}.
  
parse_srt_file(File) ->
  % (11:41:23 PM) max lapshin: AccessModule чаще всего — file
  % (11:41:31 PM) max lapshin: но в принципе может быть и http_file
  % (11:41:50 PM) max lapshin: поэтому надо сделать такую проверку:
  % parsed_srt_tracks({file, _}, Options) ->
  {ok, Data} = file:read_file(File),
  {ok, Subtitles, _More} = srt_parser:parse(Data),
  Subtitles.

subtitles_to_mp4_frames(Subtitles) ->
  [#mp4_frame{id = Id, dts = From, pts = To, size = size(Text), codec = srt, body = Text, content = text} ||
   #srt_subtitle{id = Id, from = From, to = To, text = Text} <- Subtitles].

read_header(#mp4_media{additional = Additional} = Mp4Media, {Module, Device} = Reader, Pos) -> 
  case read_atom_header(Reader, Pos) of
    eof -> {ok, Mp4Media};
    {error, Reason} -> {error, Reason};
    {atom, mdat, _Offset, all_file} ->
      {ok, Mp4Media};
    {atom, mdat, Offset, Length} ->
      read_header(Mp4Media, Reader, Offset + Length);
    {atom, _AtomName, Offset, 0} -> 
      read_header(Mp4Media, Reader, Offset);
    {atom, AtomName, Offset, Length} -> 
      % ?D({"Root atom", AtomName, Length}),
      {ok, AtomData} = Module:pread(Device, Offset, Length),
      % ?D({atom, AtomName, size(AtomData)}),
      NewMedia = case atom_to_binary(AtomName, latin1) of
        <<"EV", _/binary>> ->
          Mp4Media#mp4_media{additional = [{AtomName,AtomData}| Additional]};
        _ ->
          case erlang:function_exported(mp4, AtomName, 2) of
            true -> mp4:AtomName(AtomData, Mp4Media);
            false -> Mp4Media
          end
      end,
      read_header(NewMedia, Reader, Offset + Length)
  end.

read_atom_header({Module, Device}, Pos) ->
  case Module:pread(Device, Pos, 8) of
    {ok, <<0:32, AtomName/binary>>} ->
      {atom, binary_to_atom(AtomName, latin1), Pos + 8, all_file};
    {ok, <<1:32, AtomName/binary>>} ->
      case Module:pread(Device, Pos+4, 12) of
        {ok, <<AtomName:4/binary, AtomLength:64>>} when AtomLength >= 12 -> 
          {atom, binary_to_atom(AtomName, latin1), Pos + 16, AtomLength - 16};
        eof ->
          eof;
        {ok, Bin} ->
          ?D({invalid_atom, Bin}),
          {error, {invalid_atom, Bin}};
        {error, Error} ->
          {error, Error}
      end;
    {ok, <<AtomLength:32, AtomName/binary>>} when AtomLength >= 8 ->
      % ?D({"Atom", binary_to_atom(AtomName, latin1), Pos, AtomLength}),
      {atom, binary_to_atom(AtomName, latin1), Pos + 8, AtomLength - 8};
    eof ->
      eof;
    {ok, Bin} ->
      ?D({invalid_atom, Bin}),
      {error, {invalid_atom, Bin}};
    {error, Error} ->
      {error, Error}
  end.

keyframes(#mp4_media{} = Media, Audio, Video) ->
  keyframes(Media, Audio, Video, 0, -1, []).

keyframes(Media, Audio, Video, Id, PrevDTS, Keyframes) ->
  case read_frame(Media, #frame_id{id = Id, a = Audio, v = Video}) of
    #mp4_frame{keyframe = true, dts = DTS} when DTS > PrevDTS ->
      keyframes(Media, Audio, Video, Id + 1, DTS, [{DTS,Id}|Keyframes]);
    #mp4_frame{} ->
      keyframes(Media, Audio, Video, Id + 1, PrevDTS, Keyframes);
    eof ->
      lists:reverse(Keyframes)
  end.


seek(#mp4_media{} = Media, Audio, Video, Timestamp) ->
  seek(Media, Audio, Video, Timestamp, keyframe).

seek(#mp4_media{} = Media, Audio, undefined, Timestamp, keyframe) ->
  seek(Media, Audio, undefined, Timestamp, frame);

seek(#mp4_media{} = Media, Audio, Video, Timestamp, SeekMode) ->
  A = case SeekMode of
    keyframe -> undefined;
    _ -> Audio
  end,
  seek(Media, A, Video, Timestamp, 0, undefined, SeekMode).

seek(Media, Audio, Video, Timestamp, Id, Found, SeekMode) ->
  case read_frame(Media, #frame_id{id = Id, a = Audio, v = Video}) of
    #mp4_frame{dts = DTS} when DTS >= Timestamp andalso SeekMode == frame -> {Id,DTS};
    #mp4_frame{keyframe = true, dts = DTS} when DTS > Timestamp andalso SeekMode == keyframe -> Found;
    #mp4_frame{keyframe = true, dts = DTS} when SeekMode == keyframe -> seek(Media, Audio, Video, Timestamp, Id+1, {Id,DTS}, SeekMode);
    #mp4_frame{dts = DTS} when SeekMode == frame -> seek(Media, Audio, Video, Timestamp, Id+1, {Id, DTS}, SeekMode);
    #mp4_frame{dts = _DTS} -> seek(Media, Audio, Video, Timestamp, Id+1, Found, SeekMode);
    eof -> undefined
  end.


read_frame(#mp4_media{tracks = Tracks}, {track, TrackId, config}) ->
  #mp4_track{content = Content, codec = Codec, decoder_config = Config} = element(TrackId, Tracks),
  #video_frame{
   	content = Content,
   	flavor  = config,
		dts     = 0,
		pts     = 0,
		body    = Config,
		codec   = Codec
	};

read_frame(#mp4_media{tracks = Tracks} = Media, {track, TrackId, Id}) ->
  #mp4_track{content = Content} = Track = element(TrackId, Tracks),
  case unpack_frame(Track, Id) of
    eof -> eof;
    Mp4Frame -> video_frame(Media, Mp4Frame#mp4_frame{content = Content})
  end;

read_frame(#mp4_media{tracks = Tracks, index = Index} = Media, #frame_id{id = Id,a = Audio,v = Video, t = Text} = FrameId) ->
  IndexOffset = Id*4,
  
  case Index of
    <<_:IndexOffset/binary, Audio, _:1, AudioId:23, _/binary>> -> 
      (unpack_frame(element(Audio,Tracks), AudioId))#mp4_frame{next_id = FrameId#frame_id{id = Id+1}, content = audio};
    <<_:IndexOffset/binary, Text, _:1, TextId:23, _/binary>> -> 
       %?D({read_text,Text,TextId}),
      (unpack_frame(element(Text,Tracks), TextId))#mp4_frame{next_id = FrameId#frame_id{id = Id+1}, content = text};
    <<_:IndexOffset/binary, Video, _:1, VideoId:23, _/binary>> -> 
      (unpack_frame(element(Video,Tracks), VideoId))#mp4_frame{next_id = FrameId#frame_id{id = Id+1}, content = video};
    <<_:IndexOffset/binary>> -> 
      eof;
    <<_:IndexOffset/binary, _OtherTrackId, _K:1, _FrameIndex:23, _/binary>> ->
      read_frame(Media, FrameId#frame_id{id = Id+1})
  end.



read_data(#mp4_media{reader = {M, Dev}}, Offset, Size) ->
  case M:pread(Dev, Offset, Size) of
    {ok, Data} ->
      {ok, Data};
    Else -> Else
  end.


video_frame(#mp4_media{} = Media, #mp4_frame{offset = Offset, size = Size, content = Content, next_id = Next} = Frame) ->
  case read_data(Media, Offset, Size) of
		{ok, Data} ->
		  VideoFrame = video_frame(Content, Frame, Data),
		  VideoFrame#video_frame{next_id = Next};
    eof -> eof;
    {error, Reason} -> {error, Reason}
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

video_frame(text, #mp4_frame{dts = DTS, pts = PTS, codec = Codec}, Data) ->
  #video_frame{
   	content = metadata,
		dts     = DTS,
		pts     = DTS,
		flavor  = frame,
		codec   = Codec,
		body    = [<<"onTextData">>, {object, [
		  {name, onCuePoint},
		  {type, event},
		  {'begin', DTS},
  		{'end', PTS},
		  {text, Data}
		]}]
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

  
  
unpack_frame(#mp4_track{frames = Frames, content = text, codec = _Codec}, Id) when Id < length(Frames) ->
  lists:nth(Id+1, Frames);
  
unpack_frame(#mp4_track{frames = Frames, codec = Codec}, Id) when Id*?FRAMESIZE < size(Frames) ->
  FrameOffset = Id*?FRAMESIZE,

  <<_:FrameOffset/binary, FKeyframe:1, Size:63, Offset:64, DTS:64/float, PTS:64/float, _/binary>> = Frames,
  Keyframe = case FKeyframe of
    1 -> true;
    0 -> false
  end,
  #mp4_frame{id = Id, dts = DTS, pts = PTS, size = Size, offset = Offset, keyframe = Keyframe, codec = Codec};
  
unpack_frame(#mp4_track{frames = Frames}, Id) when Id*?FRAMESIZE == size(Frames) -> eof.


frame_count(undefined) -> 0;
frame_count(#mp4_track{frames = Frames}) -> size(Frames) div ?FRAMESIZE;
frame_count(Frames) -> size(Frames) div ?FRAMESIZE.


parse_atom(<<>>, State) ->
  State;

parse_atom(Bin, State) ->
  {ok, _Atom, NewState, Rest} = decode_atom(Bin, State),
  parse_atom(Rest, NewState).

decode_atom(<<8:32, 0:32>>, Mp4Parser) ->
  Mp4Parser;
  
decode_atom(<<AllAtomLength:32, BinaryAtomName:4/binary, AtomRest/binary>>, Mp4Parser) when (size(AtomRest) >= AllAtomLength - 8) ->
  AtomLength = AllAtomLength - 8,
  <<Atom:AtomLength/binary, Rest/binary>> = AtomRest,
  % ?D({atom,BinaryAtomName}),
  AtomName = case binary:bin_to_list(BinaryAtomName) of
   % [169|Value] -> 
   %   binary_to_atom(binary:list_to_bin(Value),latin1);
   _ValidValue ->
     binary_to_atom(BinaryAtomName,latin1)
  end,
  NewMp4Parser = case erlang:function_exported(?MODULE, AtomName, 2) of
    true -> ?MODULE:AtomName(Atom, Mp4Parser);
    false -> Mp4Parser
    % false -> Mp4Parser
  end,
  {ok, AtomName, NewMp4Parser, Rest};

decode_atom(<<0:32>>, Mp4Parser) ->
  ?D("NULL atom"),
  Mp4Parser.


%% Adobe bootstrap

abst(<<_Version, 0:24, InfoVersion:32, Profile:2, Live:1, Update:1, 0:4, 
       TimeScale:32, CurrentMediaTime:64, _SmpteTimeCodeOffset:64, Rest1/binary>>, State) ->
  {IdLen, _} = binary:match(Rest1, [<<0>>]),
  <<MovieIdentifier:IdLen/binary, 0, Rest2/binary>> = Rest1,
  <<ServerEntryCount, QualityEntryCount, DrmData, MetaData, SegmentRunTableCount, Rest3/binary>> = Rest2,
  ServerEntryCount = 0,
  QualityEntryCount = 0,
  DrmData = 0,
  MetaData = 0,
  SegmentRunTableCount = 1,
  {ok, asrt, SegmentRunTable, <<FragmentRunTableCount, Rest4/binary>>} = decode_atom(Rest3, State),
  FragmentRunTableCount = 1,
  {ok, afrt, FragmentRunTable, <<>>} = decode_atom(Rest4, State),
  {'Bootstrap', [{version,InfoVersion},{profile,Profile},{live,Live},{update,Update},{timescale,TimeScale},{current_time,CurrentMediaTime},
  {id,MovieIdentifier},{segments,SegmentRunTable},{fragments, FragmentRunTable}], Rest4}.


asrt(<<_Version, Update:24, QualityEntryCount, SegmentRunEntryCount:32, Rest/binary>>, _State) ->
  QualityEntryCount = 0,
  Segments = [{FirstSegment, FragmentsPerSegment} || <<FirstSegment:32, FragmentsPerSegment:32>> <= Rest],
  SegmentRunEntryCount = length(Segments),
  {'SegmentRunTable', [{update,Update}], Segments}.

afrt(<<_Version, Update:24, TimeScale:32, QualityEntryCount, FragmentRunEntryCount:32, Rest/binary>>, _State) ->
  QualityEntryCount = 0,
  Fragments = read_afrt_fragments(Rest),
  FragmentRunEntryCount = length(Fragments),
  {'FragmentRunEntry', [{update,Update},{timescale,TimeScale}],Fragments}.
  
read_afrt_fragments(Bin) -> read_afrt_fragments(Bin, []).

read_afrt_fragments(<<FirstFragment:32, FirstFragmentTimestamp:64, 0:32, DiscontinuityIndicator, Rest/binary>>, Acc) ->
   read_afrt_fragments(Rest, [{FirstFragment, FirstFragmentTimestamp, 0, DiscontinuityIndicator}|Acc]);

read_afrt_fragments(<<FirstFragment:32, FirstFragmentTimestamp:64, FragmentDuration:32, Rest/binary>>, Acc) ->
  read_afrt_fragments(Rest, [{FirstFragment, FirstFragmentTimestamp, FragmentDuration}|Acc]);

read_afrt_fragments(_, Acc) ->
  lists:reverse(Acc).



% FTYP atom
ftyp(<<_Major:4/binary, _Minor:4/binary, _CompatibleBrands/binary>>, MediaInfo) ->
  % ?D({"File", _Major, _Minor, ftyp(_CompatibleBrands, [])}),
  % NewParser = Mp4Parser#mp4_header{file_type = binary_to_list(Major), file_types = decode_atom(ftyp, CompatibleBrands, [])},
  MediaInfo;

ftyp(<<>>, BrandList) when is_list(BrandList) ->
  lists:reverse(BrandList);

ftyp(<<Brand:4/binary, CompatibleBrands/binary>>, BrandList) ->
  ftyp(CompatibleBrands, [Brand|BrandList]).
  
% Movie box
moov(Atom, MediaInfo) ->
  Media = #mp4_media{tracks = Tracks} = parse_atom(Atom, MediaInfo),
  Media#mp4_media{tracks = lists:reverse(Tracks)}.

free(_Atom, Media) ->
  Media.

% MVHD atom
mvhd(<<0:32, CTime:32, MTime:32, TimeScale:32, Duration:32, Rate:16, _RateDelim:16,
      Volume:16, 0:16, _Reserved1:64, Matrix:36/binary, _Reserved2:24/binary, NextTrackId:32>>, #mp4_media{} = Media) ->
        
  _Meta = [{ctime,CTime},{mtime,MTime},{timescale,TimeScale},{duration,Duration},{rate,Rate},
          {volume,Volume},{matrix,Matrix},{next_track,NextTrackId}],
  % ?D(Meta),
  Media#mp4_media{timescale = TimeScale, duration = Duration*1000 div TimeScale}.

udta(UDTA, Media) ->
  parse_atom(UDTA, Media).

meta(<<0:32, Meta/binary>>, #mp4_media{} = Media) ->
  parse_atom(Meta, Media);

meta(_, #mp4_media{} = Media) ->
  Media.

ilst(ILST, #mp4_media{} = Media) ->
  parse_atom(ILST, Media).

covr(Data, #mp4_media{additional = Add} = Media) ->
  CoverArt = parse_atom(Data, covr),
  Media#mp4_media{additional = [{cover_art, CoverArt}|Add]}.

nam(Data, #mp4_media{additional = Add} = Media) ->
  Name = parse_atom(Data, name),
  Metadata = case proplists:get_value(name, Add, undefined) of
    undefined ->
      Name;
    AnotherData -> 
      <<AnotherData/binary,"-",Name/binary>> 
  end,
  Media#mp4_media{additional = [{name, Metadata}|Add]}.

alb(Data, #mp4_media{additional = Add} = Media) ->
  Name = parse_atom(Data, name),
  Metadata = case proplists:get_value(name, Add, undefined) of
    undefined ->
      Name;
    AnotherData -> 
      <<AnotherData/binary,"-",Name/binary>> 
  end,
  Media#mp4_media{additional = [{name, Metadata}|Add]}.

data(<<_Flags:32, 0:32, Data/binary>>, _Meaning) ->
  Data.

% '----'(Meta, #mp4_media{} = Media) ->

% Track box
trak(<<>>, MediaInfo) ->
  MediaInfo;
  
trak(Atom, #mp4_media{tracks = Tracks} = MediaInfo) ->
  {_T1, Track} = timer:tc(fun() -> parse_atom(Atom, #mp4_track{number = length(Tracks)+1}) end),
  case Track#mp4_track.codec of
    undefined -> ?D({skip_mp4_track, undefined_codec}),MediaInfo;
    _ -> 
    {_T2, R} = timer:tc(fun() -> fill_track_info(MediaInfo, Track) end),
    % ?D({trak, {parse_atom,T1},{fill_info,T2}}),
    R
  end.



clean_track(#mp4_track{} = Track) ->
  Track#mp4_track{sample_sizes = [], sample_dts = [], sample_offsets = [], sample_composition = [],
                  keyframes = [], chunk_offsets = [], chunk_sizes = []}.

append_track(#mp4_media{tracks = Tracks} = MediaInfo, 
             #mp4_track{content = video, width = Width, height = Height} = Track)  ->
  MediaInfo#mp4_media{width = Width, height = Height, tracks = [clean_track(Track)|Tracks]};

append_track(#mp4_media{tracks = Tracks} = MediaInfo, #mp4_track{} = Track) ->
  MediaInfo#mp4_media{tracks = [clean_track(Track)|Tracks]}.


fill_track_info(#mp4_media{} = MediaInfo, #mp4_track{} = Track) ->
  {_T1, Track1} = timer:tc(fun() -> fill_track(Track) end),
  % ?D({fill_track_info, T1}),
  append_track(MediaInfo, Track1).

  

% Track header
tkhd(<<0, Flags:24, CTime:32, MTime:32, TrackID:32, _Reserved1:32, 
       Duration:32, _Reserved2:64, Layer:16, _AlternateGroup:16,
       Volume, _VolDelim, _Reserved3:16, Matrix:36/binary, Width:16, _WidthDelim:16, Height:16, _HeightDelim:16>>, Mp4Track) ->
  _Meta = [{flags,Flags},{ctime,CTime},{mtime,MTime},{track_id,TrackID},{duration,Duration},{layer,Layer},
         {volume,Volume},{matrix,Matrix},{width,Width},{height,Height}],
  % ?D(Meta),
  Mp4Track#mp4_track{track_id = TrackID, duration = Duration}.

% Media box
mdia(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

% Media header
mdhd(<<0:8, _Flags:24, _Ctime:32, _Mtime:32, TimeScale:32, Duration:32,
       _:1, Language:15/bitstring, _Quality:16>>, #mp4_track{} = Mp4Track) ->
  % ?D({"Timescale:", Duration, extract_language(_Language)}),
  Mp4Track#mp4_track{timescale = TimeScale, duration = Duration, language = extract_language(Language)};

mdhd(<<1:8, _Flags:24, _Ctime:64, _Mtime:64, TimeScale:32, Duration:64, 
       _:1, Language:15/bitstring, _Quality:16>>, Mp4Track) ->
  % ?D({"Timescale:", Duration, extract_language(_Language)}),
  Mp4Track#mp4_track{timescale = TimeScale, duration = Duration, language = extract_language(Language)}.
  
extract_language(<<L1:5, L2:5, L3:5>>) ->
  case list_to_binary([L1+16#60, L2+16#60, L3+16#60]) of
    <<"und">> -> undefined;
    Else -> Else
  end.


%% Handler Reference Box
hdlr(<<0:32, 0:32, _Handler:4/binary, _Reserved:8/binary, NameNull/binary>>, #mp4_media{} = Mp4Media) ->
  Len = (size(NameNull) - 1),
  _Name = case NameNull of
    <<N:Len/binary, 0>> -> N;
    _ -> NameNull
  end,
  % ?D({hdlr, Handler, Name}),
  Mp4Media;


hdlr(<<0:32, _Mhdl:32, "vide", _Reserved:8/binary, NameNull/binary>>, #mp4_track{} = Mp4Track) ->
  Len = (size(NameNull) - 1),
  _Name = case NameNull of
    <<N:Len/binary, 0>> -> N;
    _ -> NameNull
  end,
  Mp4Track#mp4_track{content = video};

hdlr(<<0:32, _Mhdl:32, "soun", _Reserved:8/binary, NameNull/binary>>, #mp4_track{} = Mp4Track) ->
  Len = (size(NameNull) - 1),
  _Name = case NameNull of
    <<N:Len/binary, 0>> -> N;
    _ -> NameNull
  end,
  Mp4Track#mp4_track{content = audio};

hdlr(<<0:32, 0:32, "hint", _Reserved:8/binary, NameNull/binary>>, #mp4_track{} = Mp4Track) ->
  Len = (size(NameNull) - 1),
  _Name = case NameNull of
    <<N:Len/binary, 0>> -> N;
    _ -> NameNull
  end,
  Mp4Track#mp4_track{content = hint};

hdlr(<<0:32, _Mhdl:32, Handler:4/binary, _Reserved:8/binary, NameNull/binary>>, #mp4_track{} = Mp4Track) ->
  Len = (size(NameNull) - 1),
  _Name = case NameNull of
    <<N:Len/binary, 0>> -> N;
    _ -> NameNull
  end,
  Content = case Mp4Track#mp4_track.content of
    undefined -> binary_to_atom(Handler, latin1);
    OldContent -> OldContent
  end,
  Mp4Track#mp4_track{content = Content}.
  
% SMHD atom
smhd(<<0:8, _Flags:3/binary, 0:16/big-signed-integer, _Reserve:2/binary>>, Mp4Track) ->
  Mp4Track;

smhd(<<0:8, _Flags:3/binary, _Balance:16/big-signed-integer, _Reserve:2/binary>>, Mp4Track) ->
  Mp4Track.

% Media information
minf(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

%% Video Media Header Box
vmhd(<<_Version:32, _Mode:16, _R:16, _G:16, _B:16>>, Mp4Track) ->
  % _VMHD = {vmhd, Version, Mode, R, G, B},
  % ?D(_VMHD),
  Mp4Track.
  
dinf(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).
  

dref(<<0:32, _Count:32, Atom/binary>> = _Dref, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

'url '(_URL, Mp4Track) ->
  Mp4Track.

% Sample table box
stbl(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

% Sample description
stsd(<<0:8, _Flags:3/binary, _EntryCount:32, EntryData/binary>>, Mp4Track) ->
  % ?D({_EntryCount, EntryData}),
  parse_atom(EntryData, Mp4Track).

mp4a(<<0:32>>, Mp4Track) ->
  Mp4Track;

mp4a(<<_Reserved:6/binary, _RefIndex:16, SoundVersion:16, _Unknown:6/binary, _ChannelsCount:16,
       _SampleSize:16, _PacketSize:16, _TimeScale:32, _Reserved3:16, Atom/binary>>, Mp4Track) when SoundVersion == 0 ->
  parse_atom(Atom, Mp4Track#mp4_track{codec = aac});

mp4a(<<_Reserved:6/binary, _RefIndex:16, SoundVersion:16, _Reserved2:6/binary, _ChannelsCount:16,
       _SampleSize:16, _Reserved3:16, _PacketSize:16, _TimeScale:16, _Reserved4:16, 
       _SamplesPerPacket:32, _BytesPerPacket:32, _BytesPerFrame:32, _BytesPerSample:32, Atom/binary>>, Mp4Track) when SoundVersion == 1 ->
  parse_atom(Atom, Mp4Track#mp4_track{codec = aac}).

wave(Atom, Mp4Track) ->
  parse_atom(Atom, Mp4Track).

mp4v(_T, Mp4Track) ->
  Mp4Track#mp4_track{codec = mpeg4}.


'.mp3'(_, Mp4Track) ->
  Mp4Track#mp4_track{codec = mp3}.


avc1(<<_Reserved:6/binary, _RefIndex:16, _Unknown1:16/binary, Width:16, Height:16,
      HorizRes:16, _:16, VertRes:16, _:16, _FrameCount:16, _CompressorName:32/binary,
      Depth:16, _Predefined:16, _Unknown:4/binary, Atom/binary>>, Mp4Track) ->
  _Meta = [{width,Width},{height,Height},
          {horiz_res, HorizRes},{vert_res, VertRes},
          {depth,Depth}],
  % ?D({"Video size:", Meta}),
  parse_atom(Atom, Mp4Track#mp4_track{codec = h264, width = Width, height = Height}).

s263(<<_Reserved:6/binary, _RefIndex:16, _Unknown1:16/binary, Width:16, Height:16,
       _HorizRes:32, _VertRes:32, _FrameCount:16, _CompressorName:32/binary, _Depth:16, 
       _Predefined:16, _Unknown:4/binary, Atom/binary>>, Mp4Track) ->
  % ?D({"Video size:", Width, Height}),
  parse_atom(Atom, Mp4Track#mp4_track{codec = s263, width = Width, height = Height}).

samr(<<_Reserved:2/binary, _RefIndex:16, Atom/binary>> = AMR, Mp4Track) ->
  ?D(AMR),
  parse_atom(Atom, Mp4Track#mp4_track{codec = samr}).


'pcm '(_, Mp4Track) ->
  ?D(pcm),
  Mp4Track#mp4_track{codec = pcm_le}.

'spx '(_, Mp4Track) ->
  ?D(pcm),
  Mp4Track#mp4_track{codec = speex}.

  
%%%%%%%%%%%%%%%%%%    ESDS     %%%%%%%%%%%%%%
esds(<<Version:8, _Flags:3/binary, DecoderConfig/binary>>, #mp4_track{} = Mp4Track) when Version == 0 ->
  % ?D({"Extracted audio config", DecoderConfig}),
  ESDS = config_from_esds_tag(DecoderConfig),
  % ?D(ESDS),
  Mp4Track#mp4_track{decoder_config = ESDS#esds.specific, codec = ESDS#esds.object_type}.

% avcC atom
avcC(DecoderConfig, #mp4_track{} = Mp4Track) ->
  % ?D({"Extracted video config", DecoderConfig, h264:unpack_config(DecoderConfig)}),
  Mp4Track#mp4_track{decoder_config = DecoderConfig}.

btrt(<<_BufferSize:32, MaxBitRate:32, AvgBitRate:32>>, #mp4_track{} = Mp4Track) ->
  ?D({btrt,MaxBitRate, AvgBitRate}),
  Mp4Track#mp4_track{max_bitrate = MaxBitRate, bitrate = AvgBitRate}.


%% FIXME: Code here must be relocated in some more generic place and way. 
%% Here goes not some esds tag, but IOD (Initial Object Description)
%% Look how to parse it at vlc/modules/demux/ts.c:2400
%%

config_from_esds_tag(Data) ->
  config_from_esds_tag(Data, #esds{}).

config_from_esds_tag(Data, ESDS) ->
  case mp4_read_tag(Data) of
    {?MP4ESDescrTag, <<_ID1:16, _Priority1, Description/binary>>, <<>>} ->
      config_from_esds_tag(Description, ESDS);
    {?MP4DecConfigDescrTag, <<ObjectType, StreamType, BufferSize:24, MaxBitrate:32, AvgBitrate:32, Rest1/binary>>, Rest2} ->
      ESDS1 = config_from_esds_tag(Rest1, ESDS#esds{
        object_type = mp4_object_type(ObjectType), stream_type = StreamType, buffer_size = BufferSize,
        max_bitrate = MaxBitrate, avg_bitrate = AvgBitrate}),
      config_from_esds_tag(Rest2, ESDS1);
    {?MP4DecSpecificDescrTag, <<Config/binary>>, _} ->
      ESDS#esds{specific = Config};
    {?MP4Unknown6Tag, _Body, Rest} ->
      config_from_esds_tag(Rest, ESDS);
    {_Tag, _Data, Rest} ->
      ?D({"Unknown esds tag. Send this line to max@maxidoors.ru: ", _Tag, _Data}),
      config_from_esds_tag(Rest, ESDS);
    undefined ->
      ESDS
  end.

mp4_object_type(8) -> text;
mp4_object_type(16#20) -> mpeg4;
mp4_object_type(16#21) -> h264;
mp4_object_type(16#40) -> aac;
mp4_object_type(16#60) -> mpeg2video;
mp4_object_type(16#61) -> mpeg2video;
mp4_object_type(16#62) -> mpeg2video;
mp4_object_type(16#63) -> mpeg2video;
mp4_object_type(16#64) -> mpeg2video;
mp4_object_type(16#65) -> mpeg2video;
mp4_object_type(16#66) -> aac;
mp4_object_type(16#67) -> aac;
mp4_object_type(16#68) -> aac;
mp4_object_type(16#69) -> mp3;
mp4_object_type(16#6A) -> mpeg1video;
mp4_object_type(16#6B) -> mp3;
mp4_object_type(16#6C) -> mjpeg;
mp4_object_type(16#6D) -> png;
mp4_object_type(16#6E) -> jpeg2000;
mp4_object_type(16#A3) -> vc1;
mp4_object_type(16#A4) -> dirac;
mp4_object_type(16#A5) -> ac3;
mp4_object_type(16#DD) -> vorbis;
mp4_object_type(16#E0) -> dvd_subtitle;
mp4_object_type(16#E1) -> qcelp.

mp4_desc_length(<<0:1, Length:7, Rest:Length/binary, Rest2/binary>>) ->
  {Rest, Rest2};

mp4_desc_length(<<1:1, Length1:7, 0:1, Length:7, Rest/binary>>) ->
  TagLength = Length1 * 128 + Length,
  <<Rest1:TagLength/binary, Rest2/binary>> = Rest,
  {Rest1, Rest2};

mp4_desc_length(<<1:1, Length2:7, 1:1, Length1:7, 0:1, Length:7, Rest/binary>>)  ->
  TagLength = (Length2 bsl 14 + Length1 bsl 7 + Length),
  <<Rest1:TagLength/binary, Rest2/binary>> = Rest,
  {Rest1, Rest2};

mp4_desc_length(<<1:1, Length3:7, 1:1, Length2:7, 1:1, Length1:7, 0:1, Length:7, Rest/binary>>)  ->
  TagLength = (Length3 bsl 21 + Length2 bsl 14 + Length1 bsl 7 + Length),
  <<Rest1:TagLength/binary, Rest2/binary>> = Rest,
  {Rest1, Rest2}.

mp4_read_tag(<<>>) ->
  undefined;

mp4_read_tag(<<Tag, Data/binary>>) ->
  {Body, Rest} = mp4_desc_length(Data),
  {Tag, Body, Rest}.


%%%%%%%%%%%%%%%%%%    STSZ  %%%%%%%%%%%%%%%%
% Sample sizes in bytes are stored here
%%
stsz(<<_Version:8, _Flags:24, 0:32, SampleCount:32, SampleSizeData/binary>>, Mp4Track) -> % case for different sizes
  size(SampleSizeData) == SampleCount*4 orelse erlang:error({broken_stsz,SampleCount,size(SampleSizeData)}),
  Mp4Track#mp4_track{sample_sizes = [Size || <<Size:32>> <= SampleSizeData]};
  
stsz(<<_Version:8, _Flags:24, Size:32, _SampleCount:32>>, Mp4Track) ->
  Mp4Track#mp4_track{sample_sizes = Size}.
  
% read_stsz(_, 0, #mp4_track{sample_sizes = SampleSizes} = Mp4Track) ->
%   Mp4Track#mp4_track{sample_sizes = lists:reverse(SampleSizes)};
%   
% read_stsz(<<Size:32, Rest/binary>>, Count, #mp4_track{sample_sizes = SampleSizes} = Mp4Track) ->
%   read_stsz(Rest, Count - 1, Mp4Track#mp4_track{sample_sizes = [Size | SampleSizes]}).


  

%%%%%%%%%%%%%%%%%%% STTS %%%%%%%%%%%%%%%%%%%
% Sample durations (dts delta between neigbour frames)
%%
stts(<<0:8, _Flags:3/binary, EntryCount:32, Rest/binary>>, Mp4Track) ->
  size(Rest) == EntryCount*8 orelse erlang:error({invalid_stts,EntryCount,size(Rest)}),
  DTSInfo = [{Count,Duration} || <<Count:32, Duration:32>> <= Rest],
  % Timestamps = fill_stts(DTSInfo),
  Mp4Track#mp4_track{sample_dts = DTSInfo}.
  
% fill_stts([{Count,Duration}|DTSInfo]) ->
%   fill_stts(Count,Duration,DTSInfo, 0, []).
% 
% fill_stts(0,_,[],_,Timestamps) ->
%   lists:reverse(Timestamps);
% 
% fill_stts(0, _, [{Count,Duration}|DTSInfo],DTS,Timestamps) ->
%   fill_stts(Count,Duration,DTSInfo,DTS,Timestamps);
% 
% fill_stts(Count,Duration,DTSInfo,DTS,Timestamps) ->
%   fill_stts(Count-1,Duration,DTSInfo,DTS+Duration,[DTS|Timestamps]).
  


%%%%%%%%%%%%%%%%%%%%% STSS atom %%%%%%%%%%%%%%%%%%%
% List of keyframes
%
stss(<<0:8, _Flags:3/binary, SampleCount:32, Samples/binary>>, Mp4Track) ->
  read_stss(Samples, SampleCount, Mp4Track).

read_stss(_, 0, #mp4_track{keyframes = Keyframes} = Mp4Track) ->
  Mp4Track#mp4_track{keyframes = lists:reverse(Keyframes)};

read_stss(<<Sample:32, Rest/binary>>, EntryCount, #mp4_track{keyframes = Keyframes} = Mp4Track) ->
  read_stss(Rest, EntryCount - 1, Mp4Track#mp4_track{keyframes = [Sample-1|Keyframes]}).




%%%%%%%%%%%%%%%%%%%%%% CTTS atom  %%%%%%%%%%%%%%%%%%%%%%%
% list of B-Frames offsets
%%
ctts(<<0:32, EntryCount:32, CTTS/binary>>, Mp4Track) ->
  size(CTTS) == EntryCount*8 orelse erlang:error({invalid_ctts,EntryCount,size(CTTS)}),
  Info = [{Count,Offset} || <<Count:32, Offset:32>> <= CTTS],
  Mp4Track#mp4_track{sample_composition = fill_ctts(Info)}.


fill_ctts([{Count,Offset}|Info]) ->
  fill_ctts(Count, Offset, Info, []).

fill_ctts(0, _, [], Acc) ->
  lists:reverse(Acc);

fill_ctts(0, _, [{Count,Offset}|Info], Acc) ->
  fill_ctts(Count, Offset, Info, Acc);

fill_ctts(Count, Offset, Info, Acc) ->
  fill_ctts(Count-1, Offset, Info, [Offset|Acc]).

  
%%%%%%%%%%%%%%%%%%%%% STSC %%%%%%%%%%%%%%%%%%%%%%%
% Samples per chunk
%%
stsc(<<0:8, _Flags:3/binary, EntryCount:32, Rest/binary>>, Mp4Track) ->
  read_stsc(Rest, EntryCount, Mp4Track).


read_stsc(_, 0, #mp4_track{chunk_sizes = ChunkSizes} = Mp4Track) ->
  Mp4Track#mp4_track{chunk_sizes = lists:reverse(ChunkSizes)};

read_stsc(<<ChunkId:32, SamplesPerChunk:32, _SampleId:32, Rest/binary>>, EntryCount, #mp4_track{chunk_sizes = ChunkSizes} = Mp4Track) ->
  read_stsc(Rest, EntryCount - 1, Mp4Track#mp4_track{chunk_sizes = [{ChunkId - 1, SamplesPerChunk}|ChunkSizes]}).




%%%%%%%%%%%%%%%%%%%%%% STCO/CO64 atom %%%%%%%%%%%%%%%%%%%%
% sample table chunk offset
%%
stco(<<0:8, _Flags:3/binary, OffsetCount:32, Offsets/binary>>, Mp4Track) ->
  size(Offsets) == OffsetCount*4 orelse erlang:error({invalid_stco,OffsetCount,size(Offsets)}),
  Mp4Track#mp4_track{chunk_offsets = [Offset || <<Offset:32>> <= Offsets]}.

co64(<<0:8, _Flags:3/binary, OffsetCount:32, Offsets/binary>>, Mp4Track) ->
  ?D({co64,OffsetCount}),
  read_co64(Offsets, OffsetCount, Mp4Track).

read_co64(<<>>, 0, #mp4_track{chunk_offsets = ChunkOffsets} = Mp4Track) ->
  Mp4Track#mp4_track{chunk_offsets = lists:reverse(ChunkOffsets)};

read_co64(<<Offset:64, Rest/binary>>, OffsetCount, #mp4_track{chunk_offsets = ChunkOffsets} = Mp4Track) ->
  read_co64(Rest, OffsetCount - 1, Mp4Track#mp4_track{chunk_offsets = [Offset | ChunkOffsets]}).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



unpack_samples_in_chunk(#mp4_track{chunk_offsets = Offsets, chunk_sizes = ChunkSizes} = Mp4Track) ->
  ChunkCount = length(Offsets),
  Mp4Track#mp4_track{chunk_sizes = unpack_samples_in_chunk(ChunkSizes, ChunkCount)}.


unpack_samples_in_chunk([{FirstChunk,SamplesInChunk}|ChunkSizes], ChunkCount) ->
  unpack_samples_in_chunk(FirstChunk, SamplesInChunk, ChunkSizes, ChunkCount, []).

unpack_samples_in_chunk(ChunkCount, _, [], ChunkCount, Acc) ->
  lists:reverse(Acc);

unpack_samples_in_chunk(FirstChunk, _, [{FirstChunk,SamplesInChunk}|ChunkSizes], ChunkCount, Acc) ->
  unpack_samples_in_chunk(FirstChunk, SamplesInChunk, ChunkSizes, ChunkCount, Acc);

unpack_samples_in_chunk(FirstChunk, SamplesInChunk, ChunkSizes, ChunkCount, Acc) ->
  unpack_samples_in_chunk(FirstChunk+1, SamplesInChunk, ChunkSizes, ChunkCount, [SamplesInChunk|Acc]).
    

  
fill_track(#mp4_track{number = TrackId, bitrate = OriginalBitrate} = Mp4Track) ->
  MinHeap = erlang:process_flag(min_heap_size, 10000000),
  Track = unpack_samples_in_chunk(Mp4Track),
  
  #mp4_track{
    chunk_offsets = ChunkOffsets,
    chunk_sizes = ChunkSizes,
    sample_sizes = SampleSizes,
    sample_dts = Timestamps,
    sample_composition = Compositions,
    keyframes = Keyframes,
    timescale = Timescale
  } = Track,
  {_T2, {Frames, IndexInfo}} = timer:tc(fun() -> fill_track(TrackId, SampleSizes, ChunkOffsets, ChunkSizes, Keyframes, Timestamps, Compositions, Timescale) end),
  
  [{Duration,_}|_] = IndexInfo,
  Bitrate = track_bitrate(OriginalBitrate, SampleSizes, Duration),
  % ?D({guess_bitrate, Bitrate, Duration, hd(lists:reverse(Timestamps)) / Timescale, lists:sum(SampleSizes)}),
  
  % ?D({fill_track, {fill_track, T2}, {count,length(SampleSizes)}}),
  erlang:process_flag(min_heap_size, MinHeap),
  Track#mp4_track{frames = Frames, bitrate = Bitrate, duration = Duration, index_info = lists:reverse(IndexInfo)}.




fill_track(TrackId, SampleSizes, [Offset|ChunkOffsets], [ChunkSize|ChunkSizes], Keyframes, Timestamps, Compositions, Timescale) ->
  fill_track(<<>>, [], TrackId, SampleSizes, Offset, ChunkOffsets, ChunkSize, ChunkSizes, Keyframes, Timestamps, 0, Compositions, Timescale, 0).

fill_track(Frames, IndexInfo, _, [], _, [], _, [], [], [], _, [], _, _) ->
  {Frames, IndexInfo};

fill_track(Frames, IndexInfo, TrackId, SampleSizes, _Offset, [Offset|ChunkOffsets], 0, [ChunkSize|ChunkSizes], Keyframes, Timestamps, DTS, Compositions, Timescale, Id) ->
fill_track(Frames, IndexInfo, TrackId, SampleSizes, Offset,     ChunkOffsets,   ChunkSize,        ChunkSizes, Keyframes, Timestamps, DTS, Compositions, Timescale, Id);

fill_track(Frames, IndexInfo, TrackId,SampleSizes, Offset, ChunkOffsets, ChunkSize, ChunkSizes, Keyframes, Timestamps, DTS, Compositions, Timescale, Id) ->
  {DTS1, TS1} = case Timestamps of
    [{1,Time}|TS1_] -> {DTS + Time, TS1_};
    [{Count,Time}|TS1_] -> {DTS + Time, [{Count-1,Time}|TS1_]}
  end,
  FDTS = DTS*1000/Timescale,
  {FPTS, Comp1} = case Compositions of
    [CTime|Comp1_] -> {(DTS+CTime)*1000/Timescale, Comp1_};
    _ -> {FDTS, []}
  end,
  {FKeyframe, KF1} = case Keyframes of
    [Id|KF1_] -> {1, KF1_};
    _ -> {0, Keyframes}
  end,
  {Size, SZ1} = case SampleSizes of
    [Size_|SampleSizes_] -> {Size_, SampleSizes_};
    Size_ -> {Size_, Size_}
  end,  
  fill_track(<<Frames/binary, FKeyframe:1, Size:63, Offset:64, FDTS:64/float, FPTS:64/float>>, [{FDTS, <<TrackId, FKeyframe:1, Id:23>>}|IndexInfo], TrackId,
             SZ1, Offset+Size, ChunkOffsets, ChunkSize-1, ChunkSizes, KF1, TS1, DTS1, Comp1, Timescale, Id+1).


track_bitrate(B, Sizes, Time) when B == undefined orelse B == 0 ->
  round(lists:sum(Sizes)*1000*8 / Time);

track_bitrate(Bitrate, _, _) ->
  Bitrate.


build_index(Tracks) when is_list(Tracks) ->
  {_T1, Indexes} = timer:tc(fun() -> [IndexInfo || #mp4_track{index_info = IndexInfo} <- Tracks] end),
  {_T2, T} = timer:tc(fun() -> lists:foldl(fun(Track, MergedTracks) -> lists:merge(Track, MergedTracks) end, [], Indexes) end),
  {_T3, Res} = timer:tc(fun() -> iolist_to_binary([I || {_DTS, I} <- T]) end),
  % ?D({build_index, {prepare_tracks_for_index,T1},{merge,T2},{glue,T3}}),
  Res;

build_index(Tracks) when is_tuple(Tracks) ->
  build_index(tuple_to_list(Tracks)).

  
%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").

% fill_track_test() ->
%   ?assertEqual(<<1:1, 300:63, 0:64, 0.0:64/float, 0.0:64/float, 0:1, 10:63, 300:64, 25.0:64/float, 25.0:64/float>>,
%   fill_track(<<>>, [300, 10], [0,300], [true,false], [0.0,25.0], [0.0,0.0],1000, 0)).

% prepare_index_tracks_test() ->
%   ?assertEqual([[{{0,1},0},{{25,1},1},{{50,1},2}], [{{0,2},0},{{30,2},1},{{45,2},2}]], prepare_tracks_for_index(test_tracks())).
% 
% test_tracks() ->
%   [#mp4_track{frames = [#mp4_frame{dts = 0}, #mp4_frame{dts = 25}, #mp4_frame{dts = 50}]}, 
%    #mp4_track{frames = [#mp4_frame{dts = 0}, #mp4_frame{dts = 30}, #mp4_frame{dts = 45}]}].
%   
% build_index_test() ->
%   ?assertEqual(<<1, 0:24, 2, 0:24, 1, 1:24, 2, 1:24, 2, 2:24, 1, 2:24>>, build_index(test_tracks())).

mp4_desc_tag_with_length_test() ->
  ?assertEqual({3, <<0,2,0,4,13,64,21,0,0,0,0,0,100,239,0,0,0,0,6,1,2>>, <<>>}, mp4_read_tag(<<3,21,0,2,0,4,13,64,21,0,0,0,0,0,100,239,0,0,0,0,6,1,2>>)),
  ?assertEqual({4, <<64,21,0,0,0,0,0,100,239,0,0,0,0>>, <<6,1,2>>}, mp4_read_tag(<<4,13,64,21,0,0,0,0,0,100,239,0,0,0,0,6,1,2>>)).
  

unpack_chunk_samples_test() ->
  Mp4Track = #mp4_track{chunk_offsets = [1,2,3,4,5,6], chunk_sizes = [{0, 4}, {3, 5}]},
  Mp4Track1 = unpack_samples_in_chunk(Mp4Track),
  Mp4Track2 = Mp4Track1#mp4_track{chunk_sizes = [4,4,4,5,5,5]},
  ?assertEqual(Mp4Track2, Mp4Track1).

  

esds_tag1_test() ->
  ?assertEqual(#esds{object_type = aac, stream_type = 21, buffer_size = 0, max_bitrate = 25839, avg_bitrate = 0}, config_from_esds_tag(<<3,21,0,2,0,4,13,64,21,0,0,0,0,0,100,239,0,0,0,0,6,1,2>>)).

esds_tag2_test() ->
  ?assertEqual(#esds{object_type = aac, stream_type = 21, buffer_size = 428, max_bitrate = 139608, avg_bitrate = 101944, specific = <<18,16>>}, config_from_esds_tag(<<3,25,0,0,0,4,17,64,21,0,1,172,0,2,33,88,0,1,142,56,5,2,18,16,6,1,2>>)).

get_coverart_validMeta_test () ->
  {ok,Dev} = file:open("test/files/tag_coverart.mp4",[read,raw,binary]),
  Reader = {file,Dev},
  Metadata = get_coverart(Reader),
  ?assertMatch(<<_:6/binary,"JFIF",_/binary>>, Metadata).

get_coverart_sizeMeta_test () ->
  {ok,Dev} = file:open("test/files/tag_coverart.mp4",[read,raw,binary]),
  Reader = {file,Dev},
  Metadata = get_coverart(Reader),
  ?assertEqual(114121,size(Metadata)).

get_coverart_unvalid_test () ->
  {ok,Dev} = file:open("test/files/without_coverart.mp4",[read,raw,binary]),
  Reader = {file,Dev},
  Metadata = get_coverart(Reader),
  ?assertEqual(<<>>,Metadata).


