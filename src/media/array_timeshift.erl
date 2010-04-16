
-module(array_timeshift).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../../include/media_info.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("../include/debug.hrl").

-export([init/1, seek/3, read/2, clean/1, store/2, info/1]).

%%%%%%%%%%%%%%%           Timeshift features         %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Options) ->
  Shift = proplists:get_value(timeshift, Options),
  Size = Shift*80 div 1000, % About 80 fps for video and audio
  First = 0,
  Last = 0,
  timer:send_interval(5000, clean_timeshift),
  {First, Last, array:new(Size)}.


info(#media_info{shift = {First, Last, Frames}}) when First =/= Last ->
  #video_frame{dts = FirstDTS} = array:get(First, Frames),
  #video_frame{dts = LastDTS} = array:get((Last-1+array:size(Frames)) rem array:size(Frames), Frames),
  [{start,FirstDTS},{length,LastDTS - FirstDTS}];
  
info(#media_info{shift = {_First, _Last, _Frames}}) ->
  [{length,0}];
  
info(_) ->
  [].


seek(#media_info{shift = {First, _Last, Frames}}, _BeforeAfter, Timestamp) when Timestamp =< 0 ->
  ?D({"going to seek", Timestamp}),
  case array:get(First, Frames) of
    undefined -> undefined;
    #video_frame{dts = DTS} -> {(First + 1) rem array:size(Frames), DTS}
  end;

seek(#media_info{shift = {First, Last, Frames}}, BeforeAfter, Timestamp) ->
  ?D({"going to seek", Timestamp}),
  S = seek_in_timeshift(First, Last, Frames, BeforeAfter, Timestamp, {undefined, undefined}),
  ?D({"Seek in array", First, Last, Timestamp, S}),
  S.
  
seek_in_timeshift(First, First, _Frames, _BeforeAfter, _Timestamp, Key) ->
  Key;

seek_in_timeshift(First, Last, Frames, BeforeAfter, Timestamp, Key) ->
  case array:get(First, Frames) of
    #video_frame{dts = DTS} when BeforeAfter == before andalso DTS > Timestamp ->
      Key;
    #video_frame{type = video, frame_type = keyframe, decoder_config = false, dts = DTS} when BeforeAfter == 'after' andalso DTS > Timestamp ->
      {First, DTS};
    #video_frame{type = video, frame_type = keyframe, decoder_config = false, dts = DTS} ->
      seek_in_timeshift((First+1) rem array:size(Frames), Last, Frames, BeforeAfter, Timestamp, {First, DTS});
    #video_frame{} ->
      seek_in_timeshift((First+1) rem array:size(Frames), Last, Frames, BeforeAfter, Timestamp, Key)
  end.

read(_, undefined) ->
  {undefined, undefined};

read(#media_info{shift = {_First, _Last, Frames}}, Key) ->
  % ?D({"Read", Key}),
  case array:get(Key, Frames) of
    undefined -> undefined;
    Frame -> {Frame, (Key + 1) rem array:size(Frames)}
  end.


clean(#media_info{shift = {_First, _Last, _Frames}, last_dts = _DTS, name = _URL} = MediaInfo) ->
  % _Bin = lists:foldl(fun({_, Bytes, _}, Sum) -> Bytes + Sum end, 0, element(2, erlang:process_info(self(), binary))),
  % {memory, Mem} = erlang:process_info(self(), memory),
  % ?D({"Store", First, Last, array:size(Frames), Bin div 1024, DTS}),
  % ?D({"Store", Bin div 1024, Mem div 1024}),
  % _Count = 0,
  % io:format("~s timeshift is ~p/~p bytes/frames in time ~p-~p, clean: ~p~n", [_URL, ets:info(Frames, memory), ets:info(Frames, size), round(ets:first(Frames)), round(DTS), _Count]),
  % io:format("~s timeshift is ~p/~p clean: ~p~n", [_URL, ets:info(Frames, memory), ets:info(Frames, size), _Count]),
  MediaInfo.

store(MediaInfo, #video_frame{decoder_config = true}) ->
  MediaInfo;

store(#media_info{shift = {First, Last, Frames}} = MediaInfo, #video_frame{} = Frame) when Frame =/= undefined->
  Last1 = (Last + 1) rem array:size(Frames),
  First1 = case Last1 of
    First -> (First + 1) rem array:size(Frames);
    _ -> First
  end,
  MediaInfo#media_info{shift = {First1, Last1, array:set(Last, Frame, Frames)}};

store(MediaInfo, _Frame) ->
  MediaInfo.
