-module(ets_timeshift).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../../include/media_info.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([init/1, seek/3, read/2, clean/1, store/2]).

%%%%%%%%%%%%%%%           Timeshift features         %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Options) ->
  timer:send_interval(5000, clean_timeshift),
  ets:new(timeshift, [ordered_set, {keypos, #video_frame.dts}]).


seek(#media_info{shift = Shift}, before, Timestamp) ->
  Frames = ets:select(Shift, ets:fun2ms(fun(#video_frame{dts = TS, frame_type = keyframe} = Frame) when TS =< Timestamp ->
    TS
  end)),
  case lists:reverse(Frames) of
    [Fr | _] -> {Fr, Fr};
    _ -> undefined
  end;

seek(#media_info{shift = Shift}, 'after', Timestamp) ->
  Frames = ets:select(Shift, ets:fun2ms(fun(#video_frame{dts = TS, frame_type = keyframe} = Frame) when TS >= Timestamp ->
    TS
  end)),
  case Frames of
    [Fr | _] -> {Fr, Fr};
    _ -> undefined
  end.
  

read(#media_info{shift = Shift}, DTS) ->
  case ets:lookup(Shift, DTS) of
    [Frame] -> 
      Next = ets:next(Shift, DTS),
      {Frame, Next};
    [] ->
      {undefined, undefined}
  end.


clean(#media_info{timeshift = Timeshift, shift = Frames, last_dts = DTS, name = _URL} = MediaInfo) ->
  Limit = DTS - Timeshift,
  Spec = ets:fun2ms(fun(#video_frame{dts = TS} = F) when TS < Limit -> 
    true
  end),
  _Count = ets:select_delete(Frames, Spec),
  % _Count = 0,
  % io:format("~s timeshift is ~p/~p bytes/frames in time ~p-~p, clean: ~p~n", [_URL, ets:info(Frames, memory), ets:info(Frames, size), round(ets:first(Frames)), round(DTS), _Count]),
  % io:format("~s timeshift is ~p/~p clean: ~p~n", [_URL, ets:info(Frames, memory), ets:info(Frames, size), _Count]),
  MediaInfo.


store(#media_info{shift = Frames, timeshift = Timeshift} = MediaInfo, #video_frame{} = Frame) when is_number(Timeshift) andalso Timeshift > 0 andalso Frame =/= undefined->
  ets:insert(Frames, Frame),
  MediaInfo;

store(MediaInfo, _Frame) ->
  MediaInfo.
