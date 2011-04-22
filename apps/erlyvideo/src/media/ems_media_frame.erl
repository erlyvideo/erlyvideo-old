%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2010 Max Lapshin
%%% @doc        Erlyvideo media clients handling
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
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
-module(ems_media_frame).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("ems_media.hrl").
-include("../log.hrl").
-include("ems_media_client.hrl").

-export([init/1, send_frame/2]).

-define(TIMEOUT, 60000).

-export([
transcode/2,
pass_frame_through_module/2,
define_media_info/2,
fix_undefined_dts/2,
calculate_new_stream_shift/2,
shift_dts_delta/2,
warn_bad_dts_delta/2,
save_last_dts/2,
fix_negative_dts/2,
start_on_keyframe/2,
store_frame/2,
save_config/2,
send_audio_to_starting_clients/2,
send_frame_to_clients/2,
store_last_gop/2,
dump_frame/2
]).



init(#ems_media{} = Media) ->
  Media#ems_media{frame_filters = frame_filters(Media)}.


% 
% Workflow is following:
%
% Filter(Frame, State) -> {reply, Frame, State} | 
%                         {reply, Frames1, State} | 
%                         {noreply, State} |
%                         {stop, Reason, State}
%

frame_filters(#ems_media{options = Options} = _Media) ->
  [
    transcode,
    pass_frame_through_module,
    define_media_info,
    fix_undefined_dts,
    calculate_new_stream_shift,
    shift_dts_delta] ++ 
  case proplists:get_value(warn_dts_delta, Options) of
    true -> [warn_bad_dts_delta];
    _ -> []
  end ++  
  [ fix_negative_dts,
    save_last_dts,
    start_on_keyframe,
    store_frame,
    save_config] ++
  case proplists:get_value(frame_dump, Options) of
    true -> [dump_frame];
    _ -> []
  end ++ 
  case proplists:get_value(send_audio_before_keyframe, Options) of
    false -> [];
    _ -> [send_audio_to_starting_clients]
  end ++
  [
    send_frame_to_clients
  ].

send_frame(#video_frame{} = Frame, #ems_media{frame_filters = FrameFilters} = Media) ->
  pass_filter_chain([Frame], Media, FrameFilters).
  % pass_filter_chain([Frame], Media, frame_filters(Media)).


pass_filter_chain([], #ems_media{} = Media, _Filters) ->
  {noreply, Media, ?TIMEOUT};

pass_filter_chain([_Frame|Frames], #ems_media{frame_filters = FrameFilters} = Media, []) ->
  pass_filter_chain(Frames, Media, FrameFilters);

pass_filter_chain([Frame|Frames], #ems_media{} = Media, [Filter|Filters]) ->
  put(current_filter, Filter),
  case ?MODULE:Filter(Frame, Media) of
    {reply, NewFrames, #ems_media{} = Media1} when is_list(NewFrames) ->
      pass_filter_chain(NewFrames ++ Frames, Media1, Filters);
    {reply, #video_frame{} = NewFrame, #ems_media{} = Media1} ->
      pass_filter_chain([NewFrame|Frames], Media1, Filters);
    {reply, undefined, #ems_media{frame_filters = FrameFilters} = Media1} ->
      pass_filter_chain(Frames, Media1, FrameFilters);
    {noreply, #ems_media{frame_filters = FrameFilters} = Media1} ->
      pass_filter_chain(Frames, Media1, FrameFilters);
    {stop, Reason, #ems_media{} = Media1} ->
      {stop, Reason, Media1}
  end.




transcode(Frame, #ems_media{transcoder = undefined} = Media) ->
  {reply, Frame, Media};


transcode(#video_frame{} = Frame, #ems_media{transcoder = Transcoder, trans_state = State} = Media) ->
  % case Frame#video_frame.content of
  %   audio ->
  %     Counter = case get(start_count) of
  %       undefined -> put(start_dts, Frame#video_frame.dts), put(start_count, 0), 0;
  %       C -> C
  %     end,
  %     put(start_count, Counter + size(Frame#video_frame.body)),
  %     ?D({transcoding, round(Frame#video_frame.dts - get(start_dts)), Counter});
  %   _ -> ok
  % end,
  {ok, Frames, State1} = Transcoder:transcode(Frame, State),
  {reply, Frames, Media#ems_media{trans_state = State1}}.



pass_frame_through_module(Frame, #ems_media{module = M} = Media) ->
  M:handle_frame(Frame, Media).


need_to_define_media_info(audio, [], _) -> true;
need_to_define_media_info(audio, wait, _) -> true;
need_to_define_media_info(video, _, []) -> true;
need_to_define_media_info(video, _, wait) -> true;
need_to_define_media_info(_, _, _) -> false.
  
define_media_info(#video_frame{content = Content} = F, #ems_media{media_info = #media_info{audio = A, video = V} = MediaInfo} = Media) ->
  case need_to_define_media_info(Content, A, V) of
    true ->
      case video_frame:define_media_info(MediaInfo, F) of
        MediaInfo -> {reply, F, Media};
        MediaInfo1 -> {reply, F, ems_media:set_media_info(Media, MediaInfo1)}
      end;
    false ->
      {reply, F, Media}
  end.



fix_undefined_dts(#video_frame{dts = undefined} = Frame, #ems_media{last_dts = LastDTS} = Media) ->
  {reply, Frame#video_frame{dts = LastDTS, pts = LastDTS}, Media};

fix_undefined_dts(Frame, Media) ->
  {reply, Frame, Media}.



calculate_new_stream_shift(#video_frame{dts = DTS} = Frame, #ems_media{ts_delta = undefined, glue_delta = GlueDelta, last_dts = LDTS} = Media) ->
  {LastDTS, TSDelta} = case LDTS of
    undefined -> {DTS, 0};
    _ -> {LDTS, LDTS - DTS + GlueDelta}
  end,
  ?D({"New instance of stream", LastDTS, DTS, TSDelta}),
  ems_event:stream_started(proplists:get_value(host,Media#ems_media.options), Media#ems_media.name, self(), Media#ems_media.options),
  {reply, Frame, Media#ems_media{ts_delta = TSDelta}}; %% Lets glue new instance of stream to old one plus small glue time

calculate_new_stream_shift(Frame, Media) ->
  {reply, Frame, Media}.


shift_dts_delta(#video_frame{dts = DTS, pts = PTS} = Frame, #ems_media{ts_delta = Delta} = Media) ->
  {reply, Frame#video_frame{dts = DTS + Delta, pts = PTS + Delta}, Media}.



save_last_dts(#video_frame{dts = DTS} = Frame, Media) ->
  {reply, Frame, Media#ems_media{last_dts = DTS, last_dts_at = os:timestamp()}}.
  

warn_bad_dts_delta(#video_frame{dts = DTS} = Frame, #ems_media{last_dts = LastDTS} = Media) when DTS < 0 orelse abs(DTS - LastDTS) > 4000 ->
  ?D({large_dts_delta, DTS, LastDTS}),
  {reply, Frame, Media};

warn_bad_dts_delta(Frame, Media) ->
  {reply, Frame, Media}.



fix_negative_dts(#video_frame{dts = DTS, pts = PTS} = Frame,  Media) when (DTS < 0 andalso DTS >= -1000) orelse (PTS < 0 andalso PTS >= -1000) ->
  {reply, Frame#video_frame{dts = 0, pts = 0}, Media};

fix_negative_dts(Frame, Media) ->
  {reply, Frame, Media}.


start_on_keyframe(#video_frame{content = video, flavor = keyframe, dts = DTS} = F, 
                  #ems_media{clients = Clients, video_config = V, audio_config = A} = M) ->
  Clients2 = case A of
    undefined -> Clients;
    _ -> ems_media_clients:send_frame(A#video_frame{dts = DTS, pts = DTS}, Clients, starting)
  end,
  Clients3 = case V of
    undefined -> Clients2;
    _ -> ems_media_clients:send_frame(V#video_frame{dts = DTS, pts = DTS}, Clients2, starting)
  end,
  ems_media_clients:flush(Clients3),
  Clients4 = ems_media_clients:mass_update_state(Clients3, starting, active),
  {reply, F, M#ems_media{clients = Clients4}};


start_on_keyframe(Frame, Media) ->
  {reply, Frame, Media}.



store_frame(#video_frame{} = Frame, #ems_media{format = undefined} = Media)  ->
  {reply, Frame, Media};

store_frame(#video_frame{} = Frame, #ems_media{format = Format, storage = Storage} = Media)  ->
  Storage1 = case Format:write_frame(Frame, Storage) of
    {ok, Storage1_} -> Storage1_;
    _ -> Storage
  end,
  {reply, Frame, Media#ems_media{storage = Storage1}}.


save_config(#video_frame{content = video, body = Config}, #ems_media{video_config = #video_frame{body = Config}} = Media) -> 
  {noreply, Media};

save_config(#video_frame{content = audio, body = Config}, #ems_media{audio_config = #video_frame{body = Config}} = Media) -> 
  {noreply, Media};

save_config(#video_frame{content = video, flavor = config} = Config, #ems_media{} = Media) ->
  {reply, Config, Media#ems_media{video_config = Config}};

save_config(#video_frame{content = audio, flavor = config} = Config, #ems_media{} = Media) -> 
  {reply, Config, Media#ems_media{audio_config = Config}};

save_config(Frame, Media) ->
  {reply, Frame, Media}.



send_audio_to_starting_clients(#video_frame{content = audio} = Frame, #ems_media{clients = Clients} = Media) ->
  ems_media_clients:send_frame(Frame, Clients, starting),
  {reply, Frame, Media};

send_audio_to_starting_clients(Frame, Media) ->
  {reply, Frame, Media}.


send_frame_to_clients(#video_frame{content = Content} = Frame, #ems_media{clients = Clients} = Media) ->
  case Content of
    metadata -> ems_media_clients:send_frame(Frame, Clients, starting);
    _ -> ok
  end,
  ems_media_clients:send_frame(Frame, Clients, active),
  {noreply, Media}.




dump_frame(#video_frame{flavor = Flavor, codec = Codec, dts = DTS} = Frame, Media) ->
  ?D({Media#ems_media.name, Codec,Flavor,round(DTS), (catch size(Frame#video_frame.body))}),
  {reply, Frame, Media}.
  


%shift_dts(#video_frame{dts = DTS, pts = PTS} = Frame, #ems_media{ts_delta = Delta} = Media) when 
%(DTS + Delta < 0 andalso DTS + Delta >= -1000) orelse (PTS + Delta < 0 andalso PTS + Delta >= -1000) ->
%  handle_shifted_frame(Frame#video_frame{dts = 0, pts = 0}, Media);



  
  
store_last_gop(_Frame, #ems_media{last_gop = undefined} = Media) ->
  Media;
  
store_last_gop(#video_frame{content = video, flavor = keyframe} = Frame, #ems_media{last_gop = GOP} = Media) when is_list(GOP) ->
  Media#ems_media{last_gop = [Frame]};

store_last_gop(_, #ems_media{last_gop = GOP} = Media) when length(GOP) == 500 ->
  Media#ems_media{last_gop = []};

store_last_gop(Frame, #ems_media{last_gop = GOP} = Media) when is_list(GOP) ->
  Media#ems_media{last_gop = [Frame | GOP]}.


  

