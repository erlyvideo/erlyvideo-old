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
-include("../include/ems_media.hrl").
-include("../log.hrl").
-include("ems_media_client.hrl").

-export([transcode/2, send_frame/2]).

-define(TIMEOUT, 60000).


transcode(Frame, #ems_media{transcoder = undefined} = Media) ->
  {Media, Frame};


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
  {Media#ems_media{trans_state = State1}, Frames}.
  


send_frame(Frame, #ems_media{module = M} = Media) ->
  case M:handle_frame(Frame, Media) of
    {reply, Frames, Media1} when is_list(Frames) ->
      lists:foldl(fun
        (_, {stop,_,_} = Stop) -> 
          Stop;
        (OutFrame, {noreply, State, _}) ->
          define_media_info(OutFrame, State)
      end, {noreply, Media1, ?TIMEOUT}, Frames);
    {reply, F, Media1} ->
      define_media_info(F, Media1);
    {noreply, Media1} ->
      {noreply, Media1, ?TIMEOUT};
    {stop, Reason, Media1} ->
      {stop, Reason, Media1}
  end.
  
  
define_media_info(#video_frame{content = Content} = F, #ems_media{media_info = #media_info{audio = A, video = V} = MediaInfo} = Media) when
  (Content == audio andalso (A == [] orelse A == wait)) orelse (Content == video andalso (V == [] orelse V == wait)) ->
  case video_frame:define_media_info(MediaInfo, F) of
    MediaInfo -> shift_dts(F, Media);
    MediaInfo1 -> shift_dts(F, ems_media:set_media_info(Media, MediaInfo1))
  end;

define_media_info(F, M) ->
  shift_dts(F, M).
  
  
shift_dts(#video_frame{} = Frame, #ems_media{last_dts = undefined} = Media) ->
  shift_dts(Frame, Media#ems_media{last_dts = 0});

shift_dts(#video_frame{dts = undefined} = Frame, #ems_media{last_dts = LastDTS} = Media) ->
  handle_shifted_frame(Frame#video_frame{dts = LastDTS, pts = LastDTS}, Media);

shift_dts(#video_frame{dts = DTS} = Frame, #ems_media{ts_delta = undefined, glue_delta = GlueDelta, last_dts = LastDTS} = Media) ->
  ?D({"New instance of stream", LastDTS, DTS, LastDTS - DTS + GlueDelta}),
  ems_event:stream_started(proplists:get_value(host,Media#ems_media.options), Media#ems_media.name, self(), Media#ems_media.options),
  shift_dts(Frame, Media#ems_media{ts_delta = LastDTS - DTS + GlueDelta}); %% Lets glue new instance of stream to old one plus small glue time

%shift_dts(#video_frame{dts = DTS, pts = PTS} = Frame, #ems_media{ts_delta = Delta} = Media) when 
%(DTS + Delta < 0 andalso DTS + Delta >= -1000) orelse (PTS + Delta < 0 andalso PTS + Delta >= -1000) ->
%  handle_shifted_frame(Frame#video_frame{dts = 0, pts = 0}, Media);

shift_dts(#video_frame{dts = DTS, pts = PTS} = Frame, #ems_media{ts_delta = Delta} = Media) ->
  %?D({Frame#video_frame.content, round(Frame#video_frame.dts), round(Delta), round(DTS + Delta)}),
      handle_shifted_frame(Frame#video_frame{dts = DTS + Delta, pts = PTS + Delta}, Media).


handle_shifted_frame(#video_frame{dts = DTS, pts = PTS} = Frame,  Media) when (DTS < 0 andalso DTS >= -1000) orelse (PTS < 0 andalso PTS >= -1000) ->
  handle_shifted_frame(Frame#video_frame{dts = 0, pts = 0}, Media);

handle_shifted_frame(#video_frame{dts = DTS} = Frame, #ems_media{format = Format, storage = Storage, frame_number = Number} = Media)  ->
  % ?D({Frame#video_frame.content, Number, Frame#video_frame.flavor, Frame#video_frame.dts}),
  Media1 = start_on_keyframe(Frame, Media),
  Storage1 = save_frame(Format, Storage, Frame),
  handle_config(Frame, Media1#ems_media{storage = Storage1, last_dts = DTS, frame_number = Number + 1}).



handle_config(#video_frame{content = video, body = Config}, #ems_media{video_config = #video_frame{body = Config}} = Media) -> 
  {noreply, Media, ?TIMEOUT};

handle_config(#video_frame{content = audio, body = Config}, #ems_media{audio_config = #video_frame{body = Config}} = Media) -> 
  {noreply, Media, ?TIMEOUT};

handle_config(#video_frame{content = video, flavor = config} = Config, #ems_media{} = Media) ->
  handle_frame(Config, Media#ems_media{video_config = Config});

handle_config(#video_frame{content = audio, flavor = config} = Config, #ems_media{} = Media) -> 
  handle_frame(Config, Media#ems_media{audio_config = Config});

handle_config(Frame, Media) ->
  handle_frame(Frame, Media).


handle_frame(#video_frame{content = Content} = Frame, #ems_media{video_config = V, clients = Clients} = Media) ->
  Media2 = store_last_gop(Media, Frame),
  case Content of
    audio when V == undefined -> ems_media_clients:send_frame(Frame, Clients, starting);
    metadata -> ems_media_clients:send_frame(Frame, Clients, starting);
    _ -> ok
  end,
  ems_media_clients:send_frame(Frame, Clients, active),
  {noreply, Media2, ?TIMEOUT}.


save_frame(undefined, Storage, _) ->
  Storage;

save_frame(Format, Storage, Frame) ->
  case Format:write_frame(Frame, Storage) of
    {ok, Storage1} -> Storage1;
    _ -> Storage
  end.
  
  
  
store_last_gop(#ems_media{last_gop = undefined} = Media, _Frame) ->
  Media;
  
store_last_gop(#ems_media{last_gop = GOP} = Media, #video_frame{content = video, flavor = keyframe} = Frame) when is_list(GOP) ->
  Media#ems_media{last_gop = [Frame]};

store_last_gop(#ems_media{last_gop = GOP} = Media, _) when length(GOP) == 500 ->
  Media#ems_media{last_gop = []};

store_last_gop(#ems_media{last_gop = GOP} = Media, Frame) when is_list(GOP) ->
  Media#ems_media{last_gop = [Frame | GOP]}.


  

start_on_keyframe(#video_frame{content = video, flavor = keyframe, dts = DTS} = _F, 
                  #ems_media{clients = Clients, video_config = V, audio_config = A} = M) ->
  Clients2 = case A of
    undefined -> Clients;
    _ -> ems_media_clients:send_frame(A#video_frame{dts = DTS, pts = DTS}, Clients, starting)
  end,
  Clients3 = case V of
    undefined -> Clients2;
    _ -> ems_media_clients:send_frame(V#video_frame{dts = DTS, pts = DTS}, Clients2, starting)
  end,
    
  Clients4 = ems_media_clients:mass_update_state(Clients3, starting, active),
  M#ems_media{clients = Clients4};


start_on_keyframe(_, Media) ->
  Media.
