%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009-2011 Max Lapshin
%%% @doc        Erlyvideo media utilities
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
-module(ems_media_utils).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("ems_media.hrl").
-include("ems_media_client.hrl").
-include("../log.hrl").

-define(TIMEOUT, 120000).

-export([source_is_lost/1, source_is_restored/1]).


source_is_lost(#ems_media{source = Source} = Media) ->
  (catch ems_media:stop(Source)),
  ?D(Media#ems_media.options),
  Options = Media#ems_media.options,
  case proplists:get_value(flush_roll,Options,[]) of
    undefined ->
      module_handling(Media);
    List ->
      flush_roll(Media,List)
  end.

flush_roll(#ems_media{source = Source} = Media, List) ->
  _Name = proplists:get_value(name,List,"mezzzz.flv"),
  _Host = proplists:get_value(host,List,default),
  _FOptions = proplists:get_value(options,List,[]), 
  ?D({"ems_media lost source", Source}),
  %ems_event:stream_source_lost(proplists:get_value(host,Media#ems_media.options), Media#ems_media.name, self()),
  %{ok,NewSource} = media_provider:open(Host,Name,FOptions),
 % MediaInfo = ems_media:media_info(NewSource),
 % Media0 = ems_media:set(Media, module, file_media),
 % Media1 = ems_media:set_media_info(Media0,MediaInfo),
 % Media2 = ems_media:set(Media1,last_dts,0),
 % Media3 = ems_media:set(Media2,ts_delta,undefined),
 % Metadata = rtmp_session:metadata(NewSource,[]),
 % StreamId=Metadata#video_frame.stream_id,
 % Options = [{stream_id,StreamId},{client_buffer, 1000},{start,0}],
 % ems_media:set_source(Media,NewSource),
 % ConfigFrames = video_frame:config_frames(MediaInfo)++ [#video_frame{content = audio, flavor = frame, codec = empty}, Metadata],
 % [?D(F#video_frame.dts)|| F <- ConfigFrames],
  %?D({Media1#ems_media.last_dts,Media3}),
  %[ems_media_frame:send_frame(F#video_frame{dts = 0,pts = 0,stream_id = StreamId},Media3) || F <- ConfigFrames],
  media_provider:play(default,"mezzzz.flv",[{stream_id, 0},{client_buffer,0}]),
  {noreply, Media#ems_media{ts_delta = undefined}, ?TIMEOUT}.    

module_handling(#ems_media{module = M, source = Source, source_timeout = SourceTimeout} = Media) ->  
  case M:handle_control({source_lost, Source}, Media#ems_media{source = undefined}) of
    {stop, Reason, Media1} ->
      ?D({"ems_media is stopping due to source_lost", M, Source, Reason}),
      {stop, Reason, Media1};
    {stop, Reason, _Reply, Media1} ->
      {stop, Reason, Media1};
    {noreply, Media1} when is_number(SourceTimeout) andalso SourceTimeout > 0 ->
      ?D({"ems_media lost source and sending graceful", SourceTimeout, round(Media1#ems_media.last_dts)}),
      {ok, Ref} = timer:send_after(SourceTimeout, no_source),
      {noreply, Media1#ems_media{source_ref = undefined, source_timeout_ref = Ref}, ?TIMEOUT};
    {noreply, Media1} when SourceTimeout == 0 ->
      {stop, normal, Media1};
    {noreply, Media1} when SourceTimeout == false ->
      ?D({"ems_media lost source but source_timeout = false"}),
      {noreply, Media1#ems_media{source_ref = undefined}, ?TIMEOUT};
    {reply, NewSource, Media1} ->
      ?D({"ems_media lost source and sending graceful, but have new source", SourceTimeout, NewSource}),
      Ref = erlang:monitor(process, NewSource),
      {noreply, Media1#ems_media{source = NewSource, source_ref = Ref, ts_delta = undefined}, ?TIMEOUT}
  end.



source_is_restored(#ems_media{source = NewSource, source_timeout_ref = OldRef, clients = Clients} = Media) ->
  (catch timer:cancel(OldRef)),
  Ref = case NewSource of
    undefined -> undefined;
    _ -> erlang:monitor(process, NewSource)
  end,
  Clients1 = ems_media_clients:mark_active_as_starting(Clients),
  {noreply, Media#ems_media{source = NewSource, source_timeout_ref = undefined, source_ref = Ref, ts_delta = undefined, clients = Clients1}, ?TIMEOUT}.



