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


source_is_lost(#ems_media{source = Source,media_info = #media_info{video = []}} = Media) ->
  (catch ems_media:stop(Source)),
  handle_lost_source(Media);

source_is_lost(#ems_media{source = Source,media_info = #media_info{video = [Video|_]}} = Media) ->
  (catch ems_media:stop(Source)),
  Options = Media#ems_media.options,
  #stream_info{codec = Codec} = Video,
  ems_event:stream_source_lost(ems_media:get(Media,host), Media#ems_media.name, self()),
  case proplists:get_value(failure_movie,Options) of
    Name when is_list(Name) andalso Codec == h264 ->
      failure_movie(Media, Name);
    _ ->
      handle_lost_source(Media)
  end.

%
% {failure_movie,"sample.mp4"} -- is a option in config file 
%
failure_movie(#ems_media{source = Source} = Media, Name) ->
  Host = ems_media:get(Media, host),
  ?D({"ems_media lost source", Source}),
  {ok, Stream} = media_provider:open(Host,Name),
  #media_info{video = [Video]} = ems_media:media_info(Stream),
  {ok,FailureSource} = case Video of
    #stream_info{codec = h264} ->    
      media_provider:play(Stream,[{stream_id, 0},{client_buffer,0}]);  
    _ ->
      catch(ems_media:stop(Stream)),
      {ok,undefined}
  end,
  handle_lost_source(Media#ems_media{ts_delta = undefined, failure_source = FailureSource}).

handle_lost_source(#ems_media{module = M, source = Source, source_timeout = SourceTimeout} = Media) ->  
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



