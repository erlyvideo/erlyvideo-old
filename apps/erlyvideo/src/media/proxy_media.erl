%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2011 Max Lapshin
%%% @doc        proxy_media
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
-module(proxy_media).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(ems_media).
-include("ems_media.hrl").
-include("../log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").

-export([init/2, handle_frame/2, handle_control/2, handle_info/2]).

-export([switch_to/2, switch_to/3, switch_to/4]).

-record(proxy, {
  next,
  stream_id
}).


switch_to(Media, Source) when is_pid(Media) andalso is_pid(Source) ->
  Media ! {switch_proxy_to, Source}.
  
switch_to(Media, Host, Name) when is_pid(Media) ->
  {ok, Source} = media_provider:open(Host, Name),
  switch_to(Media, Source).

switch_to(Host, MediaName, Host2, Name) ->
  {ok, Media} = media_provider:open(Host, MediaName),
  switch_to(Media, Host2, Name).


%%%------------------------------------------------------------------------
%%% Callback functions from ems_media
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Media::ems_media(), Options::list()) -> {ok, Media::ems_media()} |
%%                                                {stop, Reason}
%%
%% @doc Called by ems_media to initialize specific data for current media type
%% @end
%%----------------------------------------------------------------------

init(State, _Options) ->
  {ok, State#ems_media{state = #proxy{stream_id = 2}}}.

%%----------------------------------------------------------------------
%% @spec (ControlInfo::tuple(), State) -> {reply, Reply, State} |
%%                                        {stop, Reason, State} |
%%                                        {error, Reason}
%%
%% @doc Called by ems_media to handle specific events
%% @end
%%----------------------------------------------------------------------
handle_control({subscribe, _Client, _Options}, #ems_media{} = State) ->
  %% Subscribe returns:
  %% {reply, tick, State}  => client requires ticker (file reader)
  %% {reply, Reply, State} => client is subscribed as active receiver and receives custom reply
  %% {noreply, State}      => client is subscribed as active receiver and receives reply ``ok''
  %% {reply, {error, Reason}, State} => client receives {error, Reason}
  {noreply, State};

handle_control({unsubscribe, _Client}, #ems_media{} = State) ->
  %% Unsubscribe returns:
  %% {reply, Reply, State} => client is unsubscribed inside plugin, but not rejected from ets table
  %% {noreply, State}      => client is unsubscribed in usual way.
  %% {reply, {error, Reason}, State} => client receives {error, Reason} 
  {noreply, State};

handle_control({seek, _Client, _BeforeAfter, _DTS}, #ems_media{} = State) ->
  %% seek is a destructive call, that changes state of Client: it is moved from stream to file
  %% and state of media_ticker is changed
  %% seek returns:
  %% {reply, {NewPos, NewDTS}, State} => media knows how to seek in storage
  %% {stop, Reason, State}  => stop with Reason
  %% {noreply, State}       => default action is to seek in storage.
  {noreply, State};

handle_control({source_lost, _Source}, #ems_media{} = State) ->
  %% Source lost returns:
  %% {reply, Source, State} => new source is created
  %% {stop, Reason, State}  => stop with Reason
  %% {noreply, State}       => default action. it is stop
  {stop, normal, State};

handle_control({set_source, _Source}, #ems_media{} = State) ->
  %% Set source returns:
  %% {reply, NewSource, State} => source is rewritten
  %% {noreply, State}          => just ignore setting new source
  %% {stop, Reason, State}     => stop after setting
  {noreply, State};

handle_control({set_socket, _Socket}, #ems_media{} = State) ->
  %% Set socket returns:
  %% {reply, Reply, State}  => the same as noreply
  %% {noreply, State}       => just ignore
  %% {stop, Reason, State}  => stops
  {noreply, State};

handle_control(no_clients, #ems_media{} = State) ->
  %% no_clients returns:
  %% {reply, ok, State}      => wait forever till clients returns
  %% {reply, Timeout, State} => wait for Timeout till clients returns
  %% {noreply, State}        => just ignore and live more
  %% {stop, Reason, State}   => stops. This should be default
  {stop, normal, State};

handle_control(timeout, #ems_media{} = State) ->
  {stop, normal, State};

handle_control(_Control, #ems_media{} = State) ->
  {noreply, State}.

%%----------------------------------------------------------------------
%% @spec (Frame::video_frame(), State) -> {reply, Frame, State} |
%%                                        {noreply, State}   |
%%                                        {stop, Reason, State}
%%
%% @doc Called by ems_media to parse frame.
%% @end
%%----------------------------------------------------------------------
handle_frame(#video_frame{content = audio, stream_id = StreamId, flavor = config} = Frame, 
             #ems_media{state = #proxy{stream_id = StreamId}, last_dts = LastDTS} = Media) ->
  {noreply, Media#ems_media{audio_config = Frame#video_frame{dts = LastDTS, pts = LastDTS}}};

handle_frame(#video_frame{content = video, stream_id = StreamId, flavor = config} = Frame, 
             #ems_media{state = #proxy{stream_id = StreamId}, last_dts = LastDTS} = Media) ->
  {noreply, Media#ems_media{video_config = Frame#video_frame{dts = LastDTS, pts = LastDTS}}};

handle_frame(#video_frame{content = video, stream_id = StreamId, flavor = Flavor, dts = DTS} = Frame, 
             #ems_media{source = OldSource, source_ref = OldRef, last_dts = LastDTS, audio_config = Audio, video_config = Video,
                        state = #proxy{next = Source, stream_id = StreamId} = Proxy} = Media) 
             when Flavor == keyframe ->
  (catch ems_media:stop(OldSource)),
  (catch erlang:demonitor(OldRef, [flush])),
    
  Ref = erlang:monitor(process, Source),
  
  ?D({switch, self(), DTS, LastDTS, Media#ems_media.ts_delta, Flavor}),
  
  Frames = [#video_frame{
    content = audio,
    flavor = frame,
    codec = empty,
    dts = DTS,
    pts = DTS,
    stream_id = StreamId
  }] ++
  case Audio of
    undefined -> [];
    #video_frame{} -> [Audio#video_frame{dts = DTS, pts = DTS, stream_id = StreamId}]
  end ++ 
  case Video of
    undefined -> [];
    #video_frame{} -> [Video#video_frame{dts = DTS, pts = DTS, stream_id = StreamId}]
  end ++ 
  [Frame],
  {reply, Frames, 
  Media#ems_media{state = Proxy#proxy{next = undefined, stream_id = StreamId + 1}, audio_config = undefined, video_config = undefined, 
                  source = Source, source_ref = Ref, ts_delta = LastDTS - DTS + 20}};

handle_frame(#video_frame{stream_id = StreamId, content = _C, flavor = _F, dts = _DTS}, #ems_media{state = #proxy{stream_id = StreamId}} = Media) ->
  % ?D({zzz, self(), StreamId, C, F, _DTS, Media#ems_media.last_dts}),
  {noreply, Media};

handle_frame(Frame, State) ->
  % ?D({ok, self(), Frame#video_frame.dts, State#ems_media.last_dts}),
  {reply, Frame, State}.


%%----------------------------------------------------------------------
%% @spec (Message::any(), State) ->  {noreply, State}   |
%%                                   {stop, Reason, State}
%%
%% @doc Called by ems_media to parse incoming message.
%% @end
%%----------------------------------------------------------------------
handle_info({switch_proxy_to, Stream}, #ems_media{state = #proxy{next = OldStream, stream_id = OldStreamId} = Proxy} = Media) ->
  (catch ems_media:stop(OldStream)),
  StreamId = OldStreamId + 1,
  ems_media:play(Stream, [{stream_id, StreamId}]),
  {noreply, Media#ems_media{state = Proxy#proxy{next = Stream, stream_id = StreamId}}};
  
handle_info(_Message, State) ->
  {noreply, State}.








