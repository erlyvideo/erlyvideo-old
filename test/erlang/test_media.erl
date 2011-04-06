%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        ems_media for test
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
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
-module(test_media).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(ems_media).

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include("../include/ems_media.hrl").

-export([init/2, handle_frame/2, handle_control/2, handle_info/2]).
-export([seek/3]).



seek(_Media, before, Timestamp) ->
  {Timestamp, Timestamp}.




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
  timer:kill_after(1000),
  {ok, ems_media:set_media_info(State, #media_info{})}.


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

handle_control({seek, _Client, _DTS, _Options}, #ems_media{} = State) ->
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
  {noreply, State};

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
  {noreply, State};

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
handle_frame(Frame, State) ->
  {reply, Frame, State}.


%%----------------------------------------------------------------------
%% @spec (Message::any(), State) ->  {noreply, State}   |
%%                                   {stop, Reason, State}
%%
%% @doc Called by ems_media to parse incoming message.
%% @end
%%----------------------------------------------------------------------
handle_info(stop, State) ->
  {stop, normal, State};
  
handle_info(_Message, State) ->
  {noreply, State}.

