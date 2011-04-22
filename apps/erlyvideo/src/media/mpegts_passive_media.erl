%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        ems_media handler template
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
-module(mpegts_passive_media).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(ems_media).
-include_lib("erlmedia/include/video_frame.hrl").
-include("ems_media.hrl").
-include("../log.hrl").


-define(TIMEOUT_RESTART, 1000).

-export([init/2, handle_frame/2, handle_control/2, handle_info/2]).

-record(mpegts, {
  options,
  timeout
}).


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

init(#ems_media{} = Media, Options) ->
  State = #mpegts{options = Options, timeout = proplists:get_value(timeout, Options, 4000)},
  {ok, Media#ems_media{clients_timeout = false, state = State}}.


%%----------------------------------------------------------------------
%% @spec (ControlInfo::tuple(), State) -> {ok, State}       |
%%                                        {ok, State, tick} |
%%                                        {error, Reason}
%%
%% @doc Called by ems_media to handle specific events
%% @end
%%----------------------------------------------------------------------
handle_control({subscribe, _Client, _Options}, State) ->
  %% Subscribe returns:
  %% {ok, State} -> client is subscribed as active receiver
  %% {ok, State, tick} -> client requires ticker (file reader)
  %% {error, Reason} -> client receives {error, Reason}
  {noreply, State};

handle_control({source_lost, _Source}, State) ->
  %% Source lost returns:
  %% {ok, State, Source} -> new source is created
  %% {stop, Reason, State} -> stop with Reason
  {noreply, State};

handle_control({set_source, _Source}, State) ->
  %% Set source returns:
  %% {reply, Reply, State}
  %% {stop, Reason, State}
  {noreply, State};
  
handle_control({set_socket, Socket}, Media) ->
  {ok, Reader} = mpegts_sup:start_reader([{consumer,self()}]),
  ems_media:set_source(self(), Reader),
  mpegts_reader:set_socket(Reader, Socket),
  {noreply, Media};

handle_control(timeout, State) ->
  {noreply, State};

handle_control(no_clients, #ems_media{type = mpegts_passive, source = undefined, clients_timeout = LifeTimeout} = Media) ->
  ?D("MPEG-TS passive doesn't have clients and socket"),
  {reply, LifeTimeout, Media};

handle_control(no_clients, #ems_media{type = mpegts_passive} = Media) ->
  ?D("MPEG-TS passive doesn't have clients, but have socket"),
  {noreply, Media};

handle_control(_Control, State) ->
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

handle_info(_Msg, State) ->
  {noreply, State}.

