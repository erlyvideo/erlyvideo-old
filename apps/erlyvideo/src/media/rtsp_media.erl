%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        remote rtsp media
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
-module(rtsp_media).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(ems_media).
-include("ems_media.hrl").
-include("../log.hrl").

-export([init/2, handle_frame/2, handle_control/2, handle_info/2]).

-record(rtsp, {
  timeout,
  reader,
  restart_count = 0
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
  Timeout = proplists:get_value(timeout, Options, 5000),
  State = #rtsp{timeout = Timeout},
  self() ! make_request,
  {ok, Media#ems_media{state = State, source_timeout = live_media:default_timeout(), clients_timeout = live_media:default_timeout()}}.

%%----------------------------------------------------------------------
%% @spec (ControlInfo::tuple(), State) -> {reply, Reply, State} |
%%                                        {stop, Reason, State} |
%%                                        {error, Reason}
%%
%% @doc Called by ems_media to handle specific events
%% @end
%%----------------------------------------------------------------------
handle_control({subscribe, _Client, _Options}, State) ->
  {noreply, State};

handle_control({source_lost, _Source}, #ems_media{} = Media) ->
  %% Source lost returns:
  %% {ok, State, Source} -> new source is created
  %% {stop, Reason, State} -> stop with Reason
  self() ! make_request,
  {noreply, Media};

handle_control({set_source, _Source}, State) ->
  %% Set source returns:
  %% {reply, Reply, State}
  %% {stop, Reason}
  {noreply, State};

handle_control(no_clients, State) ->
  %% no_clients returns:
  %% {reply, ok, State}      => wait forever till clients returns
  %% {reply, Timeout, State} => wait for Timeout till clients returns
  %% {noreply, State}        => just ignore and live more
  %% {stop, Reason, State}   => stops. This should be default
  {noreply, State};

handle_control(timeout, #ems_media{source = Reader} = Media) ->
  erlang:exit(Reader, shutdown),
  ?D("RTSP timeout"),
  {noreply, Media};

handle_control({make_request, URL}, #ems_media{state = #rtsp{timeout = Timeout}, options = Options}) ->
  rtsp_socket:read(URL, [{consumer, self()},{timeout,Timeout},
                         {dump_traffic,proplists:get_value(dump_traffic,Options,true)},
                         {transport,proplists:get_value(transport,Options,tcp)},
                         {tracks, proplists:get_value(tracks,Options)}]);

handle_control(_Control, State) ->
  {noreply, State}.

%%----------------------------------------------------------------------
%% @spec (Frame::video_frame(), State) -> {ok, Frame, State} |
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
handle_info(make_request, Media) ->
  ?D("No RTSP source and retry limits are over"),
  {stop, normal, Media};
  
handle_info(_Message, State) ->
  {noreply, State}.








