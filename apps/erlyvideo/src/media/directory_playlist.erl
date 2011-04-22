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
-module(directory_playlist).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(ems_media).
-include("ems_media.hrl").
-include("../log.hrl").


-export([init/2, handle_frame/2, handle_control/2, handle_info/2]).

-record(playlist, {
  path,
  host,
  wildcard,
  files = []
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

init(Media, Options) ->
  Path = proplists:get_value(path, Options),
  Host = proplists:get_value(host, Options),
  Wildcard = proplists:get_value(wildcard, Options),

  self() ! read_playlist,
  State = #playlist{wildcard = Wildcard, path = Path, host = Host},
  {ok, Media#ems_media{state = State}}.

%%----------------------------------------------------------------------
%% @spec (ControlInfo::tuple(), State) -> {reply, Reply, State} |
%%                                        {stop, Reason, State} |
%%                                        {error, Reason}
%%
%% @doc Called by ems_media to handle specific events
%% @end
%%----------------------------------------------------------------------
handle_control({subscribe, _Client, _Options}, State) ->
  %% Subscribe returns:
  %% {reply, tick, State} -> client requires ticker (file reader)
  %% {reply, Reply, State} -> client is subscribed as active receiver
  %% {reply, {error, Reason}, State} -> client receives {error, Reason}
  {noreply, State};

handle_control({source_lost, _Source}, State) ->
  %% Source lost returns:
  %% {reply, Source, State} -> new source is created
  %% {stop, Reason, State} -> stop with Reason
%  {Stream, State1} = next_file(State),
%  {reply, Stream, State1};
  {noreply, State};

handle_control({set_source, _Source}, State) ->
  %% Set source returns:
  %% {reply, Reply, State}
  %% {stop, Reason, State}
  {Stream, State1} = next_file(State),
  {reply, Stream, State1};
%  {noreply, State};

handle_control({set_socket, _Socket}, State) ->
  %% Set socket returns:
  %% {reply, Reply, State}
  %% {stop, Reason, State}
  {noreply, State};

handle_control(timeout, State) ->
  {stop, timeout, State};

handle_control(_Control, State) ->
  ?D({"### HANDLE CONTROL ###", State}),
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
handle_info(read_playlist, #ems_media{state = #playlist{host = Host, wildcard = Wildcard, path = Path} = State} = Media) ->
  AbsPath = ems:pathjoin(file_media:file_dir(Host), Path),
  Files = [ems:pathjoin(Path,File) || File <- filelib:wildcard(Wildcard, AbsPath)],
  ?D({AbsPath, Wildcard, Files}),
  
  self() ! start_playing,
  {noreply, Media#ems_media{state = State#playlist{files = Files}}};

handle_info(start_playing, #ems_media{state = #playlist{host = Host, files = [Name|Files]}} = Media) ->
  State = Media#ems_media.state,
  {ok, Stream} = media_provider:play(Host, Name, [{stream_id,1}]),
  ?D({"Playing",Name, Stream}),
  {noreply, Media#ems_media{state = State#playlist{files = Files}}};

handle_info({ems_stream, _StreamId, play_complete, _DTS}, #ems_media{state = #playlist{host = Host, files = [Name|Files]}} = Media) ->
  State = Media#ems_media.state,
  {ok, Stream} = media_provider:play(Host, Name, [{stream_id,1}]),
  ems_media:set_source(self(), undefined),
  ?D({"Playing",Name,Stream}),
  {noreply, Media#ems_media{state = State#playlist{files = Files}}};

handle_info(timeout, State) ->
  {stop, normal, State};

handle_info(_Message, State) ->
  ?D({message, _Message}),
  {noreply, State}.

next_file(#ems_media{state = #playlist{host = Host, wildcard = Wildcard, path = Path, files = []} = State} = Media) ->
  AbsPath = ems:pathjoin(file_media:file_dir(Host), Path),
  Files = [ems:pathjoin(Path,File) || File <- filelib:wildcard(Wildcard, AbsPath)],
  {reply, Media#ems_media{state = State#playlist{files = Files}}};

next_file(#ems_media{state = #playlist{files = Files} = State} = Media) ->
  State = Media#ems_media.state,
%  {ok, Stream} = media_provider:play(Host, Name, [{stream_id,1}]),
%  ems_media:set_source(self(), Stream),
  {reply, Media#ems_media{state = State#playlist{files = Files}}}.
  


