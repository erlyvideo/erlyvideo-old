%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        ems_media handler template
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2010 Max Lapshin
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(rtmp_media).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(ems_media).
-include_lib("rtmp/include/rtmp.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include("../../include/ems_media.hrl").


-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-export([init/2, handle_frame/2, handle_control/2, handle_info/2]).

-record(rtmp, {
  socket,
  demuxer,
  url
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
  URL = proplists:get_value(url, Options),
  {rtmp, _UserInfo, Host, Port, _Path, _Query} = http_uri2:parse(URL),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}, {packet, raw}]),
  {ok, RTMP} = rtmp_socket:connect(Socket),
  ems_media:set_source(self(), RTMP),
  {ok, #rtmp{socket = Socket, demuxer = RTMP, url = URL}}.


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
  {reply, ok, State};

handle_control({source_lost, _Source}, State) ->
  %% Source lost returns:
  %% {reply, Source, State} -> new source is created
  %% {stop, Reason, State} -> stop with Reason
  {stop, source_lost, State};

handle_control({set_source, _Source}, State) ->
  %% Set source returns:
  %% {reply, Reply, State}
  %% {stop, Reason, State}
  {reply, ok, State};

handle_control({set_socket, _Socket}, State) ->
  %% Set socket returns:
  %% {reply, Reply, State}
  %% {stop, Reason, State}
  {reply, ok, State};

handle_control(timeout, State) ->
  {stop, normal, State};

handle_control(_Control, State) ->
  {reply, ok, State}.

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
handle_info({rtmp, RTMP, connected}, #rtmp{url = URL} = State) ->
  ?D({"Connected to RTMP source", URL}),
  {rtmp, _UserInfo, _Host, _Port, FullPath, _Query} = http_uri2:parse(URL),
  [App|PathParts] = string:tokens(FullPath, "/"),
  Path = list_to_binary(string:join(PathParts, "/")),
  ?D({"App,path", App, Path}),
  rtmp_socket:setopts(RTMP, [{active, true}]),
  rtmp_lib:connect(RTMP, [{app, list_to_binary(App)}, {tcUrl, list_to_binary(URL)}]),
  Stream = rtmp_lib:createStream(RTMP),
  ?D({"Stream",Stream}),
  rtmp_lib:play(RTMP, Stream, Path),
  ?D({"Playing", Path}),
  {noreply, State};

handle_info({rtmp, _RTMP, #rtmp_message{type = Type, timestamp = Timestamp, body = Body} = Message}, Recorder) when Type == audio orelse Type == video ->
  Frame = flv_video_frame:decode(#video_frame{dts = Timestamp, pts = Timestamp, type = Type}, Body),
  % ?D({Frame#video_frame.codec_id, Frame#video_frame.frame_type, Frame#video_frame.decoder_config, Message#rtmp_message.timestamp}),
  self() ! Frame,
  {noreply, Recorder};

handle_info({rtmp, _RTMP, #rtmp_message{type = metadata, timestamp = Timestamp, body = [<<"onMetaData">>, {object, Meta}]}}, Recorder)  ->
  ?D(Meta),
  % ?D({Frame#video_frame.codec_id, Frame#video_frame.frame_type, Frame#video_frame.decoder_config, Message#rtmp_message.timestamp}),
  Frame = #video_frame{type = metadata, dts = Timestamp, pts = Timestamp, body = Meta},
  self() ! Frame,
  {noreply, Recorder};

handle_info({rtmp, _RTMP, #rtmp_message{} = Message}, State) ->
  ?D({"RTMP message", Message}),
  {noreply, State};

handle_info(_Message, State) ->
  {noreply, State}.








