%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        remote rtmp media
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
-module(rtmp_media).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(ems_media).
-include("../log.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").
-include_lib("rtmp/include/rtmp.hrl").
-include("ems_media.hrl").


-export([init/2, handle_frame/2, handle_control/2, handle_info/2]).

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

init(Media, _Options) ->
  self() ! make_request,
  {ok, Media#ems_media{media_info = #media_info{flow_type = stream, audio = wait, video = wait}}}.


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
  {stop, normal, State};

handle_control({set_source, _Source}, State) ->
  %% Set source returns:
  %% {reply, Reply, State}
  %% {stop, Reason, State}
  {noreply, State};

handle_control({set_socket, _Socket}, State) ->
  %% Set socket returns:
  %% {reply, Reply, State}
  %% {stop, Reason, State}
  {noreply, State};

handle_control(no_clients, State) ->
  %% no_clients returns:
  %% {reply, ok, State}      => wait forever till clients returns
  %% {reply, Timeout, State} => wait for Timeout till clients returns
  %% {noreply, State}        => just ignore and live more
  %% {stop, Reason, State}   => stops. This should be default
  {stop, normal, State};

handle_control(timeout, State) ->
  {stop, normal, State};

handle_control(make_request, Media) ->
  {ok, RTMP} = rtmp_socket:connect(ems_media:get(Media, url)),
  rtmp_lib:connect(RTMP),
  {ok, RTMP};

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
  

handle_info({rtmp, RTMP, connected}, State) ->
  URL = ems_media:get(State, url),
  {_, FullPath} = http_uri2:extract_path_with_query(URL),
  [_App|PathParts] = string:tokens(FullPath, "/"),
  Path = string:join(PathParts, "/"),
  ?D({"Connected to RTMP source", URL}),
  Stream = rtmp_lib:createStream(RTMP),
  ?D({"Stream",Stream}),
  rtmp_lib:play(RTMP, Stream, Path),
  ?D({"Playing", Path}),
  {noreply, State};

handle_info({rtmp, _RTMP, #rtmp_message{type = Type, timestamp = Timestamp, body = Body}}, Recorder) when (Type == audio orelse Type == video) andalso size(Body) > 0 ->
  Frame = flv_video_frame:decode(#video_frame{dts = Timestamp, pts = Timestamp, content = Type}, Body),
  % case Frame#video_frame.flavor of
  %   command -> ?D(Frame);
  %   _ -> ok %?D({Frame#video_frame.content, Frame#video_frame.codec, Frame#video_frame.flavor, Timestamp})
  % end,
  self() ! Frame,
  {noreply, Recorder};

handle_info({rtmp, _RTMP, #rtmp_message{type = metadata, timestamp = Timestamp, body = Meta}}, Recorder)  ->
  % ?D(Meta),
  % ?D({Frame#video_frame.codec_id, Frame#video_frame.frame_type, Frame#video_frame.decoder_config, Message#rtmp_message.timestamp}),
  Frame = #video_frame{content = metadata, dts = Timestamp, pts = Timestamp, body = Meta},
  self() ! Frame,
  {noreply, Recorder};

handle_info({rtmp, _RTMP, #rtmp_message{type = Type}}, State) when Type == ping orelse Type == pong 
  orelse Type == burst_start orelse Type == burst_stop ->
  % Ignore ping/pong messages
  {noreply, State};

handle_info({rtmp, _RTMP, #rtmp_message{type = stream_end}}, State) ->
  ems_media_clients:foldl(fun({Pid, StreamId}, _) ->
    Pid ! {ems_stream, StreamId, play_complete, 0}
  end, ok, State),
  {stop,normal, State};

handle_info({rtmp, _RTMP, #rtmp_message{type = invoke, body = #rtmp_funcall{command = <<"onStatus">>,stream_id = StreamId, args = [null, {object, Command} |_]}}}, State) ->
   case proplists:get_value(code, Command) of
     <<"NetStream.Play.StreamNotFound">> ->
        lists:map(fun({Pid,_Ref})-> 
          Pid ! {ems_stream,StreamId,not_found}
        end,State#ems_media.waiting_for_config),    
        {noreply,State};
%       ems_media_clients:foldl(fun({Pid, StreamId}, _) ->
%       ?D({Pid,killl}),
%       Pid ! {ems_stream, StreamId, not_found}
%       end, ok, State),
%       {noreply, State};
     _ ->
       {noreply, State}
   end;

handle_info({rtmp, _RTMP, #rtmp_message{} = Message}, State) ->
  ?D({"RTMP message", Message}),
  {noreply, State};

handle_info(_Message, State) ->
  {noreply, State}.








