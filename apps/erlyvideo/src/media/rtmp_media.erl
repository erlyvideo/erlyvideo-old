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
  {ok, Media}.


%%----------------------------------------------------------------------
%% @spec (ControlInfo::tuple(), State) -> {reply, Reply, State} |
%%                                        {stop, Reason, State} |
%%                                        {error, Reason}
%%
%% @doc Called by ems_media to handle specific events
%% @end
%%----------------------------------------------------------------------
handle_control({source_lost, _Source}, #ems_media{} = Media) ->
  %% Source lost returns:
  %% {ok, State, Source} -> new source is created
  %% {stop, Reason, State} -> stop with Reason
  self() ! make_request,
  {noreply, Media};

handle_control(no_clients, State) ->
  %% no_clients returns:
  %% {reply, ok, State}      => wait forever till clients returns
  %% {reply, Timeout, State} => wait for Timeout till clients returns
  %% {noreply, State}        => just ignore and live more
  %% {stop, Reason, State}   => stops. This should be default
  {stop, normal, State};

handle_control(timeout, State) ->
  {stop, normal, State};

handle_control({make_request, URL}, _Media) ->
  rtmp_lib:play(URL);

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
  

handle_info({rtmp, _RTMP, #rtmp_message{type = Type, timestamp = Timestamp, body = Body}}, Recorder) when (Type == audio orelse Type == video) andalso size(Body) > 0 ->
  Frame = flv_video_frame:decode(#video_frame{dts = Timestamp, pts = Timestamp, content = Type}, Body),
  % case Frame#video_frame.flavor of
  %   command -> ?D(Frame);
  %   _ -> ok %?D({Frame#video_frame.content, Frame#video_frame.codec, Frame#video_frame.flavor, Timestamp})
  % end,
  self() ! Frame,
  {noreply, Recorder};

handle_info({rtmp, _RTMP, #rtmp_message{type = metadata, timestamp = Timestamp, body = Meta}}, Recorder)  ->
  ?D(Meta),
  % ?D({Frame#video_frame.codec_id, Frame#video_frame.frame_type, Frame#video_frame.decoder_config, Message#rtmp_message.timestamp}),
  Frame = #video_frame{content = metadata, dts = Timestamp, pts = Timestamp, body = Meta},
  self() ! Frame,
  {noreply, Recorder};

handle_info({rtmp, _RTMP, #rtmp_message{type = Type}}, State) when Type == ping orelse Type == pong 
  orelse Type == burst_start orelse Type == burst_stop ->
  % Ignore ping/pong messages
  {noreply, State};

% handle_info({rtmp, _RTMP, #rtmp_message{type = stream_end}}, State) ->
%   ems_media_clients:foldl(fun({Pid, StreamId}, _) ->
%     Pid ! {ems_stream, StreamId, play_complete, 0}
%   end, ok, State),
%   {stop,normal, State};

handle_info({rtmp, RTMP, #rtmp_message{type = invoke, body = #rtmp_funcall{command = <<"onStatus">>,stream_id = StreamId, args = [null, {object, Command} |_]}}}, State) ->
  case proplists:get_value(code, Command) of
    <<"NetStream.Play.StreamNotFound">> ->
      ?D({stream_not_found}),
       
      % Клиенты, которые сейчас ожидают первого фрейма получат not_found.
      % Этого должно хватить, что бы отрисовать RTMP 404
      lists:map(fun({Pid,_Ref})-> 
        Pid ! {ems_stream,StreamId,not_found}
      end,State#ems_media.waiting_for_config),
        
      % Эта строчка нужна, потому что клиент, который только-только стартовал наш поток
      % ждет ответа ems_media:media_info (это блокирующий вызов) и нам надо его разблокировать
      % пустой медиа инфо  
      State1 = ems_media:set_media_info(State, #media_info{audio = [], video = []}),
      
      % посылаем себе асинхронно stop, что бы наши клиенты не отвалились с error normal
      self() ! stop,
      {noreply, State1};
%       ems_media_clients:foldl(fun({Pid, StreamId}, _) ->
%       ?D({Pid,killl}),
%       Pid ! {ems_stream, StreamId, not_found}
%       end, ok, State),
%       {noreply, State};

    <<"NetStream.Play.Stop">> ->
      % Play.Stop означает нормальное завершение удаленного стрима
      ?D({remote_stream_stoped, Command}),
      {stop, normal, State};
      
    <<"NetStream.Play.Failed">> ->
      ?D({play_failed}),
      % Play.Failed означает, что стрим сдох, а следовательно нам имеет смысл попробовать рестартнуться
      rtmp_socket:close(RTMP),
      {noreply, State};
      
     _Else ->
       ?D({unknown_status, _Else}),
       {noreply, State}
   end;

handle_info({rtmp, _RTMP, #rtmp_message{} = Message}, State) ->
  ?D({"RTMP message", Message}),
  {noreply, State};

handle_info(stop, State) ->
  {stop, normal, State};

handle_info(_Message, State) ->
  {noreply, State}.






