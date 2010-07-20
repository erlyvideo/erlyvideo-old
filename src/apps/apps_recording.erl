%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        RTMP functions, that support recording
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org</a> for more information
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
-module(apps_recording).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../../include/ems.hrl").
-include("../../include/rtmp_session.hrl").
-include_lib("erlmedia/include/video_frame.hrl").

-export([publish/2]).
-export(['FCPublish'/2, 'FCUnpublish'/2]).


%%-------------------------------------------------------------------------
%% @private
%%-------------------------------------------------------------------------

'FCPublish'(State, #rtmp_funcall{args = [null, Name]} = _AMF) -> 
  ?D({"FCpublish", Name}),
  State.

'FCUnpublish'(State, #rtmp_funcall{args = Args} = AMF) ->
  ?D({"FCunpublish", Args}),
  apps_streaming:stop(State, AMF).
  % rtmp_session:reply(State,AMF#rtmp_funcall{args = [null, undefined]}),
  % State.

publish(#rtmp_session{host = Host, streams = Streams} = State, #rtmp_funcall{args = [null,Name, <<"record">>], stream_id = StreamId} = _AMF) -> 
  ?D({"Publish - Action - record",Name}),
  {ok, Recorder} = media_provider:create(Host, Name, [{type, record}]),
  State#rtmp_session{streams = ems:setelement(StreamId, Streams, Recorder)};


publish(State, #rtmp_funcall{args = [null,Name,<<"append">>]} = _AMF) -> 
  ?D({"Publish - Action - append",Name}),
  gen_fsm:send_event(self(), {publish, append, Name}),
  State;


publish(State, #rtmp_funcall{args = [null,URL,<<"LIVE">>]} = AMF) ->
  ?D({"publish LIVE rewriting to live", URL}),
  publish(State, AMF#rtmp_funcall{args = [null,URL,<<"live">>]});

publish(#rtmp_session{host = Host, streams = Streams, socket = Socket} = State, #rtmp_funcall{args = [null,URL,<<"live">>], stream_id = StreamId} = _AMF) -> 
  [Name | _Params] = string:tokens(binary_to_list(URL), "?"),
  ?D({"LIVE", _AMF#rtmp_funcall.stream_id}),
  ems_log:access(Host, "RECORD LIVE ~s ~p ~s", [State#rtmp_session.addr, State#rtmp_session.user_id, Name]),
  {ok, Recorder} = media_provider:create(Host, Name, [{type, live}]),
  rtmp_socket:send(Socket, #rtmp_message{type = stream_begin, stream_id = StreamId}),
  rtmp_socket:status(Socket, StreamId, ?NS_PUBLISH_START),
  State#rtmp_session{streams = ems:setelement(StreamId, Streams, Recorder)};

publish(State, #rtmp_funcall{args = [null, false]} = AMF) ->
  apps_streaming:stop(State, AMF);

publish(State, #rtmp_funcall{args = [null, null]} = AMF) ->
  apps_streaming:stop(State, AMF);

publish(State, #rtmp_funcall{args = [null, <<"null">>]} = AMF) ->
  apps_streaming:stop(State, AMF);
  
publish(#rtmp_session{host = Host, streams = Streams} = State, #rtmp_funcall{args = [null,Name], stream_id = StreamId} = _AMF) -> 
  ems_log:access(Host, "LIVE ~s ~p ~s", [State#rtmp_session.addr, State#rtmp_session.user_id, Name]),
  {ok, Recorder} = media_provider:create(Host, Name, [{type, live}]),
  State#rtmp_session{streams = ems:setelement(StreamId, Streams, Recorder)}.

