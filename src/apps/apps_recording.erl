%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon, 2009 Max Lapshin
%%% @doc        Generalized RTMP application behavior module
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Luke Hubbard, Stuart Jackson, Roberto Saccon, 2009 Max Lapshin
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
-module(apps_recording).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-author('max@maxidoors.ru').
-include("../../include/ems.hrl").
-include("../../include/video_frame.hrl").
-include("../../include/recorder.hrl").

-export([publish/2]).
-export(['WAIT_FOR_DATA'/2]).
-export(['FCPublish'/2, 'FCUnpublish'/2]).


message_to_frame(#rtmp_message{timestamp = Timestamp, type = Type, body = Body}) ->
  #video_frame{type = Type, timestamp = Timestamp, body = Body, raw_body = true}.

'WAIT_FOR_DATA'({publish, #rtmp_message{stream_id = StreamId} = Message}, #rtmp_session{streams = Streams} = State) ->
  Recorder = array:get(StreamId, Streams),
  stream_media:publish(Recorder, message_to_frame(Message)),
	{next_state, 'WAIT_FOR_DATA', State};	


'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a publish command and responds
%% @end
%%-------------------------------------------------------------------------

'FCPublish'(State, #rtmp_funcall{args = [null, Name]} = _AMF) -> 
  ?D({"FCpublish", Name}),
  State.

'FCUnpublish'(State, #rtmp_funcall{args = Args} = AMF) -> 
  ?D({"FCunpublish", Args}),
  apps_rtmp:reply(State,AMF#rtmp_funcall{args = [null, undefined]}),
  State.

publish(#rtmp_session{host = Host, streams = Streams} = State, #rtmp_funcall{args = [null,Name, <<"record">>], stream_id = StreamId} = _AMF) -> 
  ?D({"Publish - Action - record",Name}),
  Recorder = media_provider:create(Host, Name, record),
  ?D({"Recording", Recorder}),
  State#rtmp_session{streams = array:set(StreamId, Recorder, Streams)};


publish(State, #rtmp_funcall{args = [null,Name,<<"append">>]} = _AMF) -> 
  ?D({"Publish - Action - append",Name}),
  gen_fsm:send_event(self(), {publish, append, Name}),
  State;


publish(#rtmp_session{host = Host, streams = Streams, socket = Socket} = State, #rtmp_funcall{args = [null,Name,<<"LIVE">>], stream_id = StreamId} = _AMF) -> 
  ?D({"Publish - Action - LIVE",Name, StreamId}),
  Recorder = media_provider:create(Host, Name, live),
  rtmp_socket:send(Socket, #rtmp_message{type = stream_begin, stream_id = StreamId}),
  rtmp_socket:status(Socket, StreamId, ?NS_PUBLISH_START),
  State#rtmp_session{streams = array:set(StreamId, Recorder, Streams)};

publish(#rtmp_session{host = Host, streams = Streams} = State, #rtmp_funcall{args = [null,Name,<<"live">>], stream_id = StreamId} = _AMF) -> 
  ?D({"Publish - Action - live",Name}),
  Recorder = media_provider:create(Host, Name, live),
  State#rtmp_session{streams = array:set(StreamId, Recorder, Streams)};

publish(#rtmp_funcall{args = [null, false]} = AMF, State) ->
  apps_streaming:stop(AMF, State);
  
publish(#rtmp_session{host = Host, streams = Streams} = State, #rtmp_funcall{args = [null,Name], stream_id = StreamId} = _AMF) -> 
  ?D({"Publish - Action - default live",Name}),
  Recorder = media_provider:create(Host, Name, live),
  State#rtmp_session{streams = array:set(StreamId, Recorder, Streams)}.

