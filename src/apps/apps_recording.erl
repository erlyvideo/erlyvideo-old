%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
%%% @doc        Generalized RTMP application behavior module
%%% @reference  See <a href="http://erlyvideo.googlecode.com" target="_top">http://erlyvideo.googlecode.com</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Luke Hubbard, Stuart Jackson, Roberto Saccon
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
-include("../../include/recorder.hrl").

-export([publish/2]).
-export(['WAIT_FOR_DATA'/2]).
-export(['FCPublish'/2, 'FCUnpublish'/2]).




'WAIT_FOR_DATA'({publish, #channel{stream_id = StreamId} = Channel}, #rtmp_session{streams = Streams} = State) ->
  Recorder = array:get(StreamId, Streams),
  stream_media:publish(Recorder, Channel),
	{next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};	


'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a publish command and responds
%% @end
%%-------------------------------------------------------------------------

'FCPublish'(#amf{args = [null, Name]} = _AMF, State) -> 
  ?D({"FCpublish", Name}),
  % Start = AMF#amf{
  %     id = 0,
  %     stream_id = 0,
  %     command = 'onFCPublish',
  %     args = [null, {object, [{code, <<?NS_PUBLISH_START>>}, 
  %                             {description, Name}]}]},
  % gen_fsm:send_event(self(), {invoke, Start}),
  % 
  % apps_rtmp:reply(AMF#amf{args = [null, undefined]}),
  State.

'FCUnpublish'(#amf{args = Args} = AMF, State) -> 
  ?D({"FCunpublish", Args}),
  apps_rtmp:reply(AMF#amf{args = [null, undefined]}),
  State.

publish(#amf{args = [null,Name, <<"record">>], stream_id = StreamId} = _AMF, #rtmp_session{host = Host, streams = Streams} = State) -> 
  ?D({"Publish - Action - record",Name}),
  Recorder = media_provider:create(Host, Name, record),
  ?D({"Recording", Recorder}),
  State#rtmp_session{streams = array:set(StreamId, Recorder, Streams)};


publish(#amf{args = [null,Name,<<"append">>]} = _AMF, State) -> 
  ?D({"Publish - Action - append",Name}),
  gen_fsm:send_event(self(), {publish, append, Name}),
  State;


publish(#amf{args = [null,Name,<<"LIVE">>], stream_id = StreamId} = _AMF, #rtmp_session{host = Host, streams = Streams} = State) -> 
  ?D({"Publish - Action - LIVE",Name, StreamId}),
  Recorder = media_provider:create(Host, Name, live),
  gen_fsm:send_event(self(), {control, ?RTMP_CONTROL_STREAM_BEGIN, StreamId}),
  
  Start = #amf{
      command = 'onStatus',
      type = invoke,
      id = 0,
      stream_id = StreamId,
      args = [null, {object, [{code, <<?NS_PUBLISH_START>>}, 
                              {level, <<"status">>}, 
                              {description, <<"Publishing ", Name/binary, ".">>},
                              {clientid, 1716786930}]}]},
  gen_fsm:send_event(self(), {invoke, Start}),
  State#rtmp_session{streams = array:set(StreamId, Recorder, Streams)};

publish(#amf{args = [null,Name,<<"live">>], stream_id = StreamId} = _AMF, #rtmp_session{host = Host, streams = Streams} = State) -> 
  ?D({"Publish - Action - live",Name}),
  Recorder = media_provider:create(Host, Name, live),
  State#rtmp_session{streams = array:set(StreamId, Recorder, Streams)};

publish(#amf{args = [null, false]} = AMF, State) ->
  apps_streaming:stop(AMF, State);
  
publish(#amf{args = [null,Name], stream_id = StreamId} = _AMF, #rtmp_session{host = Host, streams = Streams} = State) -> 
  ?D({"Publish - Action - default live",Name}),
  Recorder = media_provider:create(Host, Name, live),
  State#rtmp_session{streams = array:set(StreamId, Recorder, Streams)}.

