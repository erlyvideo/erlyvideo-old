%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        RTMP encoding/decoding module. 
%%% @reference  See <a href="http://erlyvideo.org/rtmp" target="_top">http://erlyvideo.org/rtmp</a> for more information.
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
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
-module(rtmp_lib).
-author(max@maxidoors.ru).
-version(1.0).

-include("../include/rtmp.hrl").
-export([wait_for_reply/2]).
-export([connect/1, createStream/1, play/3]).
-export([shared_object_connect/2, shared_object_set/4]).

wait_for_reply(RTMP, InvokeId) when is_integer(InvokeId) ->
  wait_for_reply(RTMP, InvokeId*1.0);
  
%% @private
wait_for_reply(RTMP, InvokeId) ->
  receive
    {rtmp, RTMP, #rtmp_message{type = invoke, body = #rtmp_funcall{command = <<"_result">>, id = InvokeId, args = Args}}} -> Args
  after
    30000 -> erlang:error(timeout)
  end.

%% @spec (RTMP::rtmp_socket()) -> any()
%% @doc Send connect request to server
connect(RTMP) ->
  PlayerInfo = {object, [
    {app,<<>>},
    {flashVer,<<"MAC 10,0,32,18">>},
    {swfUrl,<<"http://localhost/player/Player.swf">>},
    {tcUrl,<<"rtmp://localhost/">>},
    {fpad,false},
    {capabilities,15.0},
    {audioCodecs,3191.0},
    {videoCodecs,252.0},
    {videoFunction,1.0},
    {pageUrl,<<"http://localhost:8082/">>},
    {objectEncoding,0.0}
  ]},
  InvokeId = 1,
  AMF = #rtmp_funcall{
    command = connect,
    type = invoke,
    id = InvokeId,
    stream_id = 0,
    args = [PlayerInfo]
  },
  rtmp_socket:invoke(RTMP, AMF),
  wait_for_reply(RTMP, InvokeId).
  
createStream(RTMP) ->
  InvokeId = 1,
  AMF = #rtmp_funcall{
    command = createStream,
    type = invoke,
    id = InvokeId,
    stream_id = 0,
    args = [null]
  },
  rtmp_socket:invoke(RTMP, AMF),
  [null, StreamId] = wait_for_reply(RTMP, InvokeId),
  round(StreamId).

play(RTMP, Stream, Path) ->
  AMF = #rtmp_funcall{
    command = play,
    type = invoke,
    stream_id = Stream,
    args = [null, Path]
  },
  rtmp_socket:invoke(RTMP, AMF),
  receive
    {rtmp, RTMP, #rtmp_message{type = stream_begin}} -> ok
  after
    30000 -> erlang:error(timeout)
  end.

shared_object_connect(RTMP, Name) ->
  rtmp_socket:send(RTMP, #rtmp_message{type=shared_object, body=#so_message{name = Name, events=[connect]}}),
  receive
    {rtmp, RTMP, #rtmp_message{type = shared_object, body = #so_message{name = Name}}} -> ok
  after
    30000 -> erlang:error(timeout)
  end.
  
shared_object_set(RTMP, Name, Key, Value) ->
  rtmp_socket:send(RTMP, #rtmp_message{type=shared_object, body=#so_message{name = Name, events=[{set_attribute, {Key, Value}}]}}),
  receive
    {rtmp, RTMP, #rtmp_message{type = shared_object, body = #so_message{name = Name}}} -> ok
  after
    30000 -> erlang:error(timeout)
  end.
  

% wait_for(Msg) ->
%   receive
%     Msg -> Msg;
%     Else -> io:format("Else: ~p/~p~n", [Msg, Else]), wait_for(Msg)
%   after
%     10000 -> erlang:error(timeout)
%   end.
% 
%     
