-module(rtmp_lib).
-author(max@maxidoors.ru).

-include("../include/rtmp.hrl").
-export([wait_for_reply/2]).
-export([connect/1, createStream/1, play/3]).

wait_for_reply(RTMP, InvokeId) when is_integer(InvokeId) ->
  wait_for_reply(RTMP, InvokeId*1.0);
  

wait_for_reply(RTMP, InvokeId) ->
  receive
    {rtmp, RTMP, #rtmp_message{type = invoke, body = #amf{command = <<"_result">>, id = InvokeId, args = Args}}} -> Args
  after
    10000 -> erlang:error(timeout)
  end.

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
  AMF = #amf{
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
  AMF = #amf{
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
  AMF = #amf{
    command = play,
    type = invoke,
    stream_id = Stream,
    args = [null, Path]
  },
  rtmp_socket:invoke(RTMP, AMF),
  receive
    {rtmp, RTMP, #rtmp_message{type = stream_begin}} -> ok
  after
    10000 -> erlang:error(timeout)
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
