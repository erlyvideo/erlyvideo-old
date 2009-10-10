%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009 Max Lapshin
%%% @doc        Player module
%%% @reference  See <a href="http://github.com/maxlapshin/erlyvideo" target="_top">http://github.com/maxlapshin/erlyvideo</a> for more information
%%% @end
%%%
%%%
%%% Copyright (c) 2009 Max Lapshin
%%%    This program is free software: you can redistribute it and/or modify
%%%    it under the terms of the GNU Affero General Public License as
%%%    published by the Free Software Foundation, either version 3 of the
%%%    License, or any later version.
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

-module(ems_play).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-author('max@maxidoors.ru').

-include("../include/ems.hrl").


-export([play/3, send/2, channel_id/2]).


play(Name, StreamId, State) ->
  %   case filelib:is_regular(FileName) of
  %     true ->
  %     _ ->
  %       ems_cluster:subscribe(self(), Name),
  %       NextState = State#ems_fsm{type  = wait},
  %       {next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT}
  %     % end
  % end;
  init_file(Name, StreamId, State).
  
  
init_file(Name, StreamId, State) ->
  FileName = filename:join([file_play:file_dir(), Name]), 
  case filelib:is_regular(FileName) of
    true -> gen_fsm:start_link(file_play, {FileName, StreamId, State, self()}, []);
    _ -> init_mpeg_ts(FileName, StreamId, State)
  end.
  
init_mpeg_ts(FileName, StreamId,  State) ->
  {ok, Re} = re:compile("http://(.*).ts"),
  case re:run(FileName, Re) of
    {match, _Captured} -> mpeg_ts:play(FileName, StreamId, State);
    _ -> init_stream(FileName, StreamId, State)
  end.

init_stream(Name, _StreamId, _State) ->
  case ems:get_var(netstream, undefined) of
    undefined -> {notfound};
    NetStreamNode -> case rpc:call(NetStreamNode, rtmp, start, [Name], ?TIMEOUT) of
      {ok, NetStream} ->
        link(NetStream),
        ?D({"Netstream created", NetStream}),
        {ok, NetStream};
      _ ->
        {notfound}
      end
  end.


%%-------------------------------------------------------------------------
%% @spec (FLV_TAG::tuple()) -> any()
%% @doc Convert FLV_Tag into Channel then transmit the Channel and Body
%% @end
%%-------------------------------------------------------------------------

send(Consumer, #video_frame{type = Type, streamid=StreamId,timestamp_abs = TimeStamp,body=Body, raw_body = false} = Frame) when is_binary(Body) ->
	Channel = #channel{id = channel_id(Type, StreamId),timestamp=TimeStamp,length=size(Body),type=Type,stream=StreamId},
	gen_fsm:send_event(Consumer, {send, {Channel, ems_flv:encode(Frame)}});

send(Consumer, #video_frame{type = Type, streamid=StreamId,timestamp_abs = TimeStamp,body=Body}) when is_binary(Body) ->
	Channel = #channel{id=channel_id(Type, StreamId),timestamp=TimeStamp,length=size(Body),type=Type,stream=StreamId},
	gen_fsm:send_event(Consumer, {send, {Channel,Body}}).


% rsaccon: TODO: streams per connections need to be stored and channelId retrieved from stream
% idea: a  process per stream, mnesia RAM table (with streamid as key) contains stream process PID
channel_id(?FLV_TAG_TYPE_META, _StreamId) -> 4;
channel_id(?FLV_TAG_TYPE_VIDEO, _StreamId) -> 5;
channel_id(?FLV_TAG_TYPE_AUDIO, _StreamId) -> 5.
% channel_id(?FLV_TAG_TYPE_AUDIO, _StreamId) -> 6.
