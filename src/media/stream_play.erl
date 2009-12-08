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

-module(stream_play).
-author('max@maxidoors.ru').

-include("../../include/ems.hrl").

-export([start_link/2, client/1]).

-export([init/2, ready/1]).



-record(stream_player, {
  consumer,
  media_info,
	stream_id,
	paused = false,
	base_ts = undefined,
	send_audio = true,
	send_video = true,
	sent_video_decoder = false,
	sent_audio_decoder = false,
	synced = false
}).



start_link(MediaEntry, Options) ->
  {ok, spawn_link(?MODULE, init, [MediaEntry, Options])}.
  
client(Player) ->
  Ref = erlang:make_ref(),
  Player ! {client, self(), Ref},
  receive 
    {Info, Ref} -> Info
  after 100 ->
    {undefined, undefined}
  end.

  
init(MediaEntry, Options) ->
  Consumer = proplists:get_value(consumer, Options),
  ?D({"Starting stream play for consumer", Consumer}),
  link(Consumer),
  link(MediaEntry),
  ?MODULE:ready(#stream_player{consumer = Consumer,
                      stream_id = proplists:get_value(stream_id, Options, 1),
                      media_info = MediaEntry}).
  
	
ready(#stream_player{consumer = Consumer, stream_id = StreamId} = State) ->
  receive
    {client_buffer, _ClientBuffer} ->
      ?MODULE:ready(State);
      
    start ->
      erlang:yield(),
      ?MODULE:ready(State);
      
    {client, Pid, Ref} ->
      Pid ! {gen_fsm:sync_send_event(Consumer, info), Ref},
      ?MODULE:ready(State);
      
    pause ->
      ?D("Player paused"),
      ?MODULE:ready(State#stream_player{paused = true});

    resume ->
      ?D("Player resumed"),
      ?MODULE:ready(State#stream_player{paused = false});
      
    {send_audio, Audio} ->
      ?D({"Send audio", Audio}),
      ?MODULE:ready(State#stream_player{send_audio = Audio});

    {send_video, Video} ->
      ?D({"Send video", Video}),
      ?MODULE:ready(State#stream_player{send_video = Video});

    {seek, Timestamp} ->
      ?D({"Requested to seek in stream", Timestamp}),
      ?MODULE:ready(State);
      
    {data, Data} ->
      gen_fsm:send_event(Consumer, {send, Data}),
      ?MODULE:ready(State);

    #video_frame{} = Frame ->
      send_frame(State, Frame);
      
    #channel{msg = Body} = Channel ->
      gen_fsm:send_event(Consumer, {send, {Channel, Body}}),
      ?MODULE:ready(State);

    eof ->
      ?D("MPEG TS finished"),
      gen_fsm:send_event(Consumer, {status, ?NS_PLAY_COMPLETE, StreamId}),
      ?MODULE:ready(State);
    
    stop -> 
      ?D({"stream play stop", self()}),
      ok;
  
    exit ->
      link(State#stream_player.media_info),
      ?D({"stream play exit", self(), State#stream_player.media_info}),
      ok;
      
  	{tcp_closed, _Socket} ->
      error_logger:info_msg("~p Video player lost connection.\n", [self()]),
      ok;
  	Else ->
  	  ?D({"Unknown message", Else}),
  	  ?MODULE:ready(State)
  end.

send_frame(#stream_player{sent_video_decoder = true} = Player, #video_frame{decoder_config = true, type = ?FLV_TAG_TYPE_VIDEO}) ->
  ?MODULE:ready(Player);

send_frame(#stream_player{sent_audio_decoder = true} = Player, #video_frame{decoder_config = true, type = ?FLV_TAG_TYPE_AUDIO}) ->
  ?MODULE:ready(Player);

% % , frame_type = ?FLV_VIDEO_FRAME_TYPE_KEYFRAME
% 
% send_frame(#stream_player{base_ts = undefined} = Player, #video_frame{timestamp = Ts} = Frame) -> % , decoder_config = false, type = ?FLV_TAG_TYPE_VIDEO
%   send_frame(Player#stream_player{base_ts = Ts}, Frame);
% 
% 
% send_frame(#stream_player{consumer = Consumer} = Player, #video_frame{decoder_config = true, type = ?FLV_TAG_TYPE_VIDEO} = Frame) ->
%   ?D({"VideoConfig", Consumer, Frame#video_frame.timestamp, size(Frame#video_frame.body), video}),
%   ems_play:send(Consumer, Frame),
%   ?MODULE:ready(Player#stream_player{sent_video_decoder = true});
% 
% 
% send_frame(#stream_player{consumer = Consumer} = Player, #video_frame{decoder_config = true, type = ?FLV_TAG_TYPE_AUDIO} = Frame) ->
%   ?D({"AudioConfig", Consumer, Frame#video_frame.timestamp, size(Frame#video_frame.body), audio}),
%   ems_play:send(Consumer, Frame),
%   ?MODULE:ready(Player#stream_player{sent_audio_decoder = true});
% 
% send_frame(#stream_player{sent_video_decoder = false} = Player, #video_frame{decoder_config = false}) ->
%   ?MODULE:ready(Player);
%   
% send_frame(#stream_player{base_ts = undefined} = Player, Frame) ->
%   ?D({"Unsynced frame", Frame#video_frame.timestamp, Frame#video_frame.type}),
%   ?MODULE:ready(Player);
% 
% send_frame(#stream_player{consumer = Consumer, stream_id = StreamId, base_ts = BaseTs} = Player, #video_frame{timestamp = Ts, type = Type} = Frame) ->
%   ems_play:send(Consumer, Frame#video_frame{stream_id = StreamId, timestamp = Ts - BaseTs}),
%   % ?D({"Frame", Consumer, Ts - BaseTs, Type, size(Frame#video_frame.body)}),
%   ?MODULE:ready(Player).



  
  
  
  send_frame(#stream_player{base_ts = undefined} = Player, #video_frame{timestamp = Ts} = Frame) when is_number(Ts) andalso Ts > 0 ->
    send_frame(Player#stream_player{base_ts = Ts}, Frame);
  
  send_frame(#stream_player{consumer = Consumer, stream_id = StreamId, base_ts = BaseTs} = Player, 
             #video_frame{timestamp = Ts, decoder_config = Decoder, type = Type} = Frame) ->
    Timestamp = case BaseTs of
      undefined -> 0;
      _ -> Ts - BaseTs
    end,
    ems_play:send(Consumer, Frame#video_frame{stream_id = StreamId, timestamp = Timestamp}),
    % ?D({"Frame", Timestamp, Type, Decoder, Frame#video_frame.frame_type}),
    Player1 = case {Decoder, Type} of
      {true, ?FLV_TAG_TYPE_AUDIO} -> Player#stream_player{sent_audio_decoder = true};
      {true, ?FLV_TAG_TYPE_VIDEO} -> Player#stream_player{sent_video_decoder = true};
      _ -> Player
    end,
    ?MODULE:ready(Player1).


