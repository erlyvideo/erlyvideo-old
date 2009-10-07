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
-module(apps_streaming).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-include("../include/ems.hrl").

-export([createStream/2, play/2, deleteStream/2, closeStream/2, pause/2, pauseRaw/2, stop/2, seek/2]).
-export(['WAIT_FOR_DATA'/2]).


'WAIT_FOR_DATA'({play, Name, StreamId}, State) ->
  case ems_play:play(Name, StreamId, State#ems_fsm{type = vod}) of
    {ok, PlayerPid} ->
      NextState = State#ems_fsm{type  = vod, video_player = PlayerPid},
      gen_fsm:send_event(PlayerPid, {start}),
      {next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};
    {notfound} ->
      gen_fsm:send_event(self(), {status, ?NS_PLAY_STREAM_NOT_FOUND, 1}),
      {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};
    Reason -> 
      ?D({"Failed to start video player", Reason}),
      {error, Reason}
  end;

'WAIT_FOR_DATA'({stop}, #ems_fsm{video_device = IoDev, video_buffer = Buffer, video_timer_ref = TimerRef, type = _Type} = State) ->
	case Buffer of
		undefined -> ok;
		_ -> file:write(IoDev, lists:reverse(Buffer))
	end,
  case IoDev of
      undefined -> ok;
      _ -> file:close(IoDev)
  end,
  case TimerRef of
      undefined -> ok;
      _ -> gen_fsm:cancel_timer(TimerRef)
  end,
  case type of
      live -> 
          ems_cluster:unsubscribe(State#ems_fsm.video_file_name, self());
      wait -> 
          ems_cluster:unsubscribe(State#ems_fsm.video_file_name, self());
      broadcast ->
          emscluster:stop_broadcast(State#ems_fsm.video_file_name);
      _ -> 
          ok
  end,
	NextState = State#ems_fsm{video_device=undefined,video_buffer=[],
							  video_timer_ref=undefined,video_pos = 0},
  {next_state, 'WAIT_FOR_DATA', NextState, ?TIMEOUT};


'WAIT_FOR_DATA'({metadata, Command, AMF, Stream}, State) ->
  gen_fsm:send_event(self(), {send, {
    #channel{id = 4, timestamp = 0, type = ?RTMP_TYPE_METADATA, stream = Stream}, 
    <<(ems_amf:encode(Command))/binary, (ems_amf:encode_mixed_array(AMF))/binary>>}}),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};

'WAIT_FOR_DATA'({metadata, Command, AMF}, State) -> 'WAIT_FOR_DATA'({metadata, Command, AMF, 0}, State);




'WAIT_FOR_DATA'({video, Data}, State) ->
  Channel = #channel{id=5,timestamp=0, length=size(Data),type = ?RTMP_TYPE_VIDEO,stream=1},
  'WAIT_FOR_DATA'({send, {Channel, Data}}, State);

'WAIT_FOR_DATA'({audio, Data}, State) ->
  Channel = #channel{id=4,timestamp=0, length=size(Data),type = ?RTMP_TYPE_AUDIO,stream=1},
  'WAIT_FOR_DATA'({send, {Channel, Data}}, State);



'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.


%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a createStream command and responds
%% @end
%%-------------------------------------------------------------------------
createStream(AMF, State) -> 
    ?D({"invoke - createStream", AMF}),
    Id = 1, % New stream ID
    NewAMF = AMF#amf{
      id = 2.0,
    	command = '_result',
    	args = [null, Id]},
    % gen_fsm:send_event(self(), {send, {#channel{timestamp = 0, id = 2},NewAMF}}),
    gen_fsm:send_event(self(), {invoke, NewAMF}),
    gen_fsm:send_event(self(), {send, {#channel{timestamp = 0, id = 2, stream = 0, type = ?RTMP_TYPE_CHUNK_SIZE}, ?RTMP_PREF_CHUNK_SIZE}}),
    State.


%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a deleteStream command and responds
%% @end
%%-------------------------------------------------------------------------
deleteStream(_AMF, #ems_fsm{video_player = Player} = State) ->
  ems_fsm:send_event(Player, {stop}),
  ?D("invoke - deleteStream"),
  State.


%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a play command and responds
%% @end
%%-------------------------------------------------------------------------

play(#amf{args = [_Null, {boolean, false} | _]} = AMF, State) -> stop(AMF, State);

play(AMF, #ems_fsm{video_player = Player} = State) ->
  StreamId = 1,
  Channel = #channel{id = 5, timestamp = 0, stream = StreamId},
  [_Null,{string,Name}] = AMF#amf.args,
  ?D({"invoke - play", Name, AMF}),
  case Player of
    undefined -> ok;
    _ -> gen_fsm:send_event(Player, {stop})
  end,
  gen_fsm:send_event(self(), {control, ?RTMP_CONTROL_STREAM_RECORDED, StreamId}),
  gen_fsm:send_event(self(), {control, ?RTMP_CONTROL_STREAM_BEGIN, StreamId}),
  gen_fsm:send_event(self(), {status, ?NS_PLAY_START, 1}),
  gen_fsm:send_event(self(), {status, ?NS_PLAY_RESET, 1}),
  gen_fsm:send_event(self(), {play, Name, Channel#channel.stream}),
  State#ems_fsm{video_player = undefined}.


%%-------------------------------------------------------------------------
%% @spec (AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a pause command and responds
%% @end
%%-------------------------------------------------------------------------
pause(AMF, #ems_fsm{video_player = Player} = State) -> 
    ?D({"invoke - pause", AMF}),
    [_, {boolean, Pausing}, {number, _Timestamp}] = AMF#amf.args,
    
    case Pausing of
      true ->
        gen_fsm:send_event(Player, {pause}),
        gen_fsm:send_event(self(), {status, ?NS_PAUSE_NOTIFY, 1}),
        State;
      false ->
        gen_fsm:send_event(Player, {resume}),
        gen_fsm:send_event(self(), {status, ?NS_UNPAUSE_NOTIFY, 1}),
        State
    end.


pauseRaw(AMF, State) -> pause(AMF, State).


%%-------------------------------------------------------------------------
%% @spec (AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a seek command and responds
%% @end
%%-------------------------------------------------------------------------
seek(AMF, #ems_fsm{video_player = Player} = State) -> 
  ?D({"invoke - seek", AMF#amf.args}),
  [_, {number, Timestamp}] = AMF#amf.args,
  StreamId = 1,
  gen_fsm:send_event(Player, {seek, Timestamp}),
  gen_fsm:send_event(self(), {status, ?NS_SEEK_NOTIFY, 1}),
  gen_fsm:send_event(self(), {control, ?RTMP_CONTROL_STREAM_RECORDED, StreamId}),
  gen_fsm:send_event(self(), {control, ?RTMP_CONTROL_STREAM_BEGIN, StreamId}),
  gen_fsm:send_event(self(), {status, ?NS_PLAY_START, 1}),
  State.
  

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a stop command and responds
%% @end
%%-------------------------------------------------------------------------
stop(_AMF, #ems_fsm{video_player = Player} = State) -> 
    ?D("invoke - stop"),
    gen_fsm:send_event(Player, {stop}),
    State.

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a closeStream command and responds
%% @end
%%-------------------------------------------------------------------------
closeStream(_AMF, State) ->
    ?D("invoke - closeStream"),
    gen_fsm:send_event(self(), {stop}),
    State.

