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
-author(max@maxidoors.ru).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-include("../../include/ems.hrl").

-export([createStream/2, play/2, deleteStream/2, closeStream/2, pause/2, pauseRaw/2, stop/2, seek/2,
         getStreamLength/2, prepareStream/1]).
-export(['WAIT_FOR_DATA'/2]).


'WAIT_FOR_DATA'({play, Name, StreamId}, #rtmp_client{video_player = CurrentPlayer, client_buffer = ClientBuffer} = State) ->
  case CurrentPlayer of
    undefined -> ok;
    _ -> 
      ?D({"Stop current player", CurrentPlayer}),
      CurrentPlayer ! exit
  end,
  case media_provider:play(Name, [{stream_id, StreamId}, {client_buffer, ClientBuffer}]) of
    {ok, Player} ->
      ?D({"Player starting", Player}),
      Player ! start,
      {next_state, 'WAIT_FOR_DATA', State#rtmp_client{video_player = Player}, ?TIMEOUT};
    {notfound, _Reason} ->
      gen_fsm:send_event(self(), {status, ?NS_PLAY_STREAM_NOT_FOUND, 1}),
      {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};
    Reason -> 
      ?D({"Failed to start video player", Reason}),
      {error, Reason}
  end;



'WAIT_FOR_DATA'({metadata, Command, AMF, Stream}, State) ->
  gen_fsm:send_event(self(), {send, {
    #channel{id = 4, timestamp = 0, type = ?RTMP_TYPE_METADATA_AMF0, stream = Stream}, 
    <<(amf0:encode(list_to_binary(Command)))/binary, (amf0:encode({object, AMF}))/binary>>}}),
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
createStream(#amf{id = TxId}, State) -> 
  apps_rtmp:reply(TxId, [null, 1]),
  gen_fsm:send_event(self(), {send, {#channel{timestamp = 0, id = 2, stream = 0, type = ?RTMP_TYPE_CHUNK_SIZE}, ?RTMP_PREF_CHUNK_SIZE}}),
    State.


%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a deleteStream command and responds
%% @end
%%-------------------------------------------------------------------------
deleteStream(_AMF, #rtmp_client{video_player = undefined} = State) ->
  State;
  
deleteStream(_AMF, #rtmp_client{video_player = Player} = State) when is_pid(Player) ->
  Player ! stop,
  State.


%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a play command and responds
%% @end
%%-------------------------------------------------------------------------

play(#amf{args = [_Null, false | _]} = AMF, State) -> stop(AMF, State);

play(#amf{args = [_Null, Name]}, State) ->
  StreamId = 1,
  ?D({"PLAY", Name}),
  prepareStream(StreamId),
  gen_fsm:send_event(self(), {play, Name, StreamId}),
  State.

prepareStream(StreamId) ->
  gen_fsm:send_event(self(), {control, ?RTMP_CONTROL_STREAM_RECORDED, StreamId}),
  gen_fsm:send_event(self(), {control, ?RTMP_CONTROL_STREAM_BEGIN, StreamId}),
  gen_fsm:send_event(self(), {status, ?NS_PLAY_START, StreamId}),
  gen_fsm:send_event(self(), {status, ?NS_PLAY_RESET, StreamId}).
  
  

%%-------------------------------------------------------------------------
%% @spec (AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a pause command and responds
%% @end
%%-------------------------------------------------------------------------
pause(#amf{args = [null, Pausing, NewTs]}, #rtmp_client{video_player = Player} = State) -> 
    ?D({"PAUSE", Pausing, round(NewTs)}),
    
    case Pausing of
      true ->
        Player ! pause,
        gen_fsm:send_event(self(), {status, ?NS_PAUSE_NOTIFY, 1}),
        State;
      false ->
        Player ! resume,
        gen_fsm:send_event(self(), {status, ?NS_UNPAUSE_NOTIFY, 1}),
        State
    end.


pauseRaw(AMF, State) -> pause(AMF, State).


getStreamLength(#amf{args = [null | Args]}, #rtmp_client{} = State) ->
  ?D({"getStreamLength", Args}),
  State.

%%-------------------------------------------------------------------------
%% @spec (AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a seek command and responds
%% @end
%%-------------------------------------------------------------------------
seek(#amf{args = [_, Timestamp]}, #rtmp_client{video_player = Player} = State) -> 
  ?D({"invoke - seek", Timestamp}),
  StreamId = 1,
  Player ! {seek, Timestamp},
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
stop(_AMF, #rtmp_client{video_player = undefined} = State) -> 
  ?D({"Requested to stop dead player"}),
  State;
  
stop(_AMF, #rtmp_client{video_player = Player} = State) -> 
  ?D({"Stopping video player", Player}),
  Player ! stop,
  State.

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a closeStream command and responds
%% @end
%%-------------------------------------------------------------------------
closeStream(_AMF, #rtmp_client{video_player = undefined} = State) ->
  % ?D("invoke - closeStream"),
  State;

closeStream(_AMF, #rtmp_client{video_player = Player} = State) ->
  % ?D("invoke - closeStream and stop player"),
  Player ! stop,
  State.

