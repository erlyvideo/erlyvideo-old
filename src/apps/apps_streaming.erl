%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Stuart Jackson <simpleenigmainc@gmail.com> [http://erlsoft.org]
%%% @author     Luke Hubbard <luke@codegent.com> [http://www.codegent.com]
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2007 Luke Hubbard, Stuart Jackson, Roberto Saccon, 2009 Max Lapshin
%%% @doc        Generalized RTMP application behavior module
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
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
-module(apps_streaming).
-author(max@maxidoors.ru).
-author('rsaccon@gmail.com').
-author('simpleenigmainc@gmail.com').
-author('luke@codegent.com').
-include("../../include/ems.hrl").

-export([createStream/2, play/2, deleteStream/2, closeStream/2, pause/2, pauseRaw/2, stop/2, seek/2,
         receiveAudio/2, receiveVideo/2, releaseStream/2,
         getStreamLength/2, prepareStream/2, checkBandwidth/2]).
-export(['WAIT_FOR_DATA'/2]).


'WAIT_FOR_DATA'({play, Name, Options}, #rtmp_session{socket = Socket, streams = Streams, host = Host} = State) ->
  {client_buffer, ClientBuffer} = rtmp_socket:getopts(Socket, client_buffer),
  StreamId = proplists:get_value(stream_id, Options),
  
  case array:get(StreamId, Streams) of
    CurrentPlayer when is_pid(CurrentPlayer) -> 
      ?D({"Stop current player", CurrentPlayer}),
      CurrentPlayer ! exit;
    _ -> ok
  end,
  case media_provider:play(Host, Name, [{client_buffer, ClientBuffer} | Options]) of
    {ok, Player} ->
      Player ! start,
      ems_log:access(Host, "PLAY ~p ~p ~s", [State#rtmp_session.addr, State#rtmp_session.user_id, Name]),
      {next_state, 'WAIT_FOR_DATA', State#rtmp_session{streams = array:set(StreamId, Player, Streams)}};
    {notfound, _Reason} ->
      ems_log:access(Host, "NOTFOUND ~p ~p ~s", [State#rtmp_session.addr, State#rtmp_session.user_id, Name]),
      rtmp_socket:status(Socket, StreamId, ?NS_PLAY_STREAM_NOT_FOUND),
      {next_state, 'WAIT_FOR_DATA', State};
    Reason -> 
      ?D({"Failed to start video player", Reason}),
      {error, Reason}
  end;



'WAIT_FOR_DATA'({metadata, Command, AMF, StreamId}, #rtmp_session{socket = Socket} = State) ->
  Socket ! #rtmp_message{
    channel_id = 4, 
    timestamp = 0, 
    type = metadata, 
    stream_id = StreamId, 
    body = <<(amf0:encode(list_to_binary(Command)))/binary, (amf0:encode({object, AMF}))/binary>>
  },
  {next_state, 'WAIT_FOR_DATA', State};

'WAIT_FOR_DATA'({metadata, Command, AMF}, State) -> 'WAIT_FOR_DATA'({metadata, Command, AMF, 0}, State);


'WAIT_FOR_DATA'(_Message, _State) -> {unhandled}.


%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a createStream command and responds
%% @end
%%-------------------------------------------------------------------------
createStream(State, AMF) -> 
  {State1, StreamId} = next_stream(State),
  apps_rtmp:reply(State,AMF#rtmp_funcall{args = [null, StreamId]}),
  State1.

releaseStream(State, _AMF) -> 
  % apps_rtmp:reply(State,AMF#rtmp_funcall{args = [null, undefined]}),
  State.

next_stream(State) -> next_stream(State, 1).
next_stream(#rtmp_session{streams = Streams} = State, Stream) ->
  case array:get(Stream, Streams) of
    undefined -> {State#rtmp_session{streams = array:set(Stream, null, Streams)}, Stream};
    _ -> next_stream(State, Stream + 1)
  end.


%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a deleteStream command and responds
%% @end
%%-------------------------------------------------------------------------
deleteStream(#rtmp_session{streams = Streams} = State, #rtmp_funcall{stream_id = StreamId} = _AMF) ->
  case array:get(StreamId, Streams) of
    Player when is_pid(Player) -> Player ! stop;
    _ -> ok
  end,
  State#rtmp_session{streams = array:reset(StreamId, Streams)}.


%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a play command and responds
%% @end
%%-------------------------------------------------------------------------

play(State, #rtmp_funcall{args = [_Null, false | _]} = AMF) -> stop(State, AMF);

play(State, #rtmp_funcall{args = [_Null, Name | Args], stream_id = StreamId}) ->
  Options = [{stream_id, StreamId} | extract_play_args(Args)],
  ?D({"PLAY", Name, Options}),
  prepareStream(State, StreamId),
  gen_fsm:send_event(self(), {play, Name, Options}),
  State.


% Part of RTMP specification.
extract_play_args([]) -> [];
extract_play_args([Start]) -> [{start, Start}];
extract_play_args([Start, Duration]) -> [{start, Start}, {duration, Duration}];
extract_play_args([Start, Duration, Reset]) -> [{start, Start}, {duration, Duration}, {reset, Reset}].



prepareStream(#rtmp_session{socket = Socket}, StreamId) ->
  rtmp_socket:send(Socket, #rtmp_message{type = stream_recorded, stream_id = StreamId}),
  rtmp_socket:send(Socket, #rtmp_message{type = stream_begin, stream_id = StreamId}),
  rtmp_socket:status(Socket, StreamId, ?NS_PLAY_START),
  rtmp_socket:status(Socket, StreamId, ?NS_PLAY_RESET).
  
  

%%-------------------------------------------------------------------------
%% @spec (AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a pause command and responds
%% @end
%%-------------------------------------------------------------------------
pause(#rtmp_session{streams = Streams, socket = Socket} = State, #rtmp_funcall{args = [null, Pausing, NewTs], stream_id = StreamId}) -> 
    ?D({"PAUSE", Pausing, round(NewTs)}),
    Player = array:get(StreamId, Streams),
    case Pausing of
      true ->
        Player ! pause,
        rtmp_socket:status(Socket, StreamId, ?NS_PAUSE_NOTIFY),
        State;
      false ->
        Player ! resume,
        rtmp_socket:status(Socket, StreamId, ?NS_UNPAUSE_NOTIFY),
        State
    end.


pauseRaw(AMF, State) -> pause(AMF, State).


receiveAudio(#rtmp_session{streams = Streams} = State, #rtmp_funcall{args = [null, Audio], stream_id = StreamId}) ->
  Player = array:get(StreamId, Streams),
  (catch Player ! {send_audio, Audio}),
  State.

receiveVideo(#rtmp_session{streams = Streams} = State, #rtmp_funcall{args = [null, Video], stream_id = StreamId}) ->
  Player = array:get(StreamId, Streams),
  (catch Player ! {send_video, Video}),
  State.


getStreamLength(#rtmp_session{} = State, #rtmp_funcall{args = [null | Args]}) ->
  ?D({"getStreamLength", Args}),
  State.

%%-------------------------------------------------------------------------
%% @spec (AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a seek command and responds
%% @end
%%-------------------------------------------------------------------------
seek(#rtmp_session{streams = Streams, socket = Socket} = State, #rtmp_funcall{args = [_, Timestamp], stream_id = StreamId}) -> 
  ?D({"invoke - seek", Timestamp}),
  Player = array:get(StreamId, Streams),
  Player ! {seek, Timestamp},
  rtmp_socket:status(Socket, StreamId, ?NS_SEEK_NOTIFY),
  rtmp_socket:send(Socket, #rtmp_message{type = stream_recorded, stream_id = StreamId}),
  rtmp_socket:send(Socket, #rtmp_message{type = stream_begin, stream_id = StreamId}),
  rtmp_socket:status(Socket, StreamId, ?NS_PLAY_START),
  State.
  

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a stop command and responds
%% @end
%%-------------------------------------------------------------------------
stop(#rtmp_session{streams = Streams} = State, #rtmp_funcall{stream_id = StreamId} = _AMF) -> 
  case array:get(StreamId, Streams) of
    Player when is_pid(Player) ->
      Player ! exit,
      State#rtmp_session{streams = array:set(StreamId, null, Streams)};
    _ -> State
  end.

%%-------------------------------------------------------------------------
%% @spec (From::pid(),AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a closeStream command and responds
%% @end
%%-------------------------------------------------------------------------

closeStream(#rtmp_session{streams = Streams} = State, #rtmp_funcall{stream_id = StreamId} = _AMF) -> 
case array:get(StreamId, Streams) of
  undefined -> State;
  Player ->
    Player ! exit,
    State#rtmp_session{streams = array:reset(StreamId, Streams)}
end.

% Required for latest flash players like (http://www.longtailvideo.com/players/jw-flv-player/)
% http://www.adobe.com/devnet/flashmediaserver/articles/dynamic_stream_switching_04.html
% TODO Stub at this point, need to determine proper response to this call

checkBandwidth(#rtmp_session{} = State, #rtmp_funcall{args = [null | Args]}) ->
  ?D({"checkBandwidth", Args}),
  State.
