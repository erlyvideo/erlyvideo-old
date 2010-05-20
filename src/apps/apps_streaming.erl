%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        RTMP functions that support playing
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
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
-module(apps_streaming).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../../include/ems.hrl").
-include("../../include/rtmp_session.hrl").

-export([createStream/2, play/2, deleteStream/2, closeStream/2, pause/2, pauseRaw/2, stop/2, seek/2,
         receiveAudio/2, receiveVideo/2, releaseStream/2,
         getStreamLength/2, checkBandwidth/2, 'FCSubscribe'/2]).
-export(['WAIT_FOR_DATA'/2]).

-export([next_stream/1, extract_play_args/1]).

% 'WAIT_FOR_DATA'({play, Name, Options}, #rtmp_session{socket = Socket, streams = Streams, host = Host} = State) ->
%   {client_buffer, ClientBuffer} = rtmp_socket:getopts(Socket, client_buffer),
%   StreamId = proplists:get_value(stream_id, Options),
%   
%   case ems:element(StreamId, Streams) of
%     CurrentPlayer when is_pid(CurrentPlayer) -> 
%       ?D({"Stop current player", CurrentPlayer}),
%       CurrentPlayer ! exit;
%     _ -> ok
%   end,
%   case media_provider:play(Host, Name, [{client_buffer, ClientBuffer} | Options]) of
%     {ok, Player} ->
%       Player ! start,
%       ems_log:access(Host, "PLAY ~s ~p ~s ~p", [State#rtmp_session.addr, State#rtmp_session.user_id, Name, StreamId]),
%       NewState = State#rtmp_session{streams = ems:setelement(StreamId, Streams, Player)},
%       {next_state, 'WAIT_FOR_DATA', NewState};
%     {notfound, _Reason} ->
%       ems_log:access(Host, "NOTFOUND ~s ~p ~s", [State#rtmp_session.addr, State#rtmp_session.user_id, Name]),
%       rtmp_socket:status(Socket, StreamId, ?NS_PLAY_STREAM_NOT_FOUND),
%       {next_state, 'WAIT_FOR_DATA', State};
%     Reason -> 
%       ?D({"Failed to start video player", Reason}),
%       {error, Reason}
%   end;



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


%% @private
createStream(#rtmp_session{} = State, AMF) -> 
  {State1, StreamId} = next_stream(State),
  rtmp_session:reply(State,AMF#rtmp_funcall{args = [null, StreamId]}),
  %#rtmp_session{streams = Streams} = State1,
  % {ok, Stream} = ems_sup:start_rtmp_stream([{consumer, self()}, {stream_id, StreamId}, {host, Host}]),
  % State1#rtmp_session{streams = setelement(StreamId, Streams, Stream)}.
  State1.

releaseStream(State, _AMF) -> 
  % rtmp_session:reply(State,AMF#rtmp_funcall{args = [null, undefined]}),
  State.

next_stream(State) -> next_stream(State, 1).

next_stream(#rtmp_session{streams = Streams} = State, StreamId) when size(Streams) < StreamId -> 
  {State#rtmp_session{streams = ems:setelement(StreamId, Streams, null)}, StreamId};
next_stream(#rtmp_session{streams = Streams} = State, StreamId) when element(StreamId, Streams) == undefined -> 
  {State#rtmp_session{streams = ems:setelement(StreamId, Streams, null)}, StreamId};
next_stream(State, StreamId) -> 
  next_stream(State, StreamId + 1).


%%-------------------------------------------------------------------------
%% @private
%%-------------------------------------------------------------------------
deleteStream(#rtmp_session{streams = Streams} = State, #rtmp_funcall{stream_id = StreamId} = _AMF) ->
  case ems:element(StreamId, Streams) of
    Player when is_pid(Player) -> ems_media:stop(Player);
    _ -> ok
  end,
  % ?D({"Delete stream", self(), StreamId}),
  State#rtmp_session{streams = ems:setelement(StreamId, Streams, undefined)}.


%%-------------------------------------------------------------------------
%% @private
%%-------------------------------------------------------------------------

'FCSubscribe'(State, AMF) -> play(State, AMF).

play(State, #rtmp_funcall{args = [null, null | _]} = AMF) -> stop(State, AMF);
play(State, #rtmp_funcall{args = [null, false | _]} = AMF) -> stop(State, AMF);

play(#rtmp_session{host = Host, streams = Streams} = State, #rtmp_funcall{args = [null, Name | Args], stream_id = StreamId}) ->
  Options = extract_play_args(Args),
  case media_provider:find_or_open(Host, Name) of
    {notfound, _Reason} -> 
      State;
    Media when is_pid(Media) ->
      self() ! {ems_stream, StreamId, start_play},
      ems_media:play(Media, [{stream_id,StreamId}|Options]),
      ems_log:access(Host, "PLAY ~s ~p ~s ~p", [State#rtmp_session.addr, State#rtmp_session.user_id, Name, StreamId]),
      State#rtmp_session{streams = ems:setelement(StreamId, Streams, Media)}
  end.
  % gen_fsm:send_event(self(), {play, Name, Options}),
  


% Part of RTMP specification.
extract_play_args([]) -> [{wait,infinity}];
extract_play_args([Start]) when Start > 0 -> [{start, Start}];
extract_play_args([Start]) when Start == -2 -> [{wait, infinity}];
extract_play_args([_Start]) -> [];
extract_play_args([Start, Duration]) when Start > 0 andalso Duration > 0 -> [{start, Start}, {duration, Duration}];
extract_play_args([Start, _Duration]) when Start > 0 -> [{start, Start}];
extract_play_args([Start, Duration]) when Start == -1 -> [{wait,Duration}];
extract_play_args([_Start, _Duration]) -> [{wait,infinity}];
extract_play_args([Start, Duration, Reset]) when Start > 0 andalso Duration > 0 andalso Reset == 0 -> [{start, Start}, {duration, Duration}, {reset, false}];
extract_play_args([Start, _Duration, Reset]) when Start > 0 andalso Reset == 0 -> [{start, Start}, {reset, false}];
extract_play_args([_Start, _Duration, Reset]) -> [{reset, Reset}].


  
  

%%-------------------------------------------------------------------------
%% @spec (AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a pause command and responds
%% @end
%%-------------------------------------------------------------------------
pause(#rtmp_session{streams = Streams, socket = Socket} = State, #rtmp_funcall{args = [null, Pausing, NewTs], stream_id = StreamId}) -> 
    ?D({"PAUSE", Pausing, round(NewTs)}),
    Player = ems:element(StreamId, Streams),
    case Pausing of
      true ->
        ems_media:pause(Player),
        rtmp_lib:pause_notify(Socket, StreamId),
        State;
      false ->
        ems_media:resume(Player),
        rtmp_socket:status(Socket, StreamId, ?NS_UNPAUSE_NOTIFY),
        State
    end.


pauseRaw(AMF, State) -> pause(AMF, State).


receiveAudio(#rtmp_session{streams = Streams} = State, #rtmp_funcall{args = [null, Audio], stream_id = StreamId}) ->
  Player = ems:element(StreamId, Streams),
  (catch Player ! {send_audio, Audio}),
  State.

receiveVideo(#rtmp_session{streams = Streams} = State, #rtmp_funcall{args = [null, Video], stream_id = StreamId}) ->
  Player = ems:element(StreamId, Streams),
  (catch Player ! {send_video, Video}),
  State.


getStreamLength(#rtmp_session{host = Host} = State, #rtmp_funcall{args = [null, Name | _]} = AMF) ->
  Info = media_provider:info(Host, Name),
  case proplists:get_value(duration, Info) of
    Length when is_number(Length) ->
      ?D({"getStreamLength", Name, Length}),
      rtmp_session:reply(State,AMF#rtmp_funcall{args = [null, Length]});
    _ ->
      ?D({"getStreamLength", Name, undefined}),
      rtmp_session:reply(State,AMF#rtmp_funcall{args = [null, 0]}),
      ok
  end,
  State.

%%-------------------------------------------------------------------------
%% @spec (AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a seek command and responds
%% @end
%%-------------------------------------------------------------------------
seek(#rtmp_session{streams = Streams} = State, #rtmp_funcall{args = [_, Timestamp], stream_id = StreamId}) -> 
  Player = ems:element(StreamId, Streams),
  ?D({"seek", round(Timestamp), Player}),
  case ems_media:seek(Player, before, Timestamp) of
    {seek_success, NewTimestamp} ->
      self() ! {ems_stream, StreamId, seek_notify, NewTimestamp};
    seek_failed ->
      self() ! {ems_stream, StreamId, seek_failed}
  end,
  State.
  

%%-------------------------------------------------------------------------
%% @private
%%-------------------------------------------------------------------------
stop(#rtmp_session{host = Host, socket = Socket, streams = Streams} = State, #rtmp_funcall{stream_id = StreamId}) -> 
  % ?D({"Stop on", self(), StreamId}),
  case ems:element(StreamId, Streams) of
    Player when is_pid(Player) ->
      ems_media:stop(Player),
      ems_log:access(Host, "STOP ~p ~p ~p", [State#rtmp_session.addr, State#rtmp_session.user_id, StreamId]),
      rtmp_socket:status(Socket, StreamId, <<?NS_PLAY_STOP>>),
      % rtmp_socket:status(Socket, StreamId, <<?NS_PLAY_COMPLETE>>),
      State;
    _ -> State
  end.

%%-------------------------------------------------------------------------
%% @private
%%-------------------------------------------------------------------------

closeStream(#rtmp_session{streams = Streams} = State, #rtmp_funcall{stream_id = StreamId} = _AMF) -> 
  case ems:element(StreamId, Streams) of
    undefined -> State;
    null ->
      State#rtmp_session{streams = ems:setelement(StreamId, Streams, undefined)};
    Player when is_pid(Player) ->
      ems_media:stop(Player),
      State#rtmp_session{streams = ems:setelement(StreamId, Streams, undefined)}
  end.

% Required for latest flash players like (http://www.longtailvideo.com/players/jw-flv-player/)
% http://www.adobe.com/devnet/flashmediaserver/articles/dynamic_stream_switching_04.html
% TODO Stub at this point, need to determine proper response to this call

checkBandwidth(#rtmp_session{} = State, #rtmp_funcall{args = [null | Args]}) ->
  ?D({"checkBandwidth", Args}),
  State.
