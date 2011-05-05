%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTMP functions that support playing
%%% @reference  See <a href="http://erlyvideo.org" target="_top">http://erlyvideo.org</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(apps_streaming).
-author('Max Lapshin <max@maxidoors.ru>').
-include("../log.hrl").
-include("../rtmp/rtmp_session.hrl").
-include_lib("rtmp/include/rtmp.hrl").

-export([createStream/2, play/2, deleteStream/2, closeStream/2, pause/2, pauseRaw/2, stop/2, seek/2,
         receiveAudio/2, receiveVideo/2, releaseStream/2,
         getStreamLength/2, checkBandwidth/2, 'FCSubscribe'/2, 'DVRGetStreamInfo'/2]).
-export(['WAIT_FOR_DATA'/2, handle_info/2]).

-export([extract_play_args/1, parse_play/2]).

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


'WAIT_FOR_DATA'(_Message, _State) -> unhandled.

handle_info({ems_stream, StreamId, play_complete, LastDTS}, #rtmp_session{socket = Socket} = State) ->
  #rtmp_stream{base_dts = BaseDTS, options = Options} = rtmp_session:get_stream(StreamId, State),
  Start = case {BaseDTS, proplists:get_value(start, Options)} of
    {undefined, undefined} -> 0;
    {Num, _} when is_number(Num) -> Num;
    {_, Num} when is_number(Num) -> Num
  end,
  rtmp_lib:play_complete(Socket, StreamId, [{duration, rtmp:justify_ts(LastDTS - Start)}]),
  State;

handle_info({ems_stream, StreamId, play_failed}, #rtmp_session{socket = Socket} = State) ->
  rtmp_lib:play_failed(Socket, StreamId),
  State;

handle_info({ems_stream, StreamId, seek_success, NewDTS}, #rtmp_session{socket = Socket} = State) ->
  #rtmp_stream{base_dts = BaseDTS} = Stream = rtmp_session:get_stream(StreamId, State),
  
  ?D({self(), "seek to", NewDTS, rtmp:justify_ts(NewDTS - BaseDTS)}),
  rtmp_lib:seek_notify(Socket, StreamId, rtmp:justify_ts(NewDTS - BaseDTS)),
  rtmp_session:set_stream(Stream#rtmp_stream{seeking = false}, State);

handle_info({ems_stream, StreamId, burst_start}, #rtmp_session{socket = Socket} = State) ->
  rtmp_socket:send(Socket, #rtmp_message{type = burst_start, stream_id = StreamId}),
  State;

handle_info({ems_stream, StreamId, burst_stop}, #rtmp_session{socket = Socket} = State) ->
  rtmp_socket:send(Socket, #rtmp_message{type = burst_stop, stream_id = StreamId}),
  State;
  
handle_info({ems_stream, StreamId, seek_failed}, #rtmp_session{socket = Socket} = State) ->
  ?D({"seek failed"}),
  rtmp_lib:seek_failed(Socket, StreamId),
  State;
  
handle_info(_, _) ->
  unhandled.
  
  


%% @private
createStream(#rtmp_session{} = State, AMF) -> 
  #rtmp_stream{stream_id = StreamId} = Stream = rtmp_session:alloc_stream(State),
  rtmp_session:reply(State,AMF#rtmp_funcall{args = [null, StreamId]}),
  rtmp_session:set_stream(Stream, State).

releaseStream(State, #rtmp_funcall{} = _AMF) -> 
  State.


%%-------------------------------------------------------------------------
%% @private
%%-------------------------------------------------------------------------

closeStream(#rtmp_session{} = State, #rtmp_funcall{} = AMF) -> 
  deleteStream(State, AMF).


%%-------------------------------------------------------------------------
%% @private
%%-------------------------------------------------------------------------
deleteStream(#rtmp_session{} = State, #rtmp_funcall{stream_id = StreamId} = _AMF) ->
  case rtmp_session:get_stream(StreamId, State) of
    #rtmp_stream{pid = Player} when is_pid(Player) -> 
      ems_media:stop(Player),
      rtmp_session:flush_stream(StreamId);
    _ -> ok
  end,
  rtmp_session:delete_stream(StreamId, State).


%%-------------------------------------------------------------------------
%% @private
%%-------------------------------------------------------------------------

'FCSubscribe'(State, AMF) -> play(State, AMF).

play(State, #rtmp_funcall{args = [null, null | _]} = AMF) -> stop(State, AMF);
play(State, #rtmp_funcall{args = [null, false | _]} = AMF) -> stop(State, AMF);

play(#rtmp_session{host = Host, socket = Socket} = State, #rtmp_funcall{args = [null, FullName | Args], stream_id = StreamId}) ->
  {Name, Options} = parse_play(FullName, Args),

  ?D({play_request, FullName,Args, '->', Name, Options}),
  case rtmp_session:get_stream(StreamId, State) of
    #rtmp_stream{pid = OldMedia} when is_pid(OldMedia) -> 
      ?D({"Unsubscribe from old", OldMedia}), 
      ems_media:stop(OldMedia),
      rtmp_session:flush_stream(StreamId);
    _ -> ok
  end,

  case media_provider:play(Host, Name, [{stream_id,StreamId},{socket,rtmp_socket:get_socket(Socket)}|Options]) of
    {notfound, _Reason} -> 
      rtmp_socket:status(Socket, StreamId, <<"NetStream.Play.StreamNotFound">>),
      ems_log:access(Host, "NOT_FOUND ~s ~p ~p ~s ~p", [State#rtmp_session.addr, State#rtmp_session.user_id, State#rtmp_session.session_id, Name, StreamId]),
      State;
    {ok, Media} ->
      State1 = rtmp_session:set_stream(#rtmp_stream{pid = Media, stream_id = StreamId, options = Options}, State),
      ems_log:access(Host, "PLAY ~s ~p ~p ~s ~p", [State#rtmp_session.addr, State#rtmp_session.user_id, State#rtmp_session.session_id, Name, StreamId]),
      State1
  end.
  % gen_fsm:send_event(self(), {play, Name, Options}),

parse_play(FullName, Args) ->
  Options1 = extract_play_args(Args),

  {RawName, Args2} = http_uri2:parse_path_query(FullName),
  Name = string:join( [Part || Part <- ems:str_split(RawName, "/"), Part =/= ".."], "/"),
  Options2 = extract_url_args(Args2),

  Options = lists:ukeymerge(1, Options2, Options1),
  {Name, Options}.


  
extract_url_args([]) -> [];
extract_url_args({"start", Start}) -> {start, list_to_integer(Start)*1000};
extract_url_args({"language", Lang}) -> {language, list_to_binary(Lang)};
extract_url_args({"bitrate", Bitrate}) -> {bitrate, list_to_integer(Bitrate)};
extract_url_args({"subtitle", Subtitle}) -> {subtitle, list_to_binary(Subtitle)};
extract_url_args({"duration", Duration}) -> {duration, list_to_integer(Duration)*1000};
extract_url_args({"clients_timeout", Timeout}) -> {clients_timeout, list_to_integer(Timeout)*1000};
extract_url_args({Key, Value}) -> {Key, Value};
extract_url_args(List) -> [extract_url_args(Arg) || Arg <- List].


% Part of RTMP specification.
extract_play_args([]) ->
  case ems:get_var(rtmp_default_wait, false) of
    true -> [{wait,infinity}];
    false -> []
  end;
extract_play_args([Start]) when is_number(Start) andalso Start > 0 -> [{start, Start}];
extract_play_args([Start]) when Start == -2 -> [{wait, infinity}];
extract_play_args([_Start]) -> [];
extract_play_args([Start, Duration]) when is_number(Start) andalso Start > 0 andalso is_number(Duration) andalso Duration > 0 -> [{start, Start}, {duration, Duration}];
extract_play_args([Start, _Duration]) when is_number(Start) andalso Start > 0 -> [{start, Start}];
extract_play_args([Start, Duration]) when is_number(Duration) andalso Start == -1 -> [{wait,Duration}];
extract_play_args([_Start, _Duration]) -> 
  case ems:get_var(rtmp_default_wait, false) of
    true -> [{wait,infinity}];
    false -> []
  end;
extract_play_args([Start, Duration, Reset]) when is_number(Start) andalso Start > 0 andalso Duration > 0 andalso Reset == 0 -> [{start, Start}, {duration, Duration}, {reset, false}];
extract_play_args([Start, _Duration, Reset]) when is_number(Start) andalso Start > 0 andalso Reset == 0 -> [{start, Start}, {reset, false}];
extract_play_args([_Start, _Duration, Reset]) -> [{reset, Reset}].


  
  

%%-------------------------------------------------------------------------
%% @spec (AMF::tuple(),Channel::tuple) -> any()
%% @doc  Processes a pause command and responds
%% @end
%%-------------------------------------------------------------------------
pause(#rtmp_session{socket = Socket} = State, #rtmp_funcall{args = [null, Pausing, NewTs], stream_id = StreamId}) -> 
    ?D({"PAUSE", Pausing, round(NewTs), rtmp_session:get_stream(StreamId, State)}),
    
    case {rtmp_session:get_stream(StreamId, State), Pausing} of
      {null, _} ->
        State;
      {undefined, _} ->
        State;
      {#rtmp_stream{pid = undefined}, _} ->
        State;
      {#rtmp_stream{pid = Player}, true} ->
        ems_media:pause(Player),
        rtmp_lib:pause_notify(Socket, StreamId),
        State;
      {#rtmp_stream{pid = Player}, false} ->
        rtmp_lib:unpause_notify(Socket, StreamId, NewTs),
        ems_media:resume(Player),
        State
    end.


pauseRaw(AMF, State) -> pause(AMF, State).


receiveAudio(#rtmp_session{} = State, #rtmp_funcall{args = [null, Flag], stream_id = StreamId}) when Flag == true orelse Flag == false ->
  Stream = rtmp_session:get_stream(StreamId, State),
  rtmp_session:set_stream(Stream#rtmp_stream{receive_audio = Flag}, State).

receiveVideo(#rtmp_session{} = State, #rtmp_funcall{args = [null, Flag], stream_id = StreamId}) when Flag == true orelse Flag == false ->
  Stream = rtmp_session:get_stream(StreamId, State),
  rtmp_session:set_stream(Stream#rtmp_stream{receive_video = Flag}, State).


getStreamLength(#rtmp_session{host = Host} = State, #rtmp_funcall{args = [null, FullName | _]} = AMF) ->
  
  {RawName, Args} = http_uri2:parse_path_query(FullName),
  Name = string:join( [Part || Part <- ems:str_split(RawName, "/"), Part =/= ".."], "/"),
  Options = extract_url_args(Args),

  {Length,Type} = case proplists:get_value(duration, Options) of 
    undefined ->
      Info = media_provider:info(Host, Name),
      L = proplists:get_value(duration, Info),
      T = proplists:get_value(type, Info),
      {L,T};
    N when is_number(N) ->
      {N / 1000, file}
  end,
  case {Length,Type} of
    {Length, file} when is_number(Length) ->
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
seek(#rtmp_session{socket = Socket} = State, #rtmp_funcall{args = [_, Timestamp], stream_id = StreamId}) ->
  #rtmp_stream{pid = Player, base_dts = BaseDTS} = Stream = rtmp_session:get_stream(StreamId, State),
  ?D({self(), "seek", round(Timestamp), Player}),
  case ems_media:seek(Player, Timestamp + BaseDTS) of
    seek_failed -> 
      rtmp_lib:seek_failed(Socket, StreamId),
      State;
    _ ->
      rtmp_session:set_stream(Stream#rtmp_stream{seeking = true}, State)
  end.
  

%%-------------------------------------------------------------------------
%% @private
%%-------------------------------------------------------------------------
stop(#rtmp_session{host = Host, socket = Socket} = State, #rtmp_funcall{stream_id = StreamId}) -> 
  % ?D({"Stop on", self(), StreamId}),
  case rtmp_session:get_stream(StreamId, State) of
    #rtmp_stream{pid = Player} when is_pid(Player) ->
      ems_media:stop(Player),
      rtmp_session:flush_stream(StreamId),
      ems_log:access(Host, "STOP ~p ~p ~p ~p", [State#rtmp_session.addr, State#rtmp_session.user_id, State#rtmp_session.session_id, StreamId]),
      rtmp_socket:status(Socket, StreamId, <<"NetStream.Play.Stop">>),
      % rtmp_socket:status(Socket, StreamId, <<?NS_PLAY_COMPLETE>>),
      State;
    _ -> State
  end.
  
'DVRGetStreamInfo'(#rtmp_session{host = Host} = State, #rtmp_funcall{args = [null | RawName]} = AMF) ->
  Name = case re:run(RawName, "[^:]+:(.*)", [{capture, [1], binary}]) of
    {match, [N]} -> N;
    _ -> RawName
  end,
  case media_provider:info(Host, Name) of
    undefined ->
      State;
    Info ->
      Reply = [{maxLen,proplists:get_value(timeshift_size, Info)/1000}, {currLen, proplists:get_value(duration,Info)}],
      ?D({'DVRGetStreamInfo', Reply}),
      rtmp_session:reply(State, AMF#rtmp_funcall{args = [null,
        [{data, Reply},{code,<<"NetStream.DVRStreamInfo.Success">>}]
      ]}),
      State
  end.


% Required for latest flash players like (http://www.longtailvideo.com/players/jw-flv-player/)
% http://www.adobe.com/devnet/flashmediaserver/articles/dynamic_stream_switching_04.html
% TODO Stub at this point, need to determine proper response to this call

checkBandwidth(#rtmp_session{} = State, #rtmp_funcall{args = [null | Args]}) ->
  ?D({"checkBandwidth", Args}),
  State.
