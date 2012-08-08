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
-include_lib("rtmp/include/rtmp.hrl").
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").

-export([play/2, pause/2, pauseRaw/2, stop/2, seek/2,
         getStreamLength/2, checkBandwidth/2, 'FCSubscribe'/2, 'DVRGetStreamInfo'/2]).

-export([extract_play_args/1, parse_play/2]).

  



%%-------------------------------------------------------------------------
%% @private
%%-------------------------------------------------------------------------

'FCSubscribe'(Session, #rtmp_funcall{stream_id = StreamId, args = [null, Name]}) ->
  RTMP = rtmp_session:get(Session, socket),
  Reply = {object, [
    {code, <<"NetStream.Play.Start">>}, 
    {level, <<"status">>}, 
    {description, <<"FCSubscribe to ", Name/binary>>}
  ]},
  Status = #rtmp_funcall{
    command = 'onFCSubscribe',
    type = invoke,
    id = 0,
    stream_id = StreamId,
    args = [null, Reply]},
  rtmp_socket:invoke(RTMP, Status),
  Session.

play(State, #rtmp_funcall{args = [null, null | _]} = AMF) -> stop(State, AMF);
play(State, #rtmp_funcall{args = [null, false | _]} = AMF) -> stop(State, AMF);

play(State, #rtmp_funcall{args = [null, FullName | Args], stream_id = StreamId} = RtmpFuncCall) ->
  Host = rtmp_session:get(State, host),
  Socket = rtmp_session:get(State, socket),
  Addr = rtmp_session:get(State, addr),
  UserId = rtmp_session:get(State, user_id),
  SessionId = rtmp_session:get(State, session_id),
  {Name, Options} = parse_play(FullName, Args),


  % ?D({play_request, StreamId, FullName,Args, '->', Name, Options}),
  Stream = rtmp_session:get_stream(StreamId, State),
  State1 = case rtmp_stream:get(Stream, pid) of
    OldMedia when is_pid(OldMedia) -> 
      ?D({"Unsubscribe from old", OldMedia}), 
      rtmp_session:closeStream(State, RtmpFuncCall);
    _ ->
      State
  end,
  
  
  % SocketOptions = case {rtmp_session:get(State, disable_accel), rtmp_socket:get_socket(Socket)} of
  %   {true, _} -> [];
  %   {_, {rtmp, RTMPSocket}} -> [{socket,{rtmp,RTMPSocket}}];
  %   _ -> []
  % end,
  % Totally remove useless acceleration
  SocketOptions = [],
  case media_provider:open(Host, Name) of
    {notfound, _Reason} -> 
      rtmp_socket:status(Socket, StreamId, <<"NetStream.Play.StreamNotFound">>),
      ems_log:access(Host, "NOT_FOUND ~s ~p ~p ~s ~p", [Addr, UserId, SessionId, Name, StreamId]),
      State1;
    {ok, Media} ->
      erlang:monitor(process,Media),
      Stream = case rtmp_session:get_stream(StreamId, State1) of
        false -> rtmp_stream:construct([]);
        Stream_ -> Stream_
      end,
      State2 = rtmp_session:set_stream(rtmp_stream:set(Stream, [{pid, Media}, {stream_id, StreamId}, {options, Options}, {name, Name}, {started, false}]), State1),
      ems_media:play(Media, SocketOptions ++ [{stream_id,StreamId},{host,Host}|Options]),
      ems_log:access(Host, "PLAY ~s ~p ~p ~s ~p", [Addr, UserId, SessionId, Name, StreamId]),
      State2
  end.

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
extract_play_args([[Start, Duration]]) -> extract_play_args([Start, Duration]);
extract_play_args([Start]) when is_number(Start) andalso Start > 0 -> [{start, Start}];
extract_play_args([Start]) when Start == -2 -> [{wait, infinity}];
extract_play_args([_Start]) -> [];
extract_play_args([Start, Duration]) when is_number(Start) andalso is_number(Duration) andalso Duration > 0 -> [{start, Start}, {duration, Duration}];
extract_play_args([Start, Duration]) when is_number(Start) andalso Duration == 0 -> [{start, Start}, {duration, 1}];
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
pause(State, #rtmp_funcall{args = [null, Pausing, NewTs], stream_id = StreamId}) -> 
    Socket = rtmp_session:get(State, socket),
    ?D({"PAUSE", Pausing, round(NewTs), rtmp_session:get_stream(StreamId, State)}),
    
    case rtmp_session:get_stream(StreamId, State) of
      null ->
        State;
      undefined ->
        State;
      Stream ->
        case {rtmp_stream:get(Stream, pid), Pausing} of
          {undefined, _} ->
            State;
          {Player, true} ->
            ems_media:pause(Player),
            rtmp_lib:pause_notify(Socket, StreamId),
            State;
          {Player, false} ->
            rtmp_lib:unpause_notify(Socket, StreamId, NewTs),
            ems_media:resume(Player),
            State
        end
    end.


pauseRaw(AMF, State) -> pause(AMF, State).



getStreamLength(State, #rtmp_funcall{args = [null, FullName | _]} = AMF) ->
  Host = rtmp_session:get(State, host),
  
  {RawName, Args} = http_uri2:parse_path_query(FullName),
  Name = string:join( [Part || Part <- ems:str_split(RawName, "/"), Part =/= ".."], "/"),
  Options = extract_url_args(Args),

  {Length1,Type} = case proplists:get_value(duration, Options) of 
    undefined ->
      Info = media_provider:info(Host, Name),
      L = proplists:get_value(duration, Info),
      T = proplists:get_value(type, Info),
      {L,T};
    N when is_number(N) ->
      {N, file}
  end,
  Length = case Length1 of
    undefined -> undefined;
    _ -> Length1 / 1000
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
seek(State, #rtmp_funcall{args = [_, Timestamp], stream_id = StreamId}) ->
  Socket = rtmp_session:get(State, socket),
  Stream = rtmp_session:get_stream(StreamId, State),
  Player = rtmp_stream:get(Stream, pid),
  ?D({self(), StreamId, seek, round(Timestamp), StreamId, Stream}),
  case ems_media:seek(Player, Timestamp) of
    seek_failed -> 
      rtmp_lib:seek_failed(Socket, StreamId),
      State;
    _ ->
      rtmp_session:set_stream(rtmp_stream:set(Stream, seeking, true), State)
  end.
  

%%-------------------------------------------------------------------------
%% @private
%%-------------------------------------------------------------------------
stop(State, #rtmp_funcall{stream_id = StreamId}) -> 
  Host = rtmp_session:get(State, host),
  Socket = rtmp_session:get(State, socket),
  Addr = rtmp_session:get(State, addr),
  UserId = rtmp_session:get(State, user_id),
  SessionId = rtmp_session:get(State, session_id),
  % ?D({"Stop on", self(), StreamId}),
  case rtmp_session:get_stream(StreamId, State) of
    Stream when is_tuple(Stream) ->
      Player = rtmp_stream:get(Stream, pid),
      if
        is_pid(Player) ->
          ems_media:stop(Player),
          rtmp_session:flush_stream(StreamId),
          ems_log:access(Host, "STOP ~p ~p ~p ~p", [Addr, UserId, SessionId, StreamId]),
          rtmp_socket:status(Socket, StreamId, <<"NetStream.Play.Stop">>),
          State;
        true ->
          State
      end;
    _ -> State
  end.
  
'DVRGetStreamInfo'(State, #rtmp_funcall{args = [null | RawName]} = AMF) ->
  Host = rtmp_session:get(State, host),
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

checkBandwidth(State, #rtmp_funcall{args = [null | Args]}) ->
  ?D({"checkBandwidth", Args}),
  State.
