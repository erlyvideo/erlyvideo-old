%%%---------------------------------------------------------------------------------------
%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        RTMPT support
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
-module(shoutcast_writer).
-author('Ilya Shcherbak <tthread@gmail.com>').
-include("src/log.hrl").
-include_lib("deps/erlmedia/include/video_frame.hrl").
-export([write/2]).
-record(shoutcast,
  {body,audio_config}).

%init(Options) ->
%  case proplists:get_value(consumer,Options,undefined) of
%    undefined -> 
%      ok;
%    Player -> {ok,Player}.

write(Player,Req) -> 
  erlang:monitor(process,Player),
  write_frame(undefined, Req).

write_frame(AudioConfig, Req) ->
  case receive_frame() of
    #video_frame{} = Frame -> 
      NewAudioConfig = prepare_frame(Frame,AudioConfig,Req),
      write_frame(NewAudioConfig,Req);
    {ok, _Reason} -> ok
  end.

prepare_frame(Frame,AudioConfig,Req)->
  State = #shoutcast{body = Frame, audio_config = AudioConfig},
  case Frame#video_frame.content of
    audio when Frame#video_frame.flavor == frame andalso
    Frame#video_frame.codec == aac ->
      write_aac_frame(State,Req);
    audio when Frame#video_frame.flavor == frame ->
      Req:stream(Frame#video_frame.body),
      State#shoutcast.audio_config;         
    audio when Frame#video_frame.flavor == config ->
      get_audio_config(State);
    _ -> AudioConfig
  end.


write_aac_frame (#shoutcast{body = Frame} = State, Req) ->
  case State#shoutcast.audio_config of
    undefined ->
      get_audio_config(State);
    AudioConfig ->
      Adts = aac:pack_adts(Frame#video_frame.body,AudioConfig),
      Req:stream(Adts),
      AudioConfig
  end.
  
get_audio_config(#shoutcast{body = Frame} = _State) when Frame#video_frame.flavor == config ->
  _AudioConfig = aac:decode_config(Frame#video_frame.body);

get_audio_config(_State) ->
  Frame = receive_frame(),
  get_audio_config(#shoutcast{body = Frame}).
  

receive_frame() ->
  receive
    Frame = #video_frame{} -> Frame;
    {ems_stream,_StreamId, Command} when Command == burst_start orelse Command == burst_stop->
      receive_frame();
    Else -> {ok,Else}
  end.
