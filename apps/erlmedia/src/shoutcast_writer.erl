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
-include("log.hrl").
-include("../include/video_frame.hrl").
-export([write/2, get_textTags/2]).
-record(shoutcast,
  {body,audio_config}).

write(Player,Req) -> 
  erlang:monitor(process,Player),
  write_frame(10449,undefined, Req).

write_frame(NextMeta,AudioConfig, Req) when NextMeta < 2 ->
  Body = <<2,"StreamTitle='Nam';00000000000000">>,
  Req:stream(Body),
  write_frame(10449,AudioConfig, Req);

write_frame(NextMeta,AudioConfig, Req) ->
  case receive_frame() of
    #video_frame{dts = 0, content = Content} = Frame when Content =/= video ->
      start_stream(Frame, Req),
      NewAudioConfig = prepare_frame(Frame,AudioConfig,Req),
      write_frame(NextMeta-size(Frame#video_frame.body),NewAudioConfig,Req);      
    #video_frame{} = Frame -> 
      NewAudioConfig = prepare_frame(Frame,AudioConfig,Req),
      case NextMeta of
        Val when Val < 1050 andalso Val >= 0 ->
          ?D(Val);
        Else -> Else
      end,
      write_frame(NextMeta-size(Frame#video_frame.body),NewAudioConfig,Req);
    {ok, _Reason} -> ok
  end.

    

get_encoding_from_bom(OrderByte) ->
  {Bom,_Number} = unicode:bom_to_encoding(OrderByte),
  Bom.

get_textTags(List,[]) ->
  List;

get_textTags(List,[{FrameID,<<OrderByte:16,Body/binary>>}|Tail]) ->
  Result = case FrameID of 
    "TALB" -> lists:merge(List,[{'Icy-Name',unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))}]);
    "TCON" -> lists:merge(List,[{'Icy-Genre',unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))}]);
    "TIT2" -> lists:merge(List,[{'Icy-Notice2',unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))}]);
    "TPE1" -> lists:merge(List,[{'Icy-Notice1',unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))}]);
    "TRCK" -> lists:merge(List,[{'Icy-Date',unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))}]);
    "TYER" -> lists:merge(List,[{'Icy-Name',unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))}]);
    _Else -> lists:merge(List,[])
  end,
  get_textTags(Result,Tail).

start_stream(Frame,Req)->
  case Frame#video_frame.content of
    metadata ->
%      MetaTags = get_textTags([],Frame#video_frame.body),
      Req:stream(head,[{'Icy-Metaint',10449}]),
      Req:stream(head,[{"Content-Type","audio/aacp"},{'Cache-Control', 'no-cache'}]);
    _Any -> 
      Req:stream(head,[{"Content-Type","audio/aacp"},{'Cache-Control', 'no-cache'}])
  end.

prepare_frame(Frame,AudioConfig,Req) ->
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
