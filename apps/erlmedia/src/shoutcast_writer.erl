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
-export([write/2]).
-record(shoutcast,
  {body,audio_config}).

write(Player,Req) -> 
  erlang:monitor(process,Player),
  handle_message(start_stream,Req),
  receive_frame(#shoutcast{audio_config = undefined},10449,Req).

%get_encoding_from_bom(OrderByte) ->
%  {Bom,_Number} = unicode:bom_to_encoding(OrderByte),
%  Bom.
%
%get_textTags(List,[]) ->
%  List;
%
%get_textTags(List,[{FrameID,<<OrderByte:16,Body/binary>>}|Tail]) ->
%  Result = case FrameID of 
%    "TALB" -> lists:merge(List,[{'Icy-Name',unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))}]);
%    "TCON" -> lists:merge(List,[{'Icy-Genre',unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))}]);
%    "TIT2" -> lists:merge(List,[{'Icy-Notice2',unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))}]);
%    "TPE1" -> lists:merge(List,[{'Icy-Notice1',unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))}]);
%    "TRCK" -> lists:merge(List,[{'Icy-Date',unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))}]);
%    "TYER" -> lists:merge(List,[{'Icy-Name',unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))}]);
%    _Else -> lists:merge(List,[])
%  end,
%  get_textTags(Result,Tail).

start_stream(Req) ->
      Req:stream(head,[{'Icy-Metaint',10449}]),
      Req:stream(head,[{"Content-Type","audio/aacp"},{'Cache-Control', 'no-cache'}]).

handle_message(start_stream,Req) ->
  start_stream(Req),
  {reply,Req};

handle_message(mp3,#shoutcast{body = Body, audio_config = AudioConfig}) ->
  {reply,#shoutcast{body = Body, audio_config = AudioConfig}};

handle_message(aac,#shoutcast{body = Body} = State) ->
  case State#shoutcast.audio_config of
    undefined -> 
      {noreply,noconfig};
    AudioConfig -> 
      Adts = aac:pack_adts(Body,AudioConfig),
      {reply, #shoutcast{body = Adts,audio_config = AudioConfig}}
  end; 

handle_message(get_config,#shoutcast{body = Body}) ->
  AudioConfig = aac:decode_config(Body),
  {reply,#shoutcast{body = Body,audio_config = AudioConfig}}.

receive_frame(#shoutcast{audio_config = AudioConfig} = State,NextMeta,Req) ->
  receive
    #video_frame{flavor = frame,codec = mp3,body = Body} -> 
      {reply, State1} = handle_message(mp3,#shoutcast{body = Body,audio_config = AudioConfig}),
      case NextMeta  of
        Value when Value == 0  ->
          Req:stream(<<1,"StreamTitle='1';">>),
          receive_frame(State1,10449,Req);
        Value when Value == 1  ->
          Req:stream(<<0,1,"StreamTitle='1';">>),
          receive_frame(State1,10449,Req);
        _Else ->
          Req:stream(Body),
          ?D(size(Body)),
          receive_frame(State1,NextMeta-size(Body),Req)
      end;
    #video_frame{flavor = frame,codec = aac,body = Body} -> 
      case handle_message(aac,#shoutcast{body = Body,audio_config = AudioConfig}) of
        {reply, State1} -> 
          Req:stream(State1#shoutcast.body),
          receive_frame(State1,NextMeta,Req);
        {noreply,noconfig} ->
          receive_frame(State,NextMeta,Req)
      end;  
    #video_frame{flavor = config, body = Body,content = audio} -> 
      {reply,State1} = handle_message(get_config,#shoutcast{body = Body}),
      receive_frame(State1,NextMeta,Req);
    #video_frame{flavor = frame,content = metadata, body = _Body} -> 
%      handle_message(new_meta,#shoutcast{body = Body}),
      receive_frame(State,NextMeta,Req);
    #video_frame{} ->
      receive_frame(State,NextMeta,Req);
    {ems_stream,_StreamId, Command} when Command == burst_start orelse Command == burst_stop->
      receive_frame(State,NextMeta,Req);
    Else -> {ok,Else}
  end.
