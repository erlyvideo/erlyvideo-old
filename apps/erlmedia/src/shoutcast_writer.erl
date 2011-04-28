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
-include("../include/media_info.hrl").

-export([write/2, receive_frame/2]).

-record(shoutcast, {
  body,
  audio_config,
  metaint,
  metadata,
  timeout = 5000,
  buffer = <<>>
}).

write(Player,Req) -> 
  erlang:monitor(process,Player),
  Metaint = 16000,
  Codec = get_codec_info(Player),
  case Codec of
    aac ->   
      Req:stream(head,[{"Content-Type","audio/aac"},{'Cache-Control', 'no-cache'},{'icy-metaint',Metaint}]),
      receive_frame(#shoutcast{audio_config = undefined, metaint = Metaint},Req);
    mp3 -> 
      Req:stream(head,[{"Content-Type","audio/mpeg"},{'Cache-Control', 'no-cache'},{'icy-metaint',Metaint}]),
      receive_frame(#shoutcast{audio_config = undefined, metaint = Metaint},Req);
    _ ->
      {error,codec_unsuported}
  end.

get_codec_info(Player) ->
  case ems_media:media_info(Player) of
    #media_info{audio = [Info]} ->
      Info#stream_info.codec;
    _Else -> {error,info_notfound}
  end.


get_encoding_from_bom(OrderByte) ->
  {Bom,_Number} = unicode:bom_to_encoding(OrderByte),
  Bom.


split_metaTags([],Body) ->
  Body;

split_metaTags([Head|Tail],<<Body/binary>>) ->
  NewTag = case is_list(Head) of
   true ->  binary:list_to_bin(Head);
   false -> Head
  end,
  Size = size(NewTag),
  split_metaTags(Tail,<<Body/binary,NewTag:Size/binary,"-">>).

get_textTags(List,[]) ->
  split_metaTags(List,<<>>);

get_textTags(List,[{FrameID,<<OrderByte:16,Body/binary>>}|Tail]) ->
  Result = case FrameID of 
%    "TALB" -> lists:merge(List,[{'Icy-Name',unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))}]);
%    "TCON" -> lists:merge(List,[{'Icy-Genre',unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))}]);
     "TIT2" -> lists:merge(List,[unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))]);
%    "TPE1" -> lists:merge(List,[unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))]);
     "TRCK" -> lists:merge(List,[unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))]);
%    "TYER" -> lists:merge(List,[{'Icy-Name',unicode:characters_to_binary(Body,get_encoding_from_bom(<<OrderByte:16>>))}]);
    _Else -> lists:merge(List,[])
  end,
  get_textTags(Result,Tail).

receive_frame(#shoutcast{timeout = Timeout} = State, Req) ->
  receive
    Message ->
      case handle_message(Message, State) of
        {noreply, NewState} ->
          ?MODULE:receive_frame(NewState, Req);
        {reply, Bin, NewState} ->
          Req:stream(Bin),
          ?MODULE:receive_frame(NewState, Req);
        {stop, Reason, NewState} ->
          {stop, Reason, NewState}
      end
  after
    Timeout -> {stop, timeout, State}
  end.    

handle_message(#video_frame{flavor = frame,content = metadata, body = Metadata} = _Frame,#shoutcast{} = State) -> 
  {noreply,State#shoutcast{metadata = Metadata}};

handle_message(#video_frame{flavor = config, content = audio, body = Config}, #shoutcast{} = State) ->
  {noreply, State#shoutcast{audio_config = aac:decode_config(Config)}};

handle_message(#video_frame{content = Content}, State) when Content =/= audio -> 
  {noreply, State};

handle_message(#video_frame{flavor = frame, content = audio, body = Body, codec = mp3}, 
               #shoutcast{buffer = Buffer, audio_config = Config, metaint = Metaint} = State) ->
  Packetized = packetize(mp3, Config, Body),
  {Reply, Rest} = split(<<Buffer/binary, Packetized/binary>>, Metaint, get_textTags([<<"Erlyvideo">>],State#shoutcast.metadata)),
  {reply, Reply, State#shoutcast{buffer = Rest}};

handle_message(#video_frame{flavor = frame, content = audio, body = Body, codec = aac}, 
               #shoutcast{buffer = Buffer, audio_config = Config, metaint = Metaint} = State) ->
  Packetized = packetize(aac, Config, Body),
  {Reply, Rest} = split(<<Buffer/binary, Packetized/binary>>, Metaint, undefined),
  {reply, Reply, State#shoutcast{buffer = Rest}};

handle_message({ems_stream, _, _}, State) ->
  {noreply, State};

handle_message({ems_stream, _, _, _}, State) ->
  {noreply, State};

handle_message({tcp_closed, _Socket}, State) ->
  {stop, normal, State}.


packetize(aac, Config, Body) ->
  aac:pack_adts(Body, Config);

packetize(mp3, _Config, Body) ->
  Body.


split(Packetized, Metaint, Metadata) -> split(Packetized, Metaint, Metadata, []).

split(Packetized, Metaint, _Metadata, Acc) when size(Packetized) < Metaint ->
  {lists:reverse(Acc), Packetized};

split(Packetized, Metaint, Metadata, Acc) ->
  <<Bin:Metaint/binary, Rest/binary>> = Packetized,
  split(Rest, Metaint, undefined, prepend_metadata(Metadata, [Bin|Acc])).

prepend_metadata(undefined, Acc) ->         
         [<<2,"StreamTitle='Erlyvideo'",0,0,0,0,0,0,0,0,0>>|Acc];

prepend_metadata(Metadata, Acc) ->
         Size = size(Metadata),
         {MetaSize,MetaRestSize} = case Size rem 16 of
           Value when Value == 0 ->
             {Size/16 + 1, 16};
           Value ->
             {Size div 16 + 1, 16 - Value} 
         end,
         MessageSize = MetaSize + 1,
         NewMeta = padding(<<MessageSize,"StreamTitle='",Metadata:Size/binary,"';",0>>, MetaRestSize),
         [NewMeta|Acc].

padding(Body,0) ->
  Body;

padding(<<Body/binary>>,MetaRestSize) ->
  padding(<<Body/binary,0>>,MetaRestSize - 1).
  
      
%     #video_frame{flavor = frame,codec = mp3,body = Body} -> 
%       {reply, State1} = handle_message(mp3,#shoutcast{body = Body,audio_config = AudioConfig}),
%       case NextMeta  of
%         Value when Value == 0  ->
%           Req:stream(<<1,"StreamTitle='1';">>),
%           receive_frame(State1,10449,Req);
%         Value when Value == 1  ->
%           Req:stream(<<0,1,"StreamTitle='1';">>),
%           receive_frame(State1,10449,Req);
%         _Else ->
%           Req:stream(Body),
%           ?D(size(Body)),
%           receive_frame(State1,NextMeta-size(Body),Req)
%       end;
%     #video_frame{flavor = frame,codec = aac,body = Body} -> 
%       case handle_message(aac,#shoutcast{body = Body,audio_config = AudioConfig}) of
%         {reply, State1} -> 
%           Req:stream(State1#shoutcast.body),
%           receive_frame(State1,NextMeta,Req);
%         {noreply,noconfig} ->
%           receive_frame(State,NextMeta,Req)
%       end;  
%     #video_frame{flavor = config, body = Body,content = audio} -> 
%       {reply,State1} = handle_message(get_config,#shoutcast{body = Body}),
%       receive_frame(State1,NextMeta,Req);
%     #video_frame{flavor = frame,content = metadata, body = _Body} -> 
% %      handle_message(new_meta,#shoutcast{body = Body}),
%       receive_frame(State,NextMeta,Req);
%     #video_frame{} ->
%       receive_frame(State,NextMeta,Req);
%     {ems_stream,_StreamId, Command} when Command == burst_start orelse Command == burst_stop->
%       receive_frame(State,NextMeta,Req);
%     Else -> {ok,Else}
%   end.
