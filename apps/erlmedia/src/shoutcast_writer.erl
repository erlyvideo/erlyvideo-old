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
  reader,
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
  State = #shoutcast{audio_config = undefined, metaint = Metaint, reader = Player},
  case Codec of
    aac ->   
      Req:stream(head,[{"Content-Type","audio/aac"},{'Cache-Control', 'no-cache'},{'icy-metaint',Metaint}]),
      receive_frame(State,Req);
    mp3 -> 
      Req:stream(head,[{"Content-Type","audio/mpeg"},{'Cache-Control', 'no-cache'},{'icy-metaint',Metaint}]),
      receive_frame(State,Req);
    _ ->
      {error,codec_unsuported}
  end.

get_codec_info(Player) ->
  case ems_media:media_info(Player) of
    #media_info{audio = [Info]} ->
      Info#stream_info.codec;
    _Else -> {error,info_notfound}
  end.

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

handle_message(#video_frame{flavor = config, content = audio, body = Config}, #shoutcast{} = State) ->
  {noreply, State#shoutcast{audio_config = aac:decode_config(Config)}};

handle_message(#video_frame{content = Content}, State) when Content =/= audio -> 
  {noreply, State};

handle_message(#video_frame{flavor = frame, content = audio, body = Body, codec = mp3}, 
               #shoutcast{buffer = Buffer, audio_config = Config} = State) ->
  Packetized = packetize(mp3, Config, Body),
  {Reply, Rest} = split(<<Buffer/binary, Packetized/binary>>, State),
  {reply, Reply, State#shoutcast{buffer = Rest}};

handle_message(#video_frame{flavor = frame, content = audio, body = Body, codec = aac}, 
               #shoutcast{buffer = Buffer, audio_config = Config} = State) ->
  Packetized = packetize(aac, Config, Body),
  {Reply, Rest} = split(<<Buffer/binary, Packetized/binary>>, State),
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


split(Packetized, #shoutcast{} = State) -> split(Packetized, State, []).

split(Packetized, #shoutcast{metaint = Metaint}, Acc) when size(Packetized) < Metaint ->
  {lists:reverse(Acc), Packetized};

split(Packetized, #shoutcast{metaint = Metaint} = State, Acc) ->
  <<Bin:Metaint/binary, Rest/binary>> = Packetized,
  split(Rest, State, prepend_metadata(State#shoutcast.reader, [Bin|Acc])).

prepend_metadata(undefined, Acc) ->         
         [<<2,"StreamTitle='Erlyvideo'",0,0,0,0,0,0,0,0,0>>|Acc];

prepend_metadata(Player, Acc) ->
         MediaInfo = ems_media:media_info(Player),
         CurrentName = get_name_current_track(MediaInfo),
         Size = size(CurrentName),
         {MetaSize,MetaRestSize} = case Size rem 16 of
           Value when Value == 0 ->
             {Size div 16 +1, 16};
           Value ->
             {Size div 16 + 1, 16 - Value} 
         end,
         MessageSize = MetaSize + 1,
         NewMeta = padding(<<MessageSize,"StreamTitle='",CurrentName:Size/binary,"';",0>>, MetaRestSize),
         [NewMeta|Acc].

get_name_current_track(MediaInfo) ->
  case MediaInfo#media_info.metadata of
    undefined -> 
      {error,metadata_notfound};
    RawMetadata -> 
      case proplists:get_value(name,RawMetadata,undefined) of
        undefined ->
          <<"Erlyvideo">>;
        Name ->
          Name
      end
  end.


padding(Body,0) ->
  Body;

padding(<<Body/binary>>,MetaRestSize) ->
  padding(<<Body/binary,0>>,MetaRestSize - 1).
  
